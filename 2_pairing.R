#######################################################################################################################
##                                                     Métropole                                                     ##
#######################################################################################################################


ehis_metro <- ehis_metro %>%
  mutate(id_insee = as.integer(id_insee))

passage_metro <- passage_metro %>%
  mutate(XML_ID = str_remove(XML_ID, "ID_")) %>%
  mutate(XML_ID = as.integer(XML_ID)) %>%
  mutate(noind = as.integer(noind))

sample_metro <- data.frame()

for (yr in 2015:2019) {
  data_file <- 
    get(glue("carto_{yr}_fideli")) %>%
    select(NUM_ENQ, starts_with("TOP"), starts_with("SUP")) %>%
    distinct(NUM_ENQ, .keep_all = TRUE) %>%
    filter(str_detect(NUM_ENQ, "^ehis1")) %>%
    mutate(NUM_ENQ = str_remove(NUM_ENQ, "ehis1_"),
           NUM_ENQ = as.integer(NUM_ENQ)) %>%
    inner_join(passage_metro, join_by("NUM_ENQ" == "XML_ID")) %>%
    inner_join(ehis_metro, join_by("noind" == "id_insee")) %>%
    mutate(refdate = ymd(refdate))
  
  assign(glue("data_{yr}_metro"), data_file)
  
  sample_metro <- sample_metro %>%
    rbind(c(yr, nrow(data_file)))
  
  rm(data_file)
}

# Pour choisir la cartographie la plus proche de la date d'interview
# On la nomme 2020 pour faciliter son exploitation par la suite
# data_2020_metro <- rbind(
#  data_2018_metro %>% filter(refdate <= ymd("2019-06-30")),
#  data_2019_metro %>% filter(refdate > ymd("2019-06-30")))

# Pour créer des tables avec les individus non-appariés
# missing_2018_metro <- anti_join(ehis_metro, data_2018_metro, join_by("id_insee" == "noind"))
# missing_2019_metro <- anti_join(ehis_metro, data_2019_metro, join_by("id_insee" == "noind"))


#######################################################################################################################
##                                                        DROM                                                       ##
#######################################################################################################################


passage_drom_nir <- passage_drom_nir %>%
  mutate(xml_id = str_remove(xml_id, "ID_")) %>%
  mutate(xml_id = as.integer(xml_id))

passage_drom_ec <- passage_drom_ec %>%
  mutate(xml_id = str_remove(xml_id, "ID_")) %>%
  mutate(xml_id = as.integer(xml_id))

passage_drom_fideli <- passage_drom_fideli %>%
  mutate(ident_men = str_c(RGES, NUMFA, SSECH)) %>%
  mutate(XML_ID = str_remove(XML_ID, "ID_")) %>%
  mutate(XML_ID = as.integer(XML_ID)) %>%
  select(XML_ID, ident_men)

sample_drom <- data.frame()

for (yr in 2015:2019) {
  # NIR déclaré
  nir_data <- 
    get(glue("carto_{yr}_drom_nir")) %>%
    select(NUM_ENQ, starts_with("TOP"), starts_with("SUP")) %>%
    distinct(NUM_ENQ, .keep_all = TRUE) %>%
    mutate(NUM_ENQ = sub("_ET_.*", "", NUM_ENQ),
           NUM_ENQ = as.integer(NUM_ENQ)) %>%
    inner_join(passage_drom_nir, join_by("NUM_ENQ" == "xml_id")) %>%
    inner_join(ehis_drom, join_by("IDENT_IND" == "ident_ind"))
  
  # Etat-civil déclaré
  ec_data <- 
    get(glue("carto_{yr}_drom_ec")) %>%
    select(NUM_ENQ, starts_with("TOP"), starts_with("SUP")) %>%
    distinct(NUM_ENQ, .keep_all = TRUE) %>%
    mutate(NUM_ENQ = sub("_ET_.*", "", NUM_ENQ),
           NUM_ENQ = as.integer(NUM_ENQ)) %>%
    inner_join(passage_drom_ec, join_by("NUM_ENQ" == "xml_id")) %>%
    inner_join(ehis_drom, join_by("IDENT_IND" == "ident_ind"))

  # NIR reconstitué
  fideli_data <- 
    get(glue("carto_{yr}_fideli")) %>%
    select(NUM_ENQ, BEN_SEX_COD, BEN_NAI_ANN, starts_with("TOP"), starts_with("SUP")) %>%
    distinct(NUM_ENQ, .keep_all = TRUE) %>%
    filter(str_detect(NUM_ENQ, "^ehis2")) %>%
    mutate(NUM_ENQ = str_remove(NUM_ENQ, "ehis2_")) %>%
    mutate(NUM_ENQ = as.integer(NUM_ENQ)) %>%
    inner_join(passage_drom_fideli, join_by("NUM_ENQ" == "XML_ID"))
  
  ### On isole les individus non-appariés par le NIR ou l'état-civil déclarés
  anti_nir_ec <- ehis_drom %>%
    anti_join(
      rbind(nir_data, ec_data), 
      join_by("ident_ind" == "IDENT_IND")) %>%
    mutate(ident_men = str_sub(ident_men, 1, -4)) %>%
    mutate(sexe = as.integer(sexe)) %>%
    mutate(yearbirth = as.integer(yearbirth))

  ### On apparie par l'identifiant du ménage lorsqu'un seul individu est présent
  fideli_data_1 <- fideli_data %>%
    group_by(ident_men) %>% filter(n() == 1) %>% ungroup() %>%
    inner_join(anti_nir_ec, join_by("ident_men")) %>%
    distinct(ident_ind, .keep_all = TRUE) %>%
    rename(IDENT_IND = ident_ind) %>%
    select(-BEN_SEX_COD, -BEN_NAI_ANN)
  
  ### On apparie par l'identifiant du ménage, le sexe et l'année de naissance lorsque plusieurs individus sont présents
  fideli_data_2 <- fideli_data %>%
    group_by(ident_men) %>% filter(n() > 1) %>% ungroup() %>%
    inner_join(anti_nir_ec, join_by("ident_men", "BEN_SEX_COD" == "sexe", "BEN_NAI_ANN" == "yearbirth")) %>%
    distinct(ident_ind, .keep_all = TRUE) %>%
    rename(IDENT_IND = ident_ind, sexe = BEN_SEX_COD, yearbirth = BEN_NAI_ANN)

  # On fusionne les bases obtenues
  data_file <- rbind(nir_data, ec_data, fideli_data_1, fideli_data_2) %>%
    mutate(refdate = ymd(refdate))
  assign(glue("data_{yr}_drom"), data_file)
  
  # On enregistre le nombre d'individus appariés de chaque manière
  sample_drom <- sample_drom %>%
    rbind(c(yr, nrow(nir_data), nrow(ec_data), nrow(fideli_data_1) + nrow(fideli_data_2), nrow(data_file)))
  
  # Nettoyage
  rm(nir_data, ec_data, fideli_data, anti_nir_ec, fideli_data_1, fideli_data_2, data_file)
}



# Pour choisir la cartographie la plus proche selon la date d'interview
# data_2020_drom <- rbind(
#  data_2018_drom %>% filter(refdate <= ymd("2019-06-30")),
#  data_2019_drom %>% filter(refdate > ymd("2019-06-30")))

# Pour créer des tables avec les individus non-appariés
# missing_2018_drom <- anti_join(ehis_drom, data_2018_drom, join_by("ident_ind" == "IDENT_IND"))
# missing_2019_drom <- anti_join(ehis_drom, data_2019_drom, join_by("ident_ind" == "IDENT_IND"))


#######################################################################################################################
##                                            Taux et modes d'appariement                                            ##
#######################################################################################################################


sample_metro <- sample_metro %>%
  rename(year = 1, fideli = 2) %>%
  mutate(appar_rate = 100 * fideli / nrow(ehis_metro))

sample_drom <- sample_drom %>%
  rename(year = 1, nir = 2, ec = 3, fideli = 4, tot = 5) %>%
  mutate(
    nir_share = 100 * nir / tot,
    ec_share = 100 * ec / tot,
    fideli_share = 100 * fideli / tot,
    appar_rate = 100 * tot / nrow(ehis_drom))

# Création d'indicatrices d'appariement pour chaque année
ehis_metro <- ehis_metro %>% mutate(
  appar_2015 = as.factor(id_insee %in% data_2018_metro$noind),
  appar_2016 = as.factor(id_insee %in% data_2018_metro$noind),
  appar_2017 = as.factor(id_insee %in% data_2018_metro$noind),
  appar_2018 = as.factor(id_insee %in% data_2018_metro$noind),
  appar_2019 = as.factor(id_insee %in% data_2019_metro$noind))

ehis_drom <- ehis_drom %>% mutate(
  appar_2015 = as.factor(ident_ind %in% data_2018_drom$IDENT_IND),
  appar_2016 = as.factor(ident_ind %in% data_2018_drom$IDENT_IND),
  appar_2017 = as.factor(ident_ind %in% data_2018_drom$IDENT_IND),
  appar_2018 = as.factor(ident_ind %in% data_2018_drom$IDENT_IND),
  appar_2019 = as.factor(ident_ind %in% data_2019_drom$IDENT_IND))


#######################################################################################################################
##                                                     Nettoyage                                                     ##
#######################################################################################################################

rm(yr, list = c(ls(pattern = "^passage"), ls(pattern = "^carto")))