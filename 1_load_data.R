#######################################################################################################################
##                                               Lecture des fichiers                                               ##
#######################################################################################################################


root_path <- "U:/DONNEES-SOURCES-DEDIEES/D-EHIS-SNDS/EHIS-SNDS_2019"

# EHIS
ehis_metro <- readRDS(glue("{root_path}/Reponses/version 20230905/ehis2019_metropole_index.rds"))
ehis_drom <- readRDS(glue("{root_path}/Reponses/version 20230905/ehis2019_dom_index.rds"))

# SNDS
for (yr in 2015:2019) {
  data_file <- read.csv(glue("{root_path}/SNDS/FIDELI/EHIS_CSV/crto_ct_ind_g8_{yr}.csv"), header = TRUE, sep = ";")
  assign(glue("carto_{yr}_fideli"), data_file)
  data_file <- read.csv(glue("{root_path}/SNDS/DOM_APPARIEMENT_COLLECTE/NIR_CSV/crto_ct_ind_g8_{yr}.csv"), header = TRUE, sep = ";")
  assign(glue("carto_{yr}_drom_nir"), data_file)
  data_file <- read.csv(glue("{root_path}/SNDS/DOM_APPARIEMENT_COLLECTE/ECIV_CSV/crto_ct_ind_g8_{yr}.csv"), header = TRUE, sep = ";")
  assign(glue("carto_{yr}_drom_ec"), data_file)
}

# Tables d'appariement
passage_metro <- read_sas(glue("{root_path}/tables_de_passage/table_passage_metropole_ehis.sas7bdat"), NULL)
passage_drom_nir <- read_sas(glue("{root_path}/tables_de_passage/appar_nir.sas7bdat"), NULL)
passage_drom_ec <- read_sas(glue("{root_path}/tables_de_passage/appar_ec.sas7bdat"), NULL)
passage_drom_fideli <- read_sas(glue("{root_path}/tables_de_passage/tdp_echantillon_identbrpp.sas7bdat"), NULL)

rm(root_path, yr)


#######################################################################################################################
##                                     Dataframe avec les équivalences EHIS-SNDS                                     ##
#######################################################################################################################


target <- data.frame(
  names = I(list()),
  abbr = character(),
  ehis_codes = I(list()),
  snds_codes = I(list())
)

target <- target %>%
  
  # MRC
  rbind(data.frame(
    names = "MRC (avec asthme et muco)",
    abbr = "CRD",
    ehis_codes = I(list(c("cd1a", "cd1b"))),
    snds_codes = I(list(c("TOP_ABPCOIR_IND", "TOP_IRMUCO_IND")))
  )) %>%
  
  # Coronaropathies
  rbind(data.frame(
    names = "Coronaropathies (dont infarctus)",
    abbr = "coropath",
    ehis_codes = I(list(c("cd1c", "cd1d"))),
    snds_codes = I(list(c("TOP_CVIDM_AIG", "TOP_CVCORON_CHR")))
  )) %>% 
  
  # Hypertension
  rbind(data.frame(
    names = "Hypertension (traitement seul)",
    abbr = "hbp_tt",
    ehis_codes = I(list("cd1e_extot")),
    snds_codes = I(list("TOP_FANTIHTA_MED"))
  )) %>%
  rbind(data.frame(
    names = "Hypertension (+ insuf card)",
    abbr = "hbp_tt_hf",
    ehis_codes = I(list("cd1e")),
    snds_codes = I(list(c("TOP_FANTIHTA_MED", "TOP_CVIC_AIG", "TOP_CVIC_CHR")))
  )) %>%
  rbind(data.frame(
    names = "Hypertension (+ insuf card, insuf rén & diab)",
    abbr = "hbp_heart_kidney_diab",
    ehis_codes = I(list("cd1e")),
    snds_codes = I(list(c("TOP_FANTIHTA_MED", "TOP_CVIC_AIG", "TOP_CVIC_CHR", "SUP_RIRCT_CAT", "TOP_FDIABET_IND")))
  )) %>%
  rbind(data.frame(
    names = "Hypertension (+ patho card, insuf rén & diab)",
    abbr = "hbp_all",
    ehis_codes = I(list("cd1e")),
    snds_codes = I(list(c("TOP_FANTIHTA_MED", "SUP_CV_CAT", "SUP_RIRCT_CAT", "TOP_FDIABET_IND")))
  )) %>%
  
  # AVC
  rbind(data.frame(
    names = "AVC",
    abbr = "stroke",
    ehis_codes = I(list("cd1f")),
    snds_codes = I(list(c("TOP_CVAVC_AIG", "TOP_CVAVC_SEQ")))
  )) %>%
  
  # Diabète
  rbind(data.frame(
    names = "Diabète",
    abbr = "diab",
    ehis_codes = I(list("cd1j")),
    snds_codes = I(list("TOP_FDIABET_IND"))
  )) %>%
  
  # Dépression
  rbind(data.frame(
    names = "Dépression (traitement seul)",
    abbr = "depression_tt",
    ehis_codes = I(list("cd1o")),
    snds_codes = I(list("TOP_PANTIDE_MED"))
  )) %>%
  rbind(data.frame(
    names = "Dépression (traitement & dép clin)",
    abbr = "depression_tt_clin",
    ehis_codes = I(list("cd1o")),
    snds_codes = I(list(c("TOP_PANTIDE_MED", "SUP_PTRDHUM_IND")))
  )) %>%
  rbind(data.frame(
    names = "Dépression (dép clin)",
    abbr = "depression_clin",
    ehis_codes = I(list("cd1o")),
    snds_codes = I(list(c("SUP_PTRDHUM_IND")))
  )) %>%
  rbind(data.frame(
    names = "Dépression (traitement & troubles névro / hum)",
    abbr = "depression_all",
    ehis_codes = I(list("cd1o")),
    snds_codes = I(list(c("TOP_PANTIDE_MED", "TOP_PDEPNEV_IND")))
  ))