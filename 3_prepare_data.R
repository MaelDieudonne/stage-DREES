# Les valeurs -1 correspondent à la non-réponse partielle, -2 à la non-pertinence 
## (lorsqu'un enquêté n'entre pas dans le champ de la question).
# Les variables suffixées en _i sont des indicatrices d'imputation.
# Les variables suffixées en _FR rassemblent les modalités des questions posées en France, 
## lorsqu'elles sont plus nombreuses ou dans un autre ordre que les modalités transmises à Eurostat.


#######################################################################################################################
##                                                 Valeurs manquantes                                                ##
#######################################################################################################################


for (plc in c("metro", "drom")) {
  # Pour les données post-appariement
  for (yr in 2015:2019) {
    data_file <- get(glue("data_{yr}_{plc}")) %>%
      mutate(across(everything(), ~ if_else(. %in% c(-1, -2), NA, .)))
    assign(glue("data_{yr}_{plc}"), data_file)
  }
  
  # Pour les données pré-appariement
  data_file <- get(glue("ehis_{plc}")) %>%
    mutate(across(everything(), ~ if_else(. %in% c(-1, -2), NA, .)))
  assign(glue("ehis_{plc}"), data_file)
}


#######################################################################################################################
##                                                    Pathologies                                                    ##
#######################################################################################################################


for (plc in c("metro", "drom")) {
  for (yr in 2018:2019) {
    data_file <- get(glue("data_{yr}_{plc}"))
    
    # Comptabilisation du nombre de pathologies déclarées dans l'ehis
    data_file <- data_file %>% 
      mutate(
        cd1_sum = rowSums(across(starts_with("cd1") & !ends_with("_i"), ~.x == 1))) %>%
      set_variable_labels(
        cd1_sum = "Nombre de pathologies déclarées")
    
    # Création d'une indicatrice pour l'hypertension hors pathologies dans l'EHIS
    data_file <- data_file %>% mutate(
      cd1e_extot = factor(
        case_when(
          (cd1e == 1 & cd1d == 2 & cd1c == 2 & cd1j == 2 & cd1n == 2) ~ 1L,
          (cd1e == 1 & (cd1d == 1 | cd1c == 1 | cd1j == 1 | cd1n == 1)) ~ 2L,
          (cd1e == 2) ~ 2L,
          TRUE ~ NA_integer_)),
      cd1e_exdiab = factor(
        case_when(
          (cd1e == 1 & cd1j == 2) ~ 1L,
          (cd1e == 1 & cd1j == 1) ~ 2L,
          (cd1e == 2) ~ 2L,
          TRUE ~ NA_integer_)))

    # Comparaison des maladies déclarées dans l'EHIS avec les pathologies prédites à partir du SNDS
    for (i in seq_len(nrow(target))) {
      ehis_filter <- glue("
        ({glue_collapse(glue('{target$ehis_codes[[i]]} == 1'), sep = ' | ')})
        ") # maladie déclarée
      snds_filter <- glue("
        ({glue_collapse(glue('{target$snds_codes[[i]]} == 1'), sep = ' | ')})
        ") # maladie repérée
      TP_filter <- glue("
        ({glue_collapse(glue('{target$ehis_codes[[i]]} == 1'), sep = ' | ')}) &
        ({glue_collapse(glue('{target$snds_codes[[i]]} == 1'), sep = ' | ')})
        ") # maladie déclarée et repérée
      FP_filter <- glue("
        ({glue_collapse(glue('{target$ehis_codes[[i]]} == 1'), sep = ' | ')}) &
        ({glue_collapse(glue('{target$snds_codes[[i]]} == 0'), sep = ' & ')})
        ") # maladie déclarée mais pas repérée
      FN_filter <- glue("
        ({glue_collapse(glue('{target$ehis_codes[[i]]} == 2'), sep = ' & ')}) &
        ({glue_collapse(glue('{target$snds_codes[[i]]} == 1'), sep = ' | ')})
        ") # maladie pas déclarée mais repérée
      TN_filter <- glue("
        ({glue_collapse(glue('{target$ehis_codes[[i]]} == 2'), sep = ' & ')}) & 
        ({glue_collapse(glue('{target$snds_codes[[i]]} == 0'), sep = ' & ')})
        ") # maladie ni déclarée ni repérée
      
      # Indicatrices de cohérence / discordance entre les déclarations et les prédictions
      # Les variables factorielles seront utilisées par la fonction svykappa() pour calculer les coefficients kappa
      # Les variables numériques seront utilisées par la fonction svyciprop() pour calculer les autres indicateurs de performance
      data_file <- data_file %>% mutate(
        !!glue("{target$abbr[[i]]}_ehis") := as.factor(eval(parse(text = ehis_filter))),
        !!glue("{target$abbr[[i]]}_snds") := as.factor(eval(parse(text = snds_filter))),
        !!glue("{target$abbr[[i]]}_TN") := as.numeric(eval(parse(text = TN_filter))),
        !!glue("{target$abbr[[i]]}_FP") := as.numeric(eval(parse(text = FP_filter))),
        !!glue("{target$abbr[[i]]}_FN") := as.numeric(eval(parse(text = FN_filter))),
        !!glue("{target$abbr[[i]]}_TP") := as.numeric(eval(parse(text = TP_filter))))
      
      # Variable multinomiale
      # Elle sera utilisée par les régressions logistiques
      data_file <- data_file %>% mutate(
        !!glue("{target$abbr[i]}") := factor(
          case_when(
            eval(parse(text = TP_filter)) ~ 1L,
            eval(parse(text = FP_filter)) ~ 2L,
            eval(parse(text = FN_filter)) ~ 3L,
            TRUE ~ NA_integer_),
          levels = c(1,2,3),
          labels = c("EHIS et SNDS", "EHIS slt.", "SNDS slt.")))
    }
    
    # Nombre total d'incohérences de chaque type
    data_file <- data_file %>% 
      mutate(
        total_ehis_not_snds = 
          rowSums(across(matches(glue("{target$abbr}")), ~ . == 1), na.rm = TRUE),
        total_snds_not_ehis = 
          rowSums(across(matches(glue("{target$abbr}")), ~ . == 2), na.rm = TRUE),
        total_ehis_and_snds = 
          rowSums(across(matches(glue("{target$abbr}")), ~ . == 3), na.rm = TRUE)
      ) %>%
      set_variable_labels(
        total_ehis_not_snds = "Maladies EHIS+ SNDS-",
        total_snds_not_ehis = "Maladies EHIS- SNDS+",
        total_ehis_and_snds = "Maladies EHIS+ SNDS+"
      )
    
    assign(glue("data_{yr}_{plc}"), data_file)
    rm(data_file)
  }
}


#######################################################################################################################
##                                  Calcul des scores et indicateurs de dépression                                   ##
#######################################################################################################################


for (plc in c("metro", "drom")) {
  for (yr in 2018:2019) {
    data_file <- get(glue("data_{yr}_{plc}")) 
    phq_data <- data_file %>% select(starts_with("mh1") & !ends_with("_i"))
    
    # Score simple
    # On retranche 1 aux modalités, qui sont décalées vers la droite par rapport au questionnaire PHQ habituel
    phq_score <- phq_data %>%
      mutate(
        phq_score = rowSums(mutate_all(., ~ if_else(!is.na(.), .-1, 0))),
        phq_score = ifelse(rowSums(is.na(.)) > 2, NA, phq_score)) %>%
      select(phq_score)
    data_file <- cbind(data_file, phq_score)
    
    # Indicatrices
    ## de SD mineur (entre 2 et 4 symptômes plus de la moitié des jours)
    ## de SD majeur (au moins 5 symptômes plus de la moitié des jours)
    dep_dummies <- phq_data %>%
      mutate(symptoms_count = rowSums(mutate_all(., ~ if_else(.>3 & !is.na(.), 1, 0)))) %>%
      mutate(
        min_dep = ifelse(symptoms_count >= 2 & symptoms_count <= 4, 1, 0),
        min_dep = ifelse(rowSums(is.na(.)) > 2, NA, min_dep),
        min_dep = as.factor(min_dep)) %>%
      mutate(
        maj_dep = ifelse(symptoms_count >= 5, 1, 0),
        maj_dep = ifelse(rowSums(is.na(.)) > 2, NA, maj_dep),
        maj_dep = as.factor(maj_dep)) %>%        
      select(min_dep, maj_dep)
    data_file <- cbind(data_file, dep_dummies)
    
    # Algorithme
    ## Au moins un des deux premiers symptômes plus de la moitié des jours 
    ## (peu d'intérêt ou de plaisir à faire les choses / sentiment d'être triste, déprimé ou désespéré)
    ## Au moins deux autres symptômes plusieurs jours
    dep_algo <- phq_data %>%
      mutate(
        dep_algo = 
          case_when(
            (mh1a %in% 2:3 | mh1b %in% 2:3) & 
              rowSums(across(mh1a:mh1i, ~ . %in% 1:3), na.rm = TRUE)
            >= 3 ~ 1, 
            TRUE ~ 0),
        dep_algo = ifelse(rowSums(is.na(.)) > 2, NA, dep_algo),
        dep_algo = as.factor(dep_algo)) %>%
      select(dep_algo)
    data_file <- cbind(data_file, dep_algo)
    
    data_file <- data_file %>% 
      mutate(
        min_dep = factor(
          min_dep,
          levels = c(0:1),
          labels = c("Non", "Oui")),
        maj_dep = factor(
          maj_dep,
          levels = c(0:1),
          labels = c("Non", "Oui")),
        dep_algo = factor(
          dep_algo,
          levels = c(0:1),
          labels = c("Non", "Oui"))
      ) %>%
      set_variable_labels(
        phq_score = "Score PHQ",
        min_dep = "Dépression mineure",
        maj_dep = "Dépression majeure",
        dep_algo = "Algorithme pour la dépression")
    
    assign(glue("data_{yr}_{plc}"), data_file)
  }
}


#######################################################################################################################
##                                           Variables socio-démographiques                                          ##
#######################################################################################################################


recode_fct <- function(data_file) {
  data_file %>% mutate(
    
    # Âge
    age_cat = cut(
      age, 
      breaks = c(-Inf, 35, 55, 75, Inf),
      labels = c("< 35 ans", "35-55 ans", "55-75", ">75 ans"),
      right = FALSE),
    
    # PCS
    pcs = substr(cstot, 1, 1),
    
    # PCS simplifiées     
    pcs_s = factor(
      case_when(
        pcs %in% 2:3 ~ 1,
        pcs %in% 5:6 ~ 2,
        pcs %in% 3:4 ~ 3,
        pcs %in% 7:8 ~ 4),
      labels = c("Agriculteurs et artisans, commerçants et chefs d'entreprise", 
                 "Ouvriers et employés", 
                 "Professions intermédiaires et supérieures",
                 "Inactifs et retraités")),
    
    # Niveau d'études simplifié
    hatlevel_ss = factor(case_when(
      hatlevel %in% 0:3 ~ 1,
      hatlevel %in% 4:8 ~ 2,
      TRUE ~ NA_real_),
      labels = c("Primaire et secondaire", 
                 "Supérieur")),
    
    hatlevel_s = factor(case_when(
      hatlevel %in% 0:3 ~ 1,
      hatlevel %in% 4:6 ~ 2,
      hatlevel %in% 7:8 ~ 3,
      TRUE ~ NA_real_),
      labels = c("Primaire et secondaire",
                 "Supérieur court",
                 "Supérieur long")),
    
    # Situation professionnelle simplifiée
    situa_s = factor(
      case_when(
        situa %in% 1:4 ~ 1,
        situa %in% c(5,7,8,10) ~ 2,
        situa == 6 ~ 3,
        TRUE ~ NA_real_),
      labels = c("Actif ou étudiant", "Inactif", "Retraité")),
    
    # Français parlé à la maison
    lang_FR = factor(
      case_when(
        is.na(lang) ~ NA_character_,
        lang %in% c("FRANÇAIS", "FRANCAIS") ~ "Oui",
        TRUE ~ "Non"),
      levels = c("Oui", "Non")),
    
    # Descendant d'immigré
    immig_2g = factor(
      case_when(
        !is.na(naim) & !is.na(natiom) & naim == 2 & natiom != "FR" ~ "Oui",
        !is.na(naip) & !is.na(natiop) & naip == 2 & natiop != "FR" ~ "Oui",
        (is.na(naim) | is.na(natiom)) & (is.na(naip) | is.na(natiop)) ~ NA_character_,
        TRUE ~ "Non"),
      levels = c("Non", "Oui")),
    
    # Santé perçue
    hs1_s = factor(
      case_when(
        hs1 %in% 1:3 ~ 0,
        hs1 %in% 4:5 ~ 1),
      labels = c("Bon", "Mauvais")),
    
    # Difficultés de mémoire ou de concentration
    pl8_s = factor(
      case_when(
        pl8 %in% 1:2 ~ 0,
        pl8 %in% 3:4 ~ 1,
        TRUE ~ NA_real_),
      labels = c("Nulles ou faibles", "Fortes ou insurmontables")),
    
    # Score de littératie en santé
    liter_score = {
      cols <- select(., starts_with("liter") & !ends_with("_i"))
      cols <- mutate_all(cols, as.numeric)
      ifelse(rowSums(is.na(cols)) >= 3,
             NA_real_, 
             rowMeans(cols, na.rm = TRUE))},
    
    # Indicatrice de difficultés de compréhension de l'information médicale
    liter_score_s = factor(
      case_when(
        liter_score >=3.5 ~ 0,
        liter_score < 3.5 ~ 1,
        TRUE ~ NA_real_),
      labels = c("Non", "Oui")),
    
    # Score de soutien social
    # On inverse les modalités pour les questions 2 et 3, qui sont opposées par rapport au questionnaire d'Oslo habituel
    ss2 = 6 - ss2,
    ss3 = 6 - ss3,
    social_support = {
      cols <- select(., starts_with("ss") & !ends_with("_i"))
      ifelse(rowSums(is.na(cols)) >= 2,
             NA_real_, 
             rowSums(cols, na.rm = TRUE))}
  )
}


#######################################################################################################################
##                                           Types de variables et labels                                            ##
#######################################################################################################################


label_fct <- function(data_file) {
  data_file %>% 
    mutate(
      wgt = as.numeric(wgt),
      sexe = factor(
        sexe,
        levels = c(1:2),
        labels = c("Masculin", "Féminin")),
      pcs = factor(
        pcs,
        levels = c(6,1:5,7:8),
        labels = c("Ouvriers",
                   "Agriculteurs exploitants", 
                   "Artisans, commerçants, chefs d'entreprises", 
                   "Cadres et professions intellectuelles supérieures", 
                   "Professions intermédiaires", 
                   "Employés",
                   "Retraités", 
                   "Chômeurs n'ayant jamais travaillé, militaires, étudiants")),
      hatlevel = factor(
        hatlevel,
        levels = c(0:8),
        labels = c("Préprimaire", 
                   "Primaire", 
                   "Collège", 
                   "Lycée", 
                   "Post-secondaire non-supérieur", 
                   "Supérieur de cycle court", 
                   "Licence", 
                   "Master", 
                   "Doctorat")),
      situa = factor(
        situa,
        levels = c(1,3:8,10),
        labels = c("Actif occupé", 
                   "Apprenti ou stagiaire", 
                   "Etudiant", 
                   "Chômeur", 
                   "Retraité", 
                   "Au foyer", 
                   "Inactif pour cause d'invalidité", 
                   "Autre situation d'inactivité")),
      couple = factor(
        couple,
        levels = c(3,1,2),
        labels = c("Hors couple", "Couple cohabitant", "Couple non-cohabitant")),
      stoc = factor(
        stoc,
        levels = c(1:5),
        labels = c("Propriétaire primo-accédant", 
                   "Propriétaire non-accédant", 
                   "Usufruitier ou viager", 
                   "Locataire", 
                   "Logé gratuitement")),
      deg_urb = factor(
        deg_urb,
        levels = c(1:3),
        labels = c("Métropoles", "Villes et banlieues", "Zones rurales")),
      natio1n_1 = factor(
        natio1n_1,
        levels = c(1,0),
        labels = c("Oui", "Non")),
      couv1 = factor(
        couv1,
        levels = c(1:2),
        labels = c("Oui", "Non")),
      compsant = factor(
        compsant,
        levels = c(1:2),
        labels = c("Oui", "Non")),
      hs1 = factor(
        hs1,
        levels = c(1:5),
        labels = c("Très bon", "Bon", "Assez bon", "Mauvais", "Très mauvais")),
      hs2 = factor(
        hs2,
        levels = c(2,1),
        labels = c("Non", "Oui")),
      pl8 = factor(
        pl8,
        levels = c(1:4),
        labels = c("Aucune difficulté", "Quelques", "Beaucoup", "Impossibilité")),
      proxy = factor(
        proxy,
        levels = c(1:2),
        labels = c("Non", "Oui"))
    ) %>% 
    set_variable_labels(
      sexe = "Sexe",
      age_cat = "Tranche d'âge",
      pcs = "PCS",
      pcs_s = "PCS",
      hatlevel = "Niveau de scolarité",
      hatlevel_s = "Niveau de scolarité",
      hatlevel_ss = "Niveau de scolarité",
      situa = "Situation professionnelle",
      situa_s = "Situation professionnelle",
      couple = "Situation conjugale",
      stoc = "Statut vis-à-vis du logement",
      deg_urb = "Type de commune",
      natio1n_1 = "Français de naissance",
      immig_2g = "Enfant d'immigré",
      lang_FR = "Français parlé à la maison",
      couv1 = "CMU",
      compsant = "Complémentaire santé",
      hs1 = "Santé perçue",
      hs1_s = "Santé perçue",
      hs2 = "Maladie chronique déclarée",
      pl8 = "Difficultés de mémoire ou de concentration",
      pl8_s = "Difficultés de mémoire ou de concentration",
      liter_score = "Score de littératie en santé",
      liter_score_s = "Illittératie en santé",
      social_support = "Score de soutien social",
      proxy = "Réponse par un proxy")
}

label_fct_metro <- function(data_file) {
  data_file %>%
    mutate(
      hhincome = factor(
        hhincome,
        levels = c(1:5),
        labels = c("1er quintile", "2nd quintile", "3e quintile", "4e quintile", "5e quintile")),
      qpv = factor(
        qpv,
        levels = c(0:1),
        labels = c("Non", "Oui")),
      al1_fr = factor(
        al1_fr,
        levels = c(1:8),
        labels = c("Tous les jours ou presque", 
                   "5-6 jours / semaine", 
                   "3-4 jours / semaine", 
                   "1-2 jours / semaine", 
                   "2 à 3 fois par mois", 
                   "1 fois par mois", 
                   "< 1 fois par mois", 
                   "Pas au cours des 12 derniers mois")),
      intmethod = factor(
        intmethod,
        levels = c(21,31),
        labels = c("Face-à-face", "Téléphone"))
    ) %>% 
    set_variable_labels(
      hhincome = "Quintile de revenu menuel net du ménage",
      revdispm = "Revenu disponible par unité de consommation",
      qpv = "Résidence située dans un QPV",
      al1_fr = "Consommation d'alcool",
      intmethod = "Mode de collecte")
}


#######################################################################################################################
##                                               Exécution & Nettoyage                                               ##
#######################################################################################################################


for (plc in c("metro", "drom")) {
  
  # Pour les données post-appariement
  for (yr in 2018:2019) {
    file_name <- glue("data_{yr}_{plc}")
    data_file <- get(file_name) %>% 
      recode_fct() %>%
      label_fct()
    if (plc == "metro") {data_file <- label_fct_metro(data_file)}
    assign(file_name, data_file)
  }
  
  # Pour les données pré-appariement
  file_name <- glue("ehis_{plc}")
  data_file <- get(file_name) %>%
    recode_fct() %>%
    label_fct()
  if (plc == "metro") {data_file <- label_fct_metro(data_file)}
  assign(file_name, data_file)
}


rm(list = c(ls(pattern = "filter$"), ls(pattern = "^data.*?(2015|2016|2017)")))
rm(phq_data, phq_score, dep_dummies, dep_algo, plc, yr, data_file, i, recode_fct, label_fct, label_fct_metro, file_name)