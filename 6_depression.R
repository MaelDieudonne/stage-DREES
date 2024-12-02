dep_rows <- grep("depression", target$abbr)


#######################################################################################################################
##                                                       Score                                                       ##
#######################################################################################################################


df_phq_1 <- data.frame()
df_phq_2 <- data.frame()

for (plc in c("metro", "drom")) {
  data_file <- get(glue("data_2019_{plc}"))
  
  # On itère selon les définitions de la dépression dans le SNDS
  for (i in dep_rows) {
    patho <- target$abbr[[i]]
    
    # On itère selon les valeurs du phq dans l'EHIS
    for (j in 1:26) {
      
      # Indicatrices de cohérence / discordance entre le score et les repérages
      ehis_filter <- glue("phq_score >= {j}")
      TP_filter <- glue("
        (phq_score >= {j}) &
        ({glue_collapse(glue('{target$snds_codes[[i]]} == 1'), sep = ' | ')})
        ")
      FP_filter <- glue("
        (phq_score >= {j}) &
        ({glue_collapse(glue('{target$snds_codes[[i]]} == 0'), sep = ' & ')})
        ")
      FN_filter <- glue("
        (phq_score < {j}) &
        ({glue_collapse(glue('{target$snds_codes[[i]]} == 1'), sep = ' | ')})
        ")
      TN_filter <- glue("
        (phq_score < {j}) & 
        ({glue_collapse(glue('{target$snds_codes[[i]]} == 0'), sep = ' & ')})
        ")
      
      data_file <- data_file %>% mutate(
        !!glue("{patho}_{j}_ehis") := as.factor(eval(parse(text = ehis_filter))),
        !!glue("{patho}_{j}_TP") := as.numeric(eval(parse(text = TP_filter))),
        !!glue("{patho}_{j}_FP") := as.numeric(eval(parse(text = FP_filter))),
        !!glue("{patho}_{j}_TN") := as.numeric(eval(parse(text = TN_filter))),
        !!glue("{patho}_{j}_FN") := as.numeric(eval(parse(text = FN_filter))))
      
      svd <- svydesign(ids = ~1, data = data_file, weights = ~wgt)
      
      # Calcul des effectifs absolus et pondérés
      for (sfx in c("TN", "TP", "FN", "FP")) {
        counts <- sum(data_file[[glue("{patho}_{j}_{sfx}")]] == 1, na.rm = TRUE)
        prop <- svymean(as.formula(glue("~{patho}_{j}_{sfx}")), svd, na.rm = TRUE)
        
        df_phq_1 <- rbind(df_phq_1, data.frame(
          place = plc,
          year = 2019,
          patho_code = target$abbr[[i]],
          phq_level = j,
          state = sfx,
          n = counts,
          wgt_prop = 100 * mean(prop)
        ))
      }
      
      # Calcul des prévalences et des scores pondérés, avec leurs intervalles de confiance
      prev_declar <- svyciprop(as.formula(glue("~({patho}_{j}_TP + {patho}_{j}_FP)")), 
                               svd, method = "asin", level = 0.95, na.rm = TRUE)
      prev_detect <- svyciprop(as.formula(glue("~({patho}_{j}_TP + {patho}_{j}_FN)")), 
                               svd, method = "asin", level = 0.95, na.rm = TRUE)
      sensi <- svyciprop(as.formula(glue("~({patho}_{j}_TP / ({patho}_{j}_TP + {patho}_{j}_FN))")), 
                         svd, method = "asin", level = 0.95, na.rm = TRUE)
      speci <- svyciprop(as.formula(glue("~({patho}_{j}_TN / ({patho}_{j}_TN + {patho}_{j}_FP))")), 
                         svd, method = "asin", level = 0.95, na.rm = TRUE)
      ppv <- svyciprop(as.formula(glue("~({patho}_{j}_TP / ({patho}_{j}_TP + {patho}_{j}_FP))")), 
                       svd, method = "asin", level = 0.95, na.rm = TRUE)
      npv <- svyciprop(as.formula(glue("~({patho}_{j}_TN / ({patho}_{j}_TN + {patho}_{j}_FN))")), 
                       svd, method = "asin", level = 0.95, na.rm = TRUE)
      kpp <- svykappa(as.formula(glue("~{patho}_{j}_ehis + {patho}_snds")), 
                      svd, na.rm = TRUE)
      
      df_phq_2 <- rbind(df_phq_2, data.frame(
        place = plc,
        year = 2019,
        patho_code = patho,
        patho_name = target$names[[i]],
        phq_level = j,
        prev_ehis = 100 * mean(prev_declar),
        prev_ehis_margin = 100 * max(confint(prev_declar) - mean(prev_declar)),
        prev_snds = 100 * mean(prev_detect),
        prev_snds_margin = 100 * max(confint(prev_detect) - mean(prev_detect)),
        sensitivity = 100 * mean(sensi),
        sensitivity_margin = 100 * max(confint(sensi) - mean(sensi)),
        specificity = 100 * mean(speci),
        specificity_margin = 100 * max(confint(speci) - mean(speci)),
        pos_pred_val = 100 * mean(ppv),
        pos_pred_val_margin = 100 * max(confint(ppv) - mean(ppv)),
        neg_pred_val = 100 * mean(npv),
        neg_pred_val_margin = 100 * max(confint(npv) - mean(npv)),
        kappa <- mean(kpp)
      ))
      
      # Suppression des indicatrices
      data_file <- data_file %>% select(-c(!!glue("{patho}_{j}_ehis"),
                                           !!glue("{patho}_{j}_TP"), 
                                           !!glue("{patho}_{j}_FP"), 
                                           !!glue("{patho}_{j}_FN"), 
                                           !!glue("{patho}_{j}_TN")))
    }
  }
}

### Fusion des résultats
df_phq_2 <- df_phq_2 %>% rename(kappa = kappa....mean.kpp.)

df_phq_3 <- df_phq_1 %>%
  pivot_wider(names_from = state, 
              values_from = c(n, wgt_prop)) %>%
  inner_join(df_phq_2, by = c("place", "year", "patho_code", "phq_level")) %>%
  relocate(patho_name, .after = patho_code) %>%
  rename(dep_crit = phq_level)

### Fonction pour tracer les graph
plot_phq <- function(target, sbtl) {
  ggplot(data = df_phq_2, aes(x = phq_level, linetype = place, group = place)) +
    geom_line(data = subset(df_phq_2, patho_code == target), aes(y = sensitivity, color = "sensi")) +
    geom_line(data = subset(df_phq_2, patho_code == target), aes(y = specificity, color = "speci")) +
    geom_line(data = subset(df_phq_2, patho_code == target), aes(y = pos_pred_val, color = "pos")) +
    geom_line(data = subset(df_phq_2, patho_code == target), aes(y = neg_pred_val, color = "neg")) +
    geom_line(data = subset(df_phq_2, patho_code == target), aes(y = kappa, color = "kap")) +
    labs(title = "Valeurs prédites du score PHQ",
         subtitle = sbtl,
         x = "Seuil du PHQ",
         y = "",
         color = "Performances",
         linetype = "Région") +
    scale_linetype_manual(values = c("metro" = "solid", "drom" = "dashed"),
                          labels = c("DROM", "Métropole")) +
    scale_color_manual(values = c("sensi" = "skyblue",
                                  "speci" = "darkorange",
                                  "pos" = "lightgreen",
                                  "neg" = "yellow",
                                  "kap" = "purple"),
                       labels = c("sensi" = "Sensibilité",
                                  "speci" = "Spécificité",
                                  "pos" = "Valeur prédite positive",
                                  "neg" = "Valeur prédite négative",
                                  "kap" = "Coefficient kappa")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}

#######################################################################################################################
##                                            Indicatrices de dépression                                             ##
#######################################################################################################################


df_dep_dummies_1 <- data.frame()
df_dep_dummies_2 <- data.frame()

for (plc in c("metro", "drom")) {
  data_file <- get(glue("data_2019_{plc}"))
  
  # On itère selon les définitions de la dépression dans le SNDS
  for (i in dep_rows) {
    patho <- target$abbr[[i]]
    
    # On itère selon les indicatrices de dépression dans l'EHIS
    for (var in c("min_dep", "maj_dep", "dep_algo")) {
      
      # Indicatrices de cohérence / discordance entre les et les repérages
      ehis_filter <- glue("{var} == 'Oui'")
      TP_filter <- glue("
        ({var} == 'Oui') &
        ({glue_collapse(glue('{target$snds_codes[[i]]} == 1'), sep = ' | ')})
        ")
      FP_filter <- glue("
        ({var} == 'Oui') &
        ({glue_collapse(glue('{target$snds_codes[[i]]} == 0'), sep = ' & ')})
        ")
      FN_filter <- glue("
        ({var} == 'Non') &
        ({glue_collapse(glue('{target$snds_codes[[i]]} == 1'), sep = ' | ')})
        ")
      TN_filter <- glue("
        ({var} == 'Non') & 
        ({glue_collapse(glue('{target$snds_codes[[i]]} == 0'), sep = ' & ')})
        ")
      
      data_file <- data_file %>% mutate(
        !!glue("{patho}_{var}_ehis") := as.factor(eval(parse(text = ehis_filter))),
        !!glue("{patho}_{var}_TN") := as.numeric(eval(parse(text = TN_filter))),
        !!glue("{patho}_{var}_FP") := as.numeric(eval(parse(text = FP_filter))),
        !!glue("{patho}_{var}_FN") := as.numeric(eval(parse(text = FN_filter))),
        !!glue("{patho}_{var}_TP") := as.numeric(eval(parse(text = TP_filter))))
      
      svd <- svydesign(ids = ~1, data = data_file, weights = ~wgt)
      
      # Calcul des effectifs absolus et pondérés
      for (sfx in c("TN", "TP", "FN", "FP")) {
        counts <- sum(data_file[[glue("{patho}_{var}_{sfx}")]] == 1, na.rm = TRUE)
        prop <- svymean(as.formula(glue("~{patho}_{var}_{sfx}")), svd, na.rm = TRUE)
        
        df_dep_dummies_1 <- rbind(df_dep_dummies_1, data.frame(
          place = plc,
          year = 2019,
          patho_code = target$abbr[[i]],
          dep_dummy = var,
          state = sfx,
          n = counts,
          wgt_prop = 100 * mean(prop)
        ))
      }
      
      # Calcul des prévalences et des scores pondérés, avec leurs intervalles de confiance
      prev_declar <- svyciprop(as.formula(glue("~({patho}_{var}_TP + {patho}_{var}_FP)")), 
                               svd, method = "asin", level = 0.95, na.rm = TRUE)
      prev_detect <- svyciprop(as.formula(glue("~({patho}_{var}_TP + {patho}_{var}_FN)")), 
                               svd, method = "asin", level = 0.95, na.rm = TRUE)
      sensi <- svyciprop(as.formula(glue("~({patho}_{var}_TP / ({patho}_{var}_TP + {patho}_{var}_FN))")), 
                         svd, method = "asin", level = 0.95, na.rm = TRUE)
      speci <- svyciprop(as.formula(glue("~({patho}_{var}_TN / ({patho}_{var}_TN + {patho}_{var}_FP))")), 
                         svd, method = "asin", level = 0.95, na.rm = TRUE)
      ppv <- svyciprop(as.formula(glue("~({patho}_{var}_TP / ({patho}_{var}_TP + {patho}_{var}_FP))")), 
                       svd, method = "asin", level = 0.95, na.rm = TRUE)
      npv <- svyciprop(as.formula(glue("~({patho}_{var}_TN / ({patho}_{var}_TN + {patho}_{var}_FN))")), 
                       svd, method = "asin", level = 0.95, na.rm = TRUE)
      kpp <- svykappa(as.formula(glue("~{patho}_{var}_ehis + {patho}_snds")), 
                      svd, na.rm = TRUE)
      
      df_dep_dummies_2 <- rbind(df_dep_dummies_2, data.frame(
        place = plc,
        year = 2019,
        patho_code = patho,
        patho_name = target$names[[i]],
        dep_dummy = var,
        prev_ehis = 100 * mean(prev_declar),
        prev_ehis_margin = 100 * max(confint(prev_declar) - mean(prev_declar)),
        prev_snds = 100 * mean(prev_detect),
        prev_snds_margin = 100 * max(confint(prev_detect) - mean(prev_detect)),
        sensitivity = 100 * mean(sensi),
        sensitivity_margin = 100 * max(confint(sensi) - mean(sensi)),
        specificity = 100 * mean(speci),
        specificity_margin = 100 * max(confint(speci) - mean(speci)),
        pos_pred_val = 100 * mean(ppv),
        pos_pred_val_margin = 100 * max(confint(ppv) - mean(ppv)),
        neg_pred_val = 100 * mean(npv),
        neg_pred_val_margin = 100 * max(confint(npv) - mean(npv)),
        kappa <- mean(kpp)
      ))
      
      # Suppression des indicatrices
      data_file <- data_file %>% select(-c(!!glue("{patho}_{var}_ehis"),
                                           !!glue("{patho}_{var}_TP"), 
                                           !!glue("{patho}_{var}_FP"), 
                                           !!glue("{patho}_{var}_FN"), 
                                           !!glue("{patho}_{var}_TN")))
    }
  }
}

### Fusion des résultats
df_dep_dummies_2 <- df_dep_dummies_2 %>% rename(kappa = kappa....mean.kpp.)

df_dep_dummies_3 <- df_dep_dummies_1 %>%
  pivot_wider(names_from = state, 
              values_from = c(n, wgt_prop)) %>%
  inner_join(df_dep_dummies_2, by = c("place", "year", "patho_code", "dep_dummy")) %>%
  mutate(dep_dummy = case_when(
    dep_dummy == "min_dep" ~ "Dép. mineure",
    dep_dummy == "maj_dep" ~ "Dép. majeure",
    dep_dummy == "dep_algo" ~ "Algorithme")) %>%
  rename(dep_crit = dep_dummy)


#######################################################################################################################
##                                                Résultats d'ensemble                                               ##
#######################################################################################################################


### Extraction des résultats détaillés
df_dep <- rbind(
  df3 %>% 
    filter(grepl("depression", patho_code)) %>%
    select(-(ends_with("2018"))) %>%
    rename_with(~gsub("_2019", "", .x), ends_with("_2019")) %>%
    mutate(dep_crit = "Déclarations", year = 2019),
  df_phq_3 %>% 
    filter(dep_crit == 5) %>% 
    mutate(dep_crit = "PHQ >5"),
  df_phq_3 %>% 
    filter(dep_crit == 9) %>% 
    mutate(dep_crit = "PHQ >9"),
  df_phq_3 %>% 
    filter(dep_crit == 20) %>% 
    mutate(dep_crit = "PHQ >20"),
  df_dep_dummies_3) %>%
  relocate(year, .after = place) %>%
  relocate(dep_crit, .after = patho_name) %>%
  arrange(desc(place), desc(patho_code))


### Création de tableaux...
metro_rows <- 1:(nrow(df_dep)/2)
drom_rows <- (1+(nrow(df_dep)/2)):nrow(df_dep)

dep_table <- df_dep %>% gt () %>%
  fmt_number(decimals = 1, columns = !contains("margin"), drop_trailing_zeros = TRUE) %>%  
  fmt_number(decimals = 1, columns = contains("margin"), drop_trailing_zeros = FALSE) %>%
  fmt_number(decimals = 2, columns = contains("kappa"), drop_trailing_zeros = FALSE) %>%
  
  cols_merge_uncert(prev_ehis, prev_ehis_margin, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(prev_snds, prev_snds_margin, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(sensitivity, sensitivity_margin, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(specificity, specificity_margin, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(pos_pred_val, pos_pred_val_margin, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(neg_pred_val, neg_pred_val_margin, sep = " ± ", autohide = TRUE) %>%
  
  cols_hide(columns = "year" | "place" | "patho_code" | contains("wgt_")) %>%
  
  tab_spanner(label = "Définitions", columns = c("patho_name", "dep_crit"), id = "eq") %>%
  tab_spanner(label = "Effectifs", columns = c("n_TN", "n_FP", "n_FN", "n_TP"), id = "n") %>%
  tab_spanner(label = "Prévalence", columns = c("prev_ehis", "prev_snds"), id = "prev") %>%
  tab_spanner(label = "Scores", columns = c("sensitivity", "specificity"), id = "val") %>% 
  tab_spanner(label = "Valeurs prédites", columns = c("pos_pred_val", "neg_pred_val"), id = "pred") %>%
  tab_spanner(label = "Kappa", columns = "kappa", id = "kap") %>% 
  
  cols_label(
    patho_name = "Carto.",
    dep_crit = "Déclar.",
    starts_with("n_TN") ~ "Aucun",
    starts_with("n_FP") ~ "EHIS",
    starts_with("n_FN") ~ "SNDS",
    starts_with("n_TP") ~ "Tous",
    starts_with("prev_ehis") ~ "EHIS",
    starts_with("prev_snds") ~ "SNDS",
    starts_with("sensitivity") ~ "Sensibilité",
    starts_with("specificity") ~ "Spécificité",
    starts_with("pos_pred") ~ "Positives",
    starts_with("neg_pred") ~ "Négatives",
    starts_with("kappa") ~ "") %>%
  
  tab_row_group(label = "Métropole", rows = metro_rows, id = "metro") %>%
  tab_row_group(label = "DROM", rows = drom_rows, id = "drom") %>%
  row_group_order(groups = c("metro", "drom")) %>%
  
  data_color(
    method = "numeric",
    columns = contains("kappa"),
    palette = "RColorBrewer::RdYlGn",
    alpha = 0.6) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners()) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = list(
      cells_column_labels(),
      cells_body(columns = !matches("name")))) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightgrey"),
      cell_borders(sides = c("top", "bottom")),
      cell_text(style = "italic", weight = "bold")),
    location = cells_row_groups()) %>%
  tab_style(
    style = cell_borders(sides = "right", style = "solid", color = "black"),
    locations = cells_body(columns = c("dep_crit", "n_TP", "prev_snds", "specificity"))) %>%
  tab_style(
    style = cell_borders(sides = "top", style = "dashed", color = "black"),
    locations = cells_body(rows = dep_crit %in% "Déclarations"))

dep_table_counts <- dep_table %>% 
  cols_hide(contains("prev_") | contains("ity_") | contains ("pred_") | contains ("kappa_"))

dep_table_scores <- dep_table %>%
  cols_hide(starts_with("n_")) %>%
  tab_style(
    style = cell_borders(sides = "right", style = "dashed", color = "black"),
    locations = cells_body(columns = contains("prev_snds") | contains("specificity") | contains("neg_pred")))

gtsave(dep_table_counts, filename = "output/declarations/dep_counts.html")
gtsave(dep_table_scores, filename = "output/declarations/dep_scores.html")
gtsave(dep_table_scores, filename = "output/declarations/dep_scores.docx")


#######################################################################################################################
##                                                     Nettoyage                                                     ##
#######################################################################################################################

rm(list = ls(pattern = "filter$"))
rm(dep_rows, df_phq_1, plc, data_file, i, patho, j, svd, sfx, counts, prop, 
   prev_declar, prev_detect, sensi, speci, ppv, npv, kpp, kappa,
   df_dep_dummies_1, df_dep_dummies_2, var, df_phq_3, df_dep_dummies_3, 
   metro_rows, drom_rows, dep_table_counts, dep_table_scores)