#######################################################################################################################
##                                         Calcul des effectifs et des scores                                        ##
#######################################################################################################################


df1 <- data.frame()
df2 <- data.frame()

for (plc in c("metro", "drom")) {
  for (yr in 2018:2019) {
    data_file <- get(glue("data_{yr}_{plc}"))
    svd <- svydesign(ids = ~1, data = data_file, weights = ~wgt)
    
    # On itère selon les maladies
    for (i in seq_len(nrow(target))) {
      patho <- target$abbr[[i]]
      
      # Calcul des effectifs absolus et pondérés
      for (sfx in c("TN", "TP", "FN", "FP")) {
        counts <- sum(data_file[[glue("{patho}_{sfx}")]] == 1, na.rm = TRUE)
        prop <- svymean(as.formula(glue("~{patho}_{sfx}")), svd, na.rm = TRUE)
        
        df1 <- rbind(df1, data.frame(
          place = plc,
          year = yr,
          patho_code = target$abbr[[i]],
          state = sfx,
          n = counts,
          wgt_prop = 100 * mean(prop)))
      }

      # Calcul des prévalences et des scores pondérés, avec leurs intervalles de confiance
      prev_declar <- svyciprop(as.formula(glue("~{patho}_TP + {patho}_FP")), 
                               svd, method = "asin", level = 0.95, na.rm = TRUE)
      prev_detect <- svyciprop(as.formula(glue("~{patho}_TP + {patho}_FN")), 
                               svd, method = "asin", level = 0.95, na.rm = TRUE)
      sensi <- svyciprop(as.formula(glue("~{patho}_TP / ({patho}_TP + {patho}_FN)")), 
                         svd, method = "asin", level = 0.95, na.rm = TRUE)
      speci <- svyciprop(as.formula(glue("~{patho}_TN / ({patho}_TN + {patho}_FP)")), 
                         svd, method = "asin", level = 0.95, na.rm = TRUE)
      ppv <- svyciprop(as.formula(glue("~{patho}_TP / ({patho}_TP + {patho}_FP)")), 
                       svd, method = "asin", level = 0.95, na.rm = TRUE)
      npv <- svyciprop(as.formula(glue("~{patho}_TN / ({patho}_TN + {patho}_FN)")), 
                       svd, method = "asin", level = 0.95, na.rm = TRUE)
      kpp <- svykappa(as.formula(glue("~{patho}_ehis + {patho}_snds")), 
                      svd, na.rm = TRUE)
      
      df2 <- rbind(df2, data.frame(
        place = plc,
        year = yr,
        patho_code = patho,
        patho_name = target$names[[i]],
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
    }
  }
}

df2 <- df2 %>% rename(kappa = kappa....mean.kpp.)

df3 <- df1 %>%
  pivot_wider(names_from = state, 
              values_from = c(n, wgt_prop)) %>%
  inner_join(df2, by = c("place", "year", "patho_code")) %>%
  pivot_wider(names_from = year, 
              values_from = setdiff(names(.), c("patho_code", "patho_name", "year", "place")))


#######################################################################################################################
##                                                 Tableaux complets                                                 ##
#######################################################################################################################


metro_rows <- 1:(nrow(df3)/2)
drom_rows <- (1+(nrow(df3)/2)):nrow(df3)

table <- df3 %>% gt () %>%
  fmt_number(decimals = 1, columns = !contains("margin"), drop_trailing_zeros = TRUE) %>%  
  fmt_number(decimals = 1, columns = contains("margin"), drop_trailing_zeros = FALSE) %>%
  fmt_number(decimals = 2, columns = contains("kappa"), drop_trailing_zeros = FALSE) %>%
  
  cols_merge_uncert(prev_ehis_2018, prev_ehis_margin_2018, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(prev_snds_2018, prev_snds_margin_2018, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(sensitivity_2018, sensitivity_margin_2018, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(specificity_2018, specificity_margin_2018, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(pos_pred_val_2018, pos_pred_val_margin_2018, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(neg_pred_val_2018, neg_pred_val_margin_2018, sep = " ± ", autohide = TRUE) %>%
  
  cols_merge_uncert(prev_ehis_2019, prev_ehis_margin_2019, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(prev_snds_2019, prev_snds_margin_2019, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(sensitivity_2019, sensitivity_margin_2019, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(specificity_2019, specificity_margin_2019, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(pos_pred_val_2019, pos_pred_val_margin_2019, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(neg_pred_val_2019, neg_pred_val_margin_2019, sep = " ± ", autohide = TRUE) %>%
  
  cols_hide(columns = "place" | "patho_code" | contains("wgt_")) %>%
  cols_move_to_end(ends_with("2019")) %>%
  
  tab_spanner(label = "Effectifs", columns = c("n_TN_2018", "n_FP_2018", "n_FN_2018", "n_TP_2018"), id = "n_2018") %>%
  tab_spanner(label = "Prévalence", columns = c("prev_ehis_2018", "prev_snds_2018"), id = "prev_2018") %>%
  tab_spanner(label = "Scores", columns = c("sensitivity_2018", "specificity_2018"), id = "val_2018") %>% 
  tab_spanner(label = "Valeurs prédites", columns = c("pos_pred_val_2018", "neg_pred_val_2018"), id = "pred_2018") %>%
  tab_spanner(label = "Kappa", columns = "kappa_2018", id = "kap_2018") %>%
  tab_spanner(label = "2018", columns = ends_with("2018"), id = "2018") %>% 
  
  tab_spanner(label = "Effectifs", columns = c("n_TN_2019", "n_FP_2019", "n_FN_2019", "n_TP_2019"), id = "n_2019") %>%
  tab_spanner(label = "Prévalence", columns = c("prev_ehis_2019", "prev_snds_2019"), id = "prev_2019") %>%
  tab_spanner(label = "Scores", columns = c("sensitivity_2019", "specificity_2019"), id = "val_2019") %>%
  tab_spanner(label = "Valeurs prédites", columns = c("pos_pred_val_2019", "neg_pred_val_2019"), id = "pred_2019") %>%
  tab_spanner(label = "Kappa", columns = "kappa_2019", id = "kap_2019") %>%
  tab_spanner(label = "2019", columns = ends_with("2019"), id = "2019") %>%  
  
  cols_label(
    patho_name = "",
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
      cell_text(style = "italic", weight = "bold")),
    location = cells_row_groups()) %>%
  tab_style(
    style = cell_borders(sides = "top", style = "dashed", color = "black"),
    locations = cells_body(rows = patho_code %in% c("coropath", "hbp_tt", "stroke", "diab", "depression_tt")))

table_counts <- table %>% 
  cols_hide(contains("prev_") | contains("ity_") | contains ("pred_") | contains ("kappa_"))

table_scores <- table %>%
  cols_hide(starts_with("n_")) %>%
  tab_style(
    style = cell_borders(sides = "right"),
    locations = cells_body(columns = "patho_name" | "kappa_2018")) %>%
  tab_style(
    style = cell_borders(sides = "right", style = "dashed", color = "black"),
    locations = cells_body(columns = contains("prev_snds") | contains("specificity") | contains("neg_pred")))

gtsave(table_counts, filename = "output/declarations/effectifs.html")
gtsave(table_counts, filename = "output/declarations/effectifs.docx")
gtsave(table_scores, filename = "output/declarations/scores.html")
gtsave(table_scores, filename = "output/declarations/scores.docx")


#######################################################################################################################
##                                                  Tableau allégé                                                   ##
#######################################################################################################################


df4 <- df3 %>% 
  filter(patho_code %in% c("CRD", "coropath", "hbp_tt", "stroke", "diab", "depression_all")) %>%
  select(-(ends_with("2018")))

metro_rows <- 1:(nrow(df4)/2)
drom_rows <- (1+(nrow(df4)/2)):nrow(df4)

table_scores_light <- df4 %>% gt () %>%
  fmt_number(decimals = 1, columns = !contains("margin"), drop_trailing_zeros = TRUE) %>%  
  fmt_number(decimals = 1, columns = contains("margin"), drop_trailing_zeros = FALSE) %>%
  fmt_number(decimals = 2, columns = contains("kappa"), drop_trailing_zeros = FALSE) %>%
  
  cols_merge_uncert(prev_ehis_2019, prev_ehis_margin_2019, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(prev_snds_2019, prev_snds_margin_2019, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(sensitivity_2019, sensitivity_margin_2019, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(specificity_2019, specificity_margin_2019, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(pos_pred_val_2019, pos_pred_val_margin_2019, sep = " ± ", autohide = TRUE) %>%
  cols_merge_uncert(neg_pred_val_2019, neg_pred_val_margin_2019, sep = " ± ", autohide = TRUE) %>%
  
  cols_hide(columns = "place" | "patho_code" | starts_with("n_") | contains("wgt_")) %>%
  
  tab_spanner(label = "Effectifs", columns = c("n_TN_2019", "n_FP_2019", "n_FN_2019", "n_TP_2019"), id = "n_2019") %>%
  tab_spanner(label = "Prévalence", columns = c("prev_ehis_2019", "prev_snds_2019"), id = "prev_2019") %>%
  tab_spanner(label = "Scores", columns = c("sensitivity_2019", "specificity_2019"), id = "val_2019") %>%
  tab_spanner(label = "Valeurs prédites", columns = c("pos_pred_val_2019", "neg_pred_val_2019"), id = "pred_2019") %>%
  
  cols_label(
    patho_name = "",
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
      cell_text(style = "italic", weight = "bold")),
    location = cells_row_groups()) %>%
  tab_style(
    style = cell_borders(sides = "right", style = "dashed", color = "black"),
    locations = cells_body(columns = contains("prev_snds") | contains("specificity")))

gtsave(table_scores_light, filename = "output/declarations/scores_light.html")
gtsave(table_scores_light, filename = "output/declarations/scores_light.docx")


#######################################################################################################################
##                                                     Nettoyage                                                     ##
#######################################################################################################################


rm(plc, yr, data_file, svd, i, patho, sfx, counts, prop, prev_declar, prev_detect, sensi, speci, ppv, npv, kpp, kappa,
   df1, df2, df4, metro_rows, drom_rows, table, table_counts, table_scores, table_scores_light)