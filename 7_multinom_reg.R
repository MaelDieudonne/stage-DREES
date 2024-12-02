#######################################################################################################################
###                                             Ajustement des modèles                                              ###
#######################################################################################################################


results_metro <- list()
results_drom <- list()

plots_metro <- list()
plots_drom <- list()

# On utilise gtsummary et ggstats pour présenter les résultats.
# Il faut les appeler dans la boucle afin d'accéder aux variables d'environnement.
for (plc in c("metro", "drom")) {
  data_file <- get(glue("data_2019_{plc}"))
  results <- get(glue("results_{plc}"))
  plots <- get(glue("plots_{plc}"))
  regressors <- get(glue("regressors_{plc}"))
  
  single_rows <- c("sexe", "natio1n_1", "immig_2g", "lang_FR", "pl8_s", "liter_score_s", "hs1_s", "proxy")
    if (plc == "metro") {single_rows <- c(single_rows, "qpv")}
    else {single_rows <- c(single_rows, "hatlevel_ss")}
  
  for (i in seq_len(nrow(sub_target))) {
    regression_data <- data_file %>%
      select(!!sym(glue("{sub_target$abbr[i]}")), all_of(regressors)) %>%
      drop_na()
    regression_formula <- as.formula(
      glue("{sub_target$abbr[i]} ~ {paste(regressors, collapse = ' + ')}"))
    regression_results <- multinom(
      regression_formula, regression_data, refLevel = "EHIS et SNDS", weights = regression_data$wgt)
    
    results[[sub_target$abbr[i]]] <- tbl_regression(
      regression_results,
      show_single_row = single_rows,
      exponentiate = TRUE)
    # %>% add_glance_table(include = c("nobs", "deviance")) # Pour ajouter des statistiques descriptives

    plots[[sub_target$abbr[i]]] <- ggcoef_multinom(
      regression_results,
      exponentiate = TRUE,
      add_reference_row = TRUE,
      # no_reference_row = broom.helpers::all_dichotomous(), # Supprime la référence pour les variables binaires
      stripped_rows = FALSE,
      type = "faceted") + 
      ggtitle(glue("{sub_target$names[i]}"),
              subtitle = ifelse(plc == "drom", glue("DROM"), glue("Métropole"))) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
    
    assign(glue("results_{plc}"), results)
    assign(glue("plots_{plc}"), plots)
  }
}

### Nettoyage
rm(plc, data_file, results, plots, regressors, regressors_drom, regressors_metro, i, regression_data, regression_formula, regression_results, single_rows)


#######################################################################################################################
###                                   Résultats séparés (pour chaque pathologie)                                    ###
#######################################################################################################################


### Tableaux avec gtsummary

#### Fonction pour présenter les résultats au format wide
#### Récupérée depuis https://larmarange.github.io/guide-R/analyses_avancees/regression-logistique-multinomiale.html
multinom_pivot_wider <- function(x) {
  if (!inherits(x, "tbl_regression") || !inherits(x$model_obj, "multinom")) {
    stop("`x=` must be class 'tbl_regression' summary of a `nnet::multinom()` model.")
  }
  df <- tibble::tibble(outcome_level = unique(x$table_body$groupname_col))
  df$tbl <- 
    purrr::map(
      df$outcome_level,
      function(lvl) {
        gtsummary::modify_table_body(
          x, 
          ~dplyr::filter(.x, .data$groupname_col %in% lvl) %>%
            dplyr::ungroup() %>%
            dplyr::select(-.data$groupname_col)
        )})
  tbl_merge(df$tbl, tab_spanner = paste0("**", df$outcome_level, "**"))
}

#### Fonction pour afficher seulement les variables et modalités significatives pour au moins un outcome
#### Avec la modalité de référence pour les variables non-binaires
multinom_filter_p <- function(x) {
  x %>% modify_table_body(
    ~ .x %>% 
      group_by(variable) %>%
      filter(
        if_any(starts_with("p.value"), ~ . < 0.05, na.rm = TRUE) | 
          (n()>2 & any(if_any(starts_with("p.value"), ~ . < 0.05, na.rm = TRUE)) & row_number() %in% c(1,2))
      ) %>% ungroup()) 
}

#### Création des tableaux
multinom_sep_tables <- list()

for (plc in c("metro", "drom")) {
  for (i in seq_len(nrow(sub_target))) {
    data <- eval(parse(text = paste0("results_", plc, "$", sub_target$abbr[i])))
    data <- data %>%
      add_significance_stars(hide_ci = FALSE, hide_p = TRUE, hide_se = TRUE) %>%
      multinom_pivot_wider() %>%
      multinom_filter_p() %>% 
      bold_labels() %>%
      italicize_levels() %>%
      modify_header(label = "") %>%
      as_gt() %>%
      fmt_number(suffixing = TRUE) %>%
      tab_header(
        title = glue("{sub_target$names[i]}"), 
        subtitle = ifelse(plc == "drom", glue("DROM"), glue("Métropole"))) %>%
      tab_style(
        style = cell_fill(color = "lightgrey"),
        locations = cells_body(rows = (row_type != "level"))
      )
    
    multinom_sep_tables[[glue("{sub_target$abbr[i]}_{plc}")]] <- data
    gtsave(data, filename = glue("output/multinom_tables/{sub_target$abbr[i]}_{plc}.html"))
    gtsave(data, filename = glue("output/multinom_tables/{sub_target$abbr[i]}_{plc}.docx"))
    print(data)
  }
}

#### Nettoyage 
rm(multinom_pivot_wider, plc, i, data)

### Graphiques avec ggstats
multinom_graphs <- list()

for (plc in c("metro", "drom")) {
  for (i in seq_len(nrow(sub_target))) {
    plot <- eval(parse(text = paste0("plots_", plc, "$", sub_target$abbr[i])))
    
    # Sélection des modalités significatives
    plot$data <- plot$data %>%
      group_by(variable, y.level) %>%
      filter(p.value <= 0.05) %>%
      ungroup()
    
    # Troncage des labels pour éviter de déformer horizontalement les graphiques
    plot$data <- plot$data %>% 
      mutate(
        var_label = str_trunc(as.character(var_label), 35, "right", ellipsis = "..."),
        label_light = str_trunc(as.character(label_light), 35, "right", ellipsis = "..."))
    
    # Suppression de la coloration alternée entre les lignes
    # plot$layers <- plot$layers[-1]
    
    # Suppression de la légende concernant la significativité
    plot$layers[[3]]$show.legend <- FALSE
    
    # Sauvegarde et affichage
    multinom_graphs[[glue("{sub_target$abbr[i]}_{plc}")]] <- data
    
    path <- glue("output/multinom_graphs/{plc}_{sub_target$abbr[i]}.png")
    ggsave(
      path,
      plot,
      height = max(5, 0.2*nrow(plot$data)),
      units = "in",
      dpi = 300)
  }
}

#### Nettoyage
rm(plc, i, path, plot, plots_drom, plots_metro)


#######################################################################################################################
###                                    Résultats comparés (pour chaque outcome)                                     ###
#######################################################################################################################


### Fonction pour séparer les résultats pour chaque outcome
multinom_split_results <- function(x) {
  df <- tibble::tibble(outcome_level = unique(x$table_body$groupname_col))
  df$tbl <- 
    purrr::map(
      df$outcome_level,
      function(lvl) {
        gtsummary::modify_table_body(
          x, 
          ~dplyr::filter(.x, .data$groupname_col %in% lvl) %>%
            dplyr::ungroup() %>%
            dplyr::select(-.data$groupname_col)
        )})}

### Création des tableaux
multinom_agg_tables <- list()

for (plc in c("metro", "drom")) {
  results <- get(glue("results_{plc}"))
  
  for (i in seq_len(nrow(sub_target))) {
    results[[sub_target$abbr[i]]] <- results[[sub_target$abbr[i]]] %>%
      add_significance_stars(hide_ci = TRUE, hide_p = TRUE, hide_se = TRUE) %>%
      modify_header(estimate = glue("{sub_target$names[i]}")) %>%
      multinom_split_results()
  }
  
  for (j in 1:2) {
    models_list <- lapply(names(results), function(name) results[[name]][[j]])
    models_name <- glue("{sub_target$abbr}")
    model <- tbl_merge(models_list, tab_spanner = models_name) %>% 
      multinom_filter_p() %>%
      bold_labels() %>%
      italicize_levels() %>%
      modify_header(label = "") %>% 
      as_gt() %>%
      fmt_number(suffixing = TRUE) %>%
      rm_spanners() %>%
      tab_header(
        title = ifelse(j == 1, glue("Déclaration dans l'EHIS sans repérage dans le SNDS"), glue("Repérage dans le SNDS sans déclaration dans l'EHIS")), 
        subtitle = ifelse(plc == "drom", glue("DROM"), glue("Métropole"))) %>%
      tab_style(
        style = cell_fill(color = "lightgrey"),
        locations = cells_body(rows = (row_type != "level"))
      )
    
    multinom_agg_tables[[glue("model_{j}_{plc}")]] <- data
    gtsave(model, filename = glue("output/multinom_tables/model_{j}_{plc}.html"))
    gtsave(model, filename = glue("output/multinom_tables/model_{j}_{plc}.docx"))
    print(model)
  }
}

### Nettoyage
rm(multinom_filter_p, multinom_split_results, plc, results, i, j, models_list, models_name, model)
rm(results_drom, results_metro)