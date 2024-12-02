#######################################################################################################################
##                                                Taux d'appariement                                                 ##
#######################################################################################################################


### Types d'appariement dans les drom
pairing_types <- sample_drom %>%
  gt() %>%
  fmt_number(decimals = 1, columns = (-year), drop_trailing_zeros = TRUE)
gtsave(pairing_types, filename = "output/sample/pairing_types_drom.docx")

### Taux d'appariement
pairing_metro <- sample_metro %>%
  select(-fideli) %>%
  filter(year %in% 2018:2019) %>%
  pivot_wider(names_from = year, values_from = appar_rate) %>%
  mutate(region = "metro")

pairing_drom_18 <- ehis_drom %>%
  group_by(appar_2018, region) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(region) %>%
  mutate(
    total = sum(count), 
    appar_rate_2018 = 100 - (100 * count / total)) %>%
  filter(appar_2018 == FALSE) %>%
  select(-appar_2018, -count, -total)

pairing_drom_19 <- ehis_drom %>%
  group_by(appar_2019, region) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(region) %>%
  mutate(
    total = sum(count), 
    appar_rate_2019 = 100 - (100 * count / total)) %>%
  filter(appar_2019 == FALSE) %>%
  select(-appar_2019, -count, -total)

pairing_drom <- pairing_drom_18 %>%
  inner_join(pairing_drom_19, by = "region") %>%
  rename("2018" = appar_rate_2018,
         "2019" = appar_rate_2019)

appar_rate <- pairing_metro %>%
  rbind(pairing_drom) %>% 
  mutate(region = case_when(
    region == "metro" ~"Métropole",
    region == "drom" ~ "DROM",
    region == "FRY1" ~ "Guadeloupe",
    region == "FRY2" ~ "Martinique",
    region == "FRY3" ~ "Guyane",
    region == "FRY4" ~ "Réunion",
    region == "FRY5" ~ "Mayotte")) %>%
  gt() %>%
  fmt_number(decimals = 1, drop_trailing_zeros = TRUE) %>%
  cols_move_to_start("region") %>%
  cols_label(region = "") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = 1:2)) %>%
  tab_style(
    style = cell_borders(sides = "bottom"),
    locations = list(
      cells_column_labels(), 
      cells_body(rows = 2)))
gtsave(appar_rate, filename = "output/sample/appar_rate.docx")


#######################################################################################################################
##                                                Dates d'interview                                                  ##
#######################################################################################################################


int_dates_metro <- data_2019_metro %>%
  mutate(refdate = ymd(refdate)) %>%
  count(refdate) %>%
  mutate(region = "Métropole")

int_dates_drom <- data_2019_drom %>%
  mutate(refdate = ymd(refdate)) %>%
  count(refdate) %>%
  mutate(region = "DROM")

int_dates <- bind_rows(int_dates_metro, int_dates_drom)

int_plot <- ggplot(int_dates, aes(x = refdate, fill = region)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = c(ymd("2019-06-30"), ymd("2020-01-01")),
             linetype = "dashed",
             color = "darkblue") +
  scale_x_date(limits = c(ymd("2019-01-01"), NA),
               date_breaks = "2 month", 
               date_labels = "%b %Y") +
  scale_fill_manual(values = c("Métropole" = "cornflowerblue", "DROM" = "skyblue")) +
  labs(title = "",
       x = "",
       y = "Densité des interviews",
       fill = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank())
ggsave("output/sample/interview_dates.png", int_plot, dpi = 300)


#######################################################################################################################
##                                          Taux d'appariement selon l'âge                                           ##
#######################################################################################################################


plot <- ggplot(ehis_metro, aes(x = age, fill = factor(appar_2019))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    name = "Appariement",
    values = c("0" = "cornflowerblue", "1" = "skyblue"), 
    labels = c("Non", "Oui")) +
  labs(title = "Taux d'appariement selon l'âge",
       subtitle = "Métropole, 2019",
       x = "Âge",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
ggsave("output/sample/appar_age_metro.png", plot, dpi = 300)

plot <- ggplot(ehis_drom, aes(x = age, fill = factor(appar_2019))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    name = "Appariement",
    values = c("0" = "cornflowerblue", "1" = "skyblue"), 
    labels = c("Non", "Oui")) +
  labs(title = "Taux d'appariement selon l'âge",
       subtitle = "DROM, 2019",
       x = "Âge",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
ggsave("output/sample/appar_age_drom.png", plot, dpi = 300)


#######################################################################################################################
##                                Comparaison des individus appariés et non-appariés                                 ##
#######################################################################################################################


### Significativité des différences entre les variables catégorielles
chi_test <- function(data_file, var_name, plc) {
  label <- var_label(data_file[[var_name]] %||% var_name)
  data_file %>%
    chisq_test(as.formula(glue("{var_name} ~ appar_2019"))) %>%
    mutate(variable = label, place = plc)
}

test_summary <- data.frame()

for (plc in c("metro", "drom")) {
  for (var in c("sexe", "age_cat", "hatlevel", "situa", "couple", "stoc", "deg_urb", "natio1n_1", "immig_2g", "lang_FR", "couv1", "compsant", "hs1_s", "hs2", "liter_score_s", "proxy")) {
    test_summary <- rbind(
      test_summary,
      chi_test(get(glue("ehis_{plc}")), var, plc)
    )
  }
  if (plc == "metro") {
    for (var in c("qpv", "intmethod")) {
      test_summary <- rbind(
        test_summary,
        chi_test(get(glue("ehis_{plc}")), var, plc)
      )
    }
  }
}

format_pval <- function(p) {
  case_when(
    is.na(p) ~ "NA",
    p < 0.001 ~ "< 0.001",
    TRUE ~ glue("{round(p,3)}")
  )
}

test_summary <- test_summary %>%
  select(-statistic) %>%
  mutate(p_value = format_pval(p_value)) %>%
  pivot_wider(names_from = place, values_from = c("chisq_df", "p_value")) %>%
  arrange(desc(variable)) %>%
  relocate(variable)

test_table <- test_summary %>%
  gt() %>%
  tab_spanner(label = "Métropole", columns = contains("metro"), id = "metro") %>%
  tab_spanner(label = "DROM", columns = contains("drom"), id = "drom") %>%
  cols_label(
    variable = "",
    starts_with("chisq") ~ "ddl",
    starts_with("p_") ~ "p val") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners())

gtsave(test_table, filename = "output/declarations/test_summary.html")
gtsave(test_table, filename = "output/declarations/test_summary.docx")

### Comparaison des distributions
categorical_summary <- function(data_file, var_name, plc) {
  var_sym <- sym(var_name)
  label <- var_label(data_file[[var_name]] %||% var_name)
  data_file %>%
    group_by(appar_2019, !!var_sym) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(appar_2019) %>%
    mutate(share = 100 * count / sum(count)) %>%
    select(-count) %>%
    pivot_wider(
      names_from = appar_2019,
      values_from = share) %>%
    mutate(variable = label, place = plc) %>%
    rename(modalities = var_name)
}

sample_summary <- data.frame()

for (plc in c("metro", "drom")) {
  for (var in c("sexe", "age_cat", "hatlevel", "situa", "couple", "stoc", "deg_urb", "natio1n_1", "immig_2g", "lang_FR", "couv1", "compsant", "hs1_s", "hs2", "liter_score_s", "proxy")) {
    sample_summary <- rbind(
      sample_summary,
      categorical_summary(get(glue("ehis_{plc}")), var, plc)
    )
  }
  if (plc == "metro") {
    for (var in c("qpv", "intmethod")) {
      sample_summary <- rbind(
        sample_summary,
        categorical_summary(ehis_metro, var, plc)
      )
    }
  }
}

sample_summary <- sample_summary %>%
  pivot_wider(names_from = place, values_from = c("FALSE", "TRUE")) %>%
  arrange(desc(variable)) %>%
  relocate(variable) %>%
  relocate("TRUE_metro", .before = "FALSE_drom") %>%
  filter(modalities != "Non", modalities != "NA")

sample_table <- sample_summary %>% 
  group_by(variable) %>%
  gt() %>%
  fmt_number(decimals = 1, drop_trailing_zeros = TRUE) %>%
  tab_spanner(label = "Métropole", columns = contains("metro"), id = "metro") %>%
  tab_spanner(label = "DROM", columns = contains("drom"), id = "drom") %>% 
  cols_label(
    variable = "",
    modalities = "",
    starts_with("FALSE") ~ "Non-apparié",
    starts_with("TRUE") ~ "Apparié") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners()) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightgrey"),
      cell_text(style = "italic", weight = "bold")),
    location = cells_row_groups())

gtsave(sample_table, filename = "output/declarations/sample_summary.html")
gtsave(sample_table, filename = "output/declarations/sample_summary.docx")


#######################################################################################################################
##                                                     Nettoyage                                                     ##
#######################################################################################################################


rm(sample_drom, sample_metro, pairing_drom_18, pairing_drom_19, pairing_drom, pairing_metro,
   int_dates_metro, int_dates_drom, int_dates, int_plot, plot, pairing_types, appar_rate)
rm(ehis_drom, ehis_metro, plc, var, categorical_summary, chi_test, format_pval)