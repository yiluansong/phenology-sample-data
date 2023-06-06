## get NEON individual phenometrics data from NPN portal
# https://data.usanpn.org/observations/
# All sites
# 2010 Jan to 2023 Mar
# Accessed on Apr 21, 2023

# f_met <- str_c(.path$dat, "individual_phenometrics.csv")
f_int <- str_c(.path$dat, "discrete.csv")
if (!file.exists(f_met)) {
  npn_phenophases <- rnpn::npn_phenophases()

  # df_npn_met <- read_csv(str_c(.path$npn, "individual_phenometrics/individual_phenometrics_data.csv")) %>%
  #   left_join(npn_phenophases, by = c("Phenophase_ID" = "phenophase_id")) %>%
  #   mutate(event = case_when(
  #     pheno_class_id == 1 ~ "first leaf",
  #     pheno_class_id == 2 ~ "young leaf",
  #     pheno_class_id == 3 ~ "full leaf"
  #   )) %>%
  #   drop_na(event) %>%
  #   rowwise() %>%
  #   mutate(plot = str_split(Site_Name, "\\.", simplify = T)[1]) %>%
  #   mutate(site = str_split(plot, "_", simplify = T)[1]) %>%
  #   filter(plot %in% df_neon_meta$plot) %>%
  #   ungroup() %>%
  #   select(site, # plot,
  #     # genus = Genus, species = Species,
  #     # functional_type = Species_Functional_Type,
  #     id = Plant_Nickname,
  #     event_id = pheno_class_id, event,
  #     year = First_Yes_Year, first_yes_doy = First_Yes_DOY
  #   )
  #
  # # df_npn_met %>%
  # #   ggplot(aes(x = site, y = doy, col = event))+
  # #   geom_point()+
  # #   facet_wrap(.~event)
  # write_csv(df_npn_met, f_met)

  df_npn_int <- read_csv(str_c(.path$npn, "status_intensity/status_intensity_observation_data.csv")) %>%
    filter(Phenophase_Status != -1) %>%
    filter(Plant_Nickname %in% df_neon_meta$id) %>%
    mutate(site = str_sub(Site_Name, 1, 4)) %>%
    left_join(npn_phenophases, by = c("Phenophase_ID" = "phenophase_id")) %>%
    mutate(event = case_when(
      pheno_class_id == 1 ~ "first_leaf",
      pheno_class_id == 2 ~ "young_leaf",
      pheno_class_id == 3 ~ "full_leaf",
      pheno_class_id == 4 ~ "colored_leaf",
      pheno_class_id == 5 ~ "fallen_leaf"
    )) %>%
    drop_na(event) %>%
    rename(
      id = Plant_Nickname,
      date = Observation_Date,
      observer = ObservedBy_Person_ID,
      event_code = pheno_class_id,
      status_code = Phenophase_Status, intensity = Intensity_Value
    ) %>%
    mutate(status = case_when(
      status_code == 1 ~ "yes",
      status_code == 0 ~ "no"
    )) %>%
    mutate(intensity_code = case_when(
      # intensity == "Less than 3" ~ 1,
      # intensity == "3 to 10" ~ 2,
      # intensity == "11 to 100" ~ 3,
      # intensity == "101 to 1,000" ~ 4,
      # intensity == "1,001 to 10,000" ~ 5,
      # intensity == "More than 10,000" ~ 6,
      intensity == "Less than 5%" ~ 1,
      intensity == "5-24%" ~ 1,
      intensity == "25-49%" ~ 3,
      intensity == "50-74%" ~ 4,
      intensity == "75-94%" ~ 5,
      intensity == "95% or more" ~ 6
    )) %>%
    mutate(intensity = case_when(!is.na(intensity_code) ~ intensity)) %>%
    select(site, id, date, observer, event, event_code, status, status_code, intensity, intensity_code) %>%
    arrange(intensity) %>%
    distinct(site, id, date, observer, event, event_code, .keep_all = T) %>%
    mutate(t_cv = case_when(
      lubridate::year(date) >= 2021 ~ T,
      TRUE ~ F
    )) %>%
    left_join(df_neon_meta %>%
      group_by(site) %>%
      mutate(s_cv = case_when(
        (lat > quantile(lat, 0.75) & lon > quantile(lon, 0.75)) ~ T,
        TRUE ~ F
      )) %>%
      ungroup() %>%
      select(id, s_cv),
    by = "id"
    ) %>%
    mutate(tag = case_when(
      (t_cv & !s_cv) ~ "validation_temporal",
      (!t_cv & s_cv) ~ "validation_spatial",
      (t_cv & s_cv) ~ "validation_spatiotemporal",
      TRUE ~ "training"
    )) %>%
    select(-t_cv, -s_cv)

  df_npn_int %>%
    filter(lubridate::year(date) == 2017) %>%
    ggplot() +
    geom_line(aes(x = date, y = status_code, group = id, col = id), alpha = 0.2) +
    facet_wrap(. ~ event * site) +
    guides(col = "none")

  df_npn_int %>%
    filter(lubridate::year(date) == 2017) %>%
    ggplot() +
    geom_line(aes(x = date, y = intensity_code, group = id, col = id), alpha = 0.2) +
    facet_wrap(. ~ event * site) +
    guides(col = "none")
  write_csv(df_npn_int, f_int)
} else {
  # df_npn_met <- read_csv(f_met)
  df_npn_int <- read_csv(f_int)
}
