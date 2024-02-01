## get NEON status intensity data from NPN portal
# https://data.usanpn.org/observations/
# All sites
# 2013 Jan to 2023 Dec
# Accessed on Jan 23, 2024

f_met <- str_c(.path$dat, "discrete_phenometric.csv")
f_met_short <- str_c(.path$dat_short, "discrete_phenometric.csv")

f_sta <- str_c(.path$dat, "discrete_status.csv")
f_sta_short <- str_c(.path$dat_short, "discrete_status.csv")

f_int <- str_c(.path$dat, "discrete_intensity.csv")
f_int_short <- str_c(.path$dat_short, "discrete_intensity.csv")

if (!file.exists(f_met)) {
  npn_phenophases <- rnpn::npn_phenophases()

  df_npn_met <- read_csv(str_c(.path$npn, "individual_phenometrics/2024-02-01/individual_phenometrics_data.csv")) %>%
    left_join(npn_phenophases, by = c("Phenophase_ID" = "phenophase_id")) %>%
    rename(
      site = Site_Name,
      id = Plant_Nickname,
      event_code = pheno_class_id,
      year = First_Yes_Year,
      first_yes_doy = First_Yes_DOY
    ) %>%
    mutate(site = str_sub(site, 1, 4)) %>%
    filter(id %in% df_neon_meta$id) %>%
    mutate(event = factor(event_code,
      levels = c(1, 2, 3, 4, 5),
      labels = c("first_leaf", "young_leaf", "full_leaf", "colored_leaf", "fallen_leaf")
    )) %>%
    drop_na(event) %>%
    select(site, id, event, event_code, year, first_yes_doy) %>%
    distinct(site, id, event, event_code, year, .keep_all = T) %>%
    mutate(t_cv = case_when(
      year >= 2021 ~ T,
      TRUE ~ F
    )) %>%
    left_join(
      df_neon_meta %>%
        drop_na(lat, lon) %>%
        distinct(site, id, lat, lon) %>%
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

  df_npn_met %>%
    filter(site %in% c("BART", "HARV")) %>%
    filter(year == 2017) %>%
    mutate(date = lubridate::date(str_c(year, "-01-01")) + first_yes_doy - 1) %>%
    ggplot() +
    geom_segment(aes(x = date, xend = date, y = 0, yend = 1, col = id), alpha = 0.2) +
    facet_wrap(. ~ event * site, nrow = 5) +
    guides(col = "none")

  write_csv(df_npn_met, f_met)
  write_csv(df_npn_met %>% filter(site %in% c("HARV", "SJER")), f_met_short)

  df_npn_sta_int <- read_csv(str_c(.path$npn, "status_intensity/2024-01-23/status_intensity_observation_data.csv")) %>%
    filter(Phenophase_Status != -1) %>%
    left_join(npn_phenophases, by = c("Phenophase_ID" = "phenophase_id")) %>%
    rename(
      site = Site_Name,
      id = Plant_Nickname,
      date = Observation_Date,
      event_code = pheno_class_id,
      status_code = Phenophase_Status,
      intensity = Intensity_Value
    ) %>%
    mutate(site = str_sub(site, 1, 4)) %>%
    filter(id %in% df_neon_meta$id) %>%
    mutate(event = factor(event_code,
      levels = c(1, 2, 3, 4, 5),
      labels = c("first_leaf", "young_leaf", "full_leaf", "colored_leaf", "fallen_leaf")
    )) %>%
    drop_na(event) %>%
    mutate(status = factor(status_code, levels = c(0, 1), labels = c("no", "yes"))) %>%
    mutate(intensity = factor(intensity, levels = c("Less than 5%", "5-24%", "25-49%", "50-74%", "75-94%", "95% or more"))) %>%
    mutate(intensity_code = case_when(
      intensity == "Less than 5%" ~ 1,
      intensity == "5-24%" ~ 2,
      intensity == "25-49%" ~ 3,
      intensity == "50-74%" ~ 4,
      intensity == "75-94%" ~ 5,
      intensity == "95% or more" ~ 6
    )) %>%
    select(site, id, date, event, event_code, status, status_code, intensity, intensity_code) %>%
    arrange(intensity) %>%
    distinct(site, id, date, event, event_code, .keep_all = T) %>%
    mutate(t_cv = case_when(
      lubridate::year(date) >= 2021 ~ T,
      TRUE ~ F
    )) %>%
    left_join(
      df_neon_meta %>%
        drop_na(lat, lon) %>%
        distinct(site, id, lat, lon) %>%
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

  df_npn_sta <- df_npn_sta_int %>%
    select(site, id, date, event, event_code, status, status_code, tag)

  df_npn_sta %>%
    filter(site %in% c("BART", "HARV")) %>%
    filter(lubridate::year(date) == 2017) %>%
    ggplot() +
    geom_line(aes(x = date, y = status_code, group = id, col = id), alpha = 0.2) +
    facet_wrap(. ~ event * site, nrow = 5) +
    guides(col = "none")

  write_csv(df_npn_sta, f_sta)
  write_csv(df_npn_sta %>% filter(site %in% c("HARV", "SJER")), f_sta_short)

  df_npn_int <- df_npn_sta_int %>%
    select(site, id, date, event, event_code, intensity, intensity_code, tag) %>%
    drop_na(intensity)

  df_npn_int %>%
    filter(site %in% c("BART", "HARV")) %>%
    filter(lubridate::year(date) == 2017) %>%
    ggplot() +
    geom_line(aes(x = date, y = intensity_code, group = id, col = id), alpha = 0.2) +
    facet_wrap(. ~ event * site, nrow = 5) +
    guides(col = "none")

  write_csv(df_npn_int, f_int)
  write_csv(df_npn_int %>% filter(site %in% c("HARV", "SJER")), f_int_short)
} else {
  df_npn_met <- read_csv(f_met)
  df_npn_sta <- read_csv(f_sta)
  df_npn_int <- read_csv(f_int)
}
