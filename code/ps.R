f_ps <- str_c(.path$dat, "continuous_3m.csv")
f_ps_short <- str_c(.path$dat_short, "continuous_3m.csv")

if (!file.exists(f_ps)) {
  df_plant_neon <- df_neon_meta %>%
    group_by(site) %>%
    arrange(id) %>%
    mutate(ps_id = row_number()) %>%
    ungroup()

  # NEON PlanetScope data downloaded and processed using our batchplanet package.
  ls_f_ps <- list.files(.path$ps, full.names = T)
  ls_df_ps <- vector(mode = "list")
  for (f_ps in ls_f_ps) {
    site <- f_ps %>%
      str_split("evi_", simplify = T) %>%
      `[`(2) %>%
      str_sub(1, 4)

    ls_df_ps[[site]] <- read_rds(f_ps) %>%
      mutate(site = site) %>%
      select(site, id, date, evi) %>%
      group_by(site, id, date) %>%
      summarise(evi = mean(evi), .groups = "drop")
  }

  df_ps <- bind_rows(ls_df_ps) %>%
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

  df_ps %>%
    filter(site %in% c("HARV", "SJER")) %>%
    ggplot() +
    geom_line(aes(x = date, y = evi, group = id, col = tag), alpha = 0.2) +
    facet_wrap(. ~ site)

  write_csv(df_ps, f_ps)
  write_csv(df_ps %>% filter(site %in% c("HARV", "SJER")), f_ps_short)
} else {
  df_ps <- read_csv(f_ps)
}
