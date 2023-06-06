
f_ps <- str_c(.path$dat, "continuous_3m.csv")

if (!file.exists(f_ps)) {
  df_plant_neon <- df_neon_meta %>%
    group_by(site) %>%
    arrange(id) %>%
    mutate(ps_id = row_number()) %>%
    ungroup() %>%
    mutate(taxa = "all")

  source("code/func_ps_patch.R")
  source("code/func_ps_order.R")
  func_ps_batch_order(dir = .path$ps, df_plant_neon, v_site = NULL)
  source("code/func_ps_down.R")
  func_ps_batch_download(dir = .path$ps, v_site = NULL)
  source("code/func_ps_ts.R")
  func_ps_batch_ts(dir = .path$ps, tsdir = str_c(.path$ps, "ts/"), v_taxa = "all", v_site = NULL)
  source("code/func_proc_ps.R")

  ls_f_ps <- list.files(str_c(.path$ps, "ts/"), full.names = T)
  ls_df_ps <- vector(mode = "list")
  for (f_ps in ls_f_ps) {
    site <- f_ps %>%
      str_split("ps_", simplify = T) %>%
      `[`(2) %>%
      str_sub(1, 4)
    ls_df_ps[[site]] <- read_rds(f_ps) %>%
      process_ps() %>%
      rename(ps_id = id) %>%
      mutate(site = site) %>%
      left_join(df_plant_neon %>% select(site, id, ps_id, lon, lat), by = c("site", "ps_id", "lon", "lat")) %>%
      select(-ps_id, -lon, -lat)
  }
  df_ps <- bind_rows(ls_df_ps) %>%
    select(site, id, date, evi) %>%
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

  df_ps %>%
    ggplot() +
    geom_line(aes(x = date, y = evi, group = id, col = tag), alpha = 0.2) +
    facet_wrap(. ~ site)
  write_csv(df_ps, f_ps)
} else {
  df_ps <- read_csv(f_ps)
}
