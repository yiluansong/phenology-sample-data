
p_viirs <- str_c(.path$dat, "continuous_500m.csv")
if (!file.exists(p_modis)) {
  df_coarse_coord <- df_neon_meta %>%
    group_by(site) %>%
    summarise(
      lat = mean(lat),
      lon = mean(lon)
    )

  library(MODISTools)
  products <- mt_products() %>%
    filter(str_detect(description, "EVI"))
  bands <- mt_bands(product = "VNP13A1")

  ls_df_viirs <- vector(mode = "list")
  for (i in 1:nrow(df_coarse_coord)) {
    df_viirs_down <- mt_batch_subset(
      df = df_coarse_coord[i, ],
      product = "VNP13A1",
      band = "500_m_16_days_EVI",
      internal = TRUE,
      start = "2013-01-01",
      end = "2023-05-01"
    )

    ls_df_viirs[[i]] <- df_viirs_down %>%
      mutate(site = df_coarse_coord$site[i]) %>%
      select(site, date = calendar_date, evi = value) %>%
      mutate(evi = evi * 0.0001) %>%
      mutate(date = lubridate::as_date(date))
  }
  df_viirs <- bind_rows(ls_df_viirs) %>%
    mutate(t_cv = case_when(
      lubridate::year(date) >= 2021 ~ T,
      TRUE ~ F
    )) %>%
    mutate(tag = case_when(
      t_cv ~ "validation_temporal",
      TRUE ~ "training"
    )) %>%
    select(-t_cv)

  # df_viirs %>%
  #   ggplot()+
  #   geom_line(aes(x = date, y = evi))+
  #   facet_wrap(.~site)
  write_csv(df_viirs, p_viirs)
} else {
  df_viirs <- read_csv(p_viirs)
}
