f_modis <- str_c(.path$dat, "continuous_250m.csv")
f_modis_short <- str_c(.path$dat_short, "continuous_250m.csv")

start_date <- "2000-01-01"
end_date <- "2023-12-31"

if (!file.exists(f_modis)) {
  df_coarse_coord <- read_csv("data/processed/metadata.csv") %>%
    group_by(site) %>%
    summarise(
      lat = mean(lat, na.rm = T),
      lon = mean(lon, na.rm = T)
    ) %>%
    drop_na()

  library(MODISTools)
  # products <- mt_products() %>%
  #   filter(str_detect(description, "EVI"))
  # bands <- mt_bands(product = "MOD13Q1")

  ls_df_modis <- vector(mode = "list")
  for (i in 1:nrow(df_coarse_coord)) {
    df_terra_evi <- mt_batch_subset(
      df = df_coarse_coord[i, ],
      product = "MOD13Q1",
      band = "250m_16_days_EVI",
      internal = TRUE,
      start = start_date,
      end = end_date
    ) %>%
      select(
        date = calendar_date,
        evi = value
      )

    df_terra_qa <- mt_batch_subset(
      df = df_coarse_coord[i, ],
      product = "MOD13Q1",
      band = "250m_16_days_VI_Quality",
      internal = TRUE,
      start = start_date,
      end = end_date
    ) %>%
      select(value)

    df_aqua_evi <- mt_batch_subset(
      df = df_coarse_coord[i, ],
      product = "MYD13Q1",
      band = "250m_16_days_EVI",
      internal = TRUE,
      start = start_date,
      end = end_date
    ) %>%
      select(
        date = calendar_date,
        evi = value
      )

    df_aqua_qa <- mt_batch_subset(
      df = df_coarse_coord[i, ],
      product = "MYD13Q1",
      band = "250m_16_days_VI_Quality",
      internal = TRUE,
      start = start_date,
      end = end_date
    ) %>%
      select(qa = value)

    ls_df_modis[[i]] <- bind_rows(
      bind_cols(df_terra_evi, df_terra_qa),
      bind_cols(df_aqua_evi, df_aqua_qa)
    ) %>%
      mutate(qa_str = R.utils::intToBin(qa) %>% str_pad(15, pad = "0", side = "left")) %>%
      mutate(qa_good = str_sub(qa_str, 1, 2) %>% as.numeric()) %>% # https://lpdaac.usgs.gov/documents/103/MOD13_User_Guide_V6.pdf table 5 on page 16
      mutate(qa_good = (qa_good == 0)) %>%
      filter(qa_good) %>%
      mutate(site = df_coarse_coord$site[i]) %>%
      select(site, date, evi) %>%
      mutate(evi = evi * 0.0001) %>%
      mutate(date = lubridate::as_date(date))

    print(i)
  }
  df_modis <- bind_rows(ls_df_modis) %>%
    arrange(date) %>%
    mutate(t_cv = case_when(
      lubridate::year(date) >= 2021 ~ T,
      TRUE ~ F
    )) %>%
    mutate(tag = case_when(
      t_cv ~ "validation_temporal",
      TRUE ~ "training"
    )) %>%
    select(-t_cv)

  df_modis %>%
    ggplot() +
    geom_line(aes(x = date, y = evi, col = tag)) +
    facet_wrap(. ~ site)

  write_csv(df_modis, f_modis)
  write_csv(df_modis %>% filter(site %in% c("HARV", "SJER")), f_modis_short)
} else {
  df_modis <- read_csv(f_modis)
}
