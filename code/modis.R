library(MODISTools)
products <- mt_products() %>%
  filter(str_detect(description, "EVI"))
bands <- mt_bands(product = "MOD13Q1")

df_coarse_coord <- df_neon_meta %>%
  group_by(site) %>%
  summarise(
    lat = mean(lat),
    lon = mean(lon)
  )

p_modis <- str_c(.path$dat, "evi_250m.csv")
if (!file.exists(p_modis)) {
  df_modis_terra <- mt_batch_subset(
    df = df_modis_coord,
    product = "MOD13Q1",
    band = "250m_16_days_EVI",
    internal = TRUE,
    start = "2013-01-01",
    end = "2023-05-01"
  )
  df_modis_aqua <- mt_batch_subset(
    df = df_modis_coord,
    product = "MYD13Q1",
    band = "250m_16_days_EVI",
    internal = TRUE,
    start = "2013-01-01",
    end = "2023-05-01"
  )

  df_modis <- bind_rows(df_modis_terra, df_modis_aqua) %>%
    select(site, date, evi) %>%
    arrange(site, date)

  write_csv(df_modis, p_modis)
} else {
  df_modis <- read_csv(p_modis)
}
