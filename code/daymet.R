library(daymetr)

p_daymet <- str_c(.path$dat, "weather.csv")
if (!file.exists(p_daymet)) {
  df_coarse_coord <- df_neon_meta %>%
    group_by(site) %>%
    summarise(
      lat = mean(lat),
      lon = mean(lon)
    )

  ls_df_daymet <- vector(mode = "list")
  for (i in 1:nrow(df_coarse_coord)) {
    ls_df_daymet[[i]] <- daymetr::download_daymet(
      site = df_coarse_coord$site[i],
      lat = df_coarse_coord$lat[i],
      lon = df_coarse_coord$lon[i],
      start = 2013,
      end = 2022,
      internal = TRUE,
      simplify = TRUE
    ) %>%
      # filter(measurement %in% c("dayl..s.", "tmax..deg.c.", "tmin..deg.c.", "prcp..mm.day.", "vp..Pa.")) %>%
      spread(key = "measurement", value = "value") %>%
      rename(
        dayl = `dayl..s.`,
        prcp = `prcp..mm.day.`,
        srad = `srad..W.m.2.`,
        swe = `swe..kg.m.2.`,
        tmax = `tmax..deg.c.`,
        tmin = `tmin..deg.c.`,
        vp = `vp..Pa.`
      ) %>%
      mutate(date = as.Date(yday, origin = paste0(year, "-01-01")) - 1) %>%
      mutate(temp = (tmax + tmin / 2)) %>%
      mutate(
        site = df_coarse_coord$site[i] # ,
        # lat = df_coarse_coord$lat[i],
        # lon = df_coarse_coord$lon[i]
      ) %>%
      select(
        site, # lat, lon,
        date,
        dayl, srad,
        tmax, tmin, temp,
        prcp, swe, vp
      )
  }
  df_daymet <- bind_rows(ls_df_daymet)
  write_csv(df_daymet, p_daymet)
} else {
  df_daymet <- read_csv(p_daymet)
}
