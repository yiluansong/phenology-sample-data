## get NEON individual tree data from NEON portal
# https://data.neonscience.org/data-products/DP1.10055.001
# All 47 sites
# 2017 Jan to 2023 Mar
# Accessed on Apr 21, 2023

f_neon_meta <- str_c(.path$dat, "metadata.csv")
if (!file.exists(f_neon_meta)) {
  phe <- neonUtilities::stackFromStore(
    filepaths = .path$neon,
    dpID = "DP1.10055.001"
  )

  df_neon_meta <- phe$phe_perindividual %>%
    filter(subtypeSpecification == "primary") %>%
    distinct(site = siteID, plot = plotID, site_lat = decimalLatitude, site_lon = decimalLongitude, id = individualID, species = scientificName, growth_form = growthForm)

  ls_df_neon_coord <- vector(mode = "list")
  for (i in 1:nrow(df_neon_meta)) {
    siteoi <- df_neon_meta$site[i]
    plotoi <- df_neon_meta$plot[i]
    try({ # some coords could not be determined
      ls_df_neon_coord[[siteoi]] <- geoNEON::getLocTOS(
        phe$phe_perindividual %>% filter(plotID == plotoi),
        "phe_perindividual"
      ) %>%
        select(
          site = siteID,
          id = individualID,
          lon = adjDecimalLongitude,
          lat = adjDecimalLatitude,
          uncertainty = adjCoordinateUncertainty
        ) %>%
        group_by(id) %>%
        arrange(uncertainty) %>%
        slice(1)
    })
  }
  df_neon_coord <- bind_rows(ls_df_neon_coord)

  df_neon_meta <- df_neon_meta %>%
    left_join(df_neon_coord, by = c("site", "id")) %>%
    select(site, site_lat, site_lon, id, lat, lon, species, growth_form)

  write_csv(df_neon_meta, f_neon_meta)
} else {
  df_neon_meta <- read_csv(f_neon_meta)
}
