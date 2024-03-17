## get NEON individual tree data from NEON portal
# https://data.neonscience.org/data-products/DP1.10055.001
# All 47 sites
# 2017 Jan to 2023 Dec
# Accessed on Jan 23, 2024

f_neon_meta <- str_c(.path$dat, "metadata.csv")
f_neon_meta_short <- str_c(.path$dat_short, "metadata.csv")

if (!file.exists(f_neon_meta)) {
  phe <- neonUtilities::stackFromStore(
    filepaths = .path$neon,
    dpID = "DP1.10055.001"
  )

  df_neon_meta <- phe$phe_perindividual %>%
    filter(subtypeSpecification == "primary") %>%
    distinct(site = siteID, plot = plotID, site_lat = decimalLatitude, site_lon = decimalLongitude, id = individualID, scientific_name = scientificName, growth_form = growthForm)

  df_neon_plot <- df_neon_meta %>%
    distinct(site, plot)

  ls_df_neon_coord <- vector(mode = "list")
  for (i in 1:nrow(df_neon_plot)) {
    siteoi <- df_neon_plot$site[i]
    plotoi <- df_neon_plot$plot[i]
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
    select(site, site_lat, site_lon, id, lat, lon, scientific_name, growth_form)

  df_neon_taxa <- df_neon_meta %>%
    distinct(scientific_name) %>%
    arrange(scientific_name) %>%
    rowwise() %>%
    mutate(
      genus = str_split(scientific_name, " ", simplify = T)[1],
      species = str_split(scientific_name, " ", simplify = T)[2]
    )

  for (i in 1:nrow(df_neon_taxa)) {
    genus <- df_neon_taxa$genus[i]
    species <- df_neon_taxa$species[i]

    family <- tryCatch(
      {
        taxize::tax_name(str_c(genus, " ", species), get = "family", db = "ncbi")$family
      },
      error = function(e) {
        NA
      }
    )

    df_neon_taxa$family[i] <- family
  }

  df_neon_meta <- df_neon_meta %>%
    left_join(df_neon_taxa,
      by = "scientific_name"
    )

  write_csv(df_neon_meta, f_neon_meta)
  write_csv(df_neon_meta %>% filter(site %in% c("HARV", "SJER")), f_neon_meta_short)
} else {
  df_neon_meta <- read_csv(f_neon_meta)
}
