# 1. LIBRARIES
pacman::p_load(
    rstac,
    sf,
    terra,
    arcgislayers,
    tidyverse,
    elevatr,
    tidyterra
)

# 2. CITY BOUNDARIES

url <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/World_Urban_Areas/FeatureServer/0"

city_data <- arcgislayers::arc_open(
    url
)

city_sf <-
    arcgislayers::arc_select(
        city_data,
        fields = "NAME",
        where = "NAME = 'São Paulo'",
        crs = 4326
    )

plot(
    sf::st_geometry(
        city_sf
    )
)

city_bbox <- sf::st_bbox(city_sf)

main_dir <- getwd()

# 3. ESA Land cover - query and collections
ms_query <-
    rstac::stac(
        "https://planetarycomputer.microsoft.com/api/stac/v1"
    )

ms_query

ms_collections <-
    ms_query |>
    rstac::collections() |>
    rstac::get_request()

print(ms_collections, n = 123)

collections <- "esa-worldcover"

ms_esa_query <-
    rstac::stac_search(
        q = ms_query,
        collections = collections,
        # ids = "India_123121303_2023-04-25",
        datetime = "2021-01-01T00:00:00Z/2021-12-31T23:59:59Z",
        bbox = city_bbox,
        limit = 100
    ) |>
    rstac::get_request()

# 4. ESA Land cover - data download

ms_query_signin <-
    msrstac::items_sign(
        ms_esa_query,
        rstac::sign_planetary_computer()
    )

ms_query_signin

rstac::assets_download(
    items = ms_query_signin,
    asset_names = "map",
    output_dir = main_dir,
    overwrite = TRUE
)

# 5. ESA Land cover - load data

version <- "v200"
year <- "2021"
asset_name <- "map"

data_dir <- paste0(
    main_dir, "/",
    collections, "/",
    version, "/",
    year, "/",
    asset_name
)

raster_file <- list.files(
    data_dir,
    full.names = TRUE
)

land_cover_raster <- terra::rast(
    raster_file
)

city_land_cover <-
    terra::crop(
        land_cover_raster,
        terra::vect(city_sf),
        snap = "in"
    )

terra::plot(city_land_cover)

dem <- elevatr::get_elev_raster(
    locations = city_sf,
    z = 11,
    clip = "bbox"
) |>
    terra::rast()

city_land_cover_resampled <- terra::resample(
    x = city_land_cover,
    y = dem,
    method = "near"
)

terra::plot(city_land_cover_resampled)

actual_values <- sort(unique(terra::values(city_land_cover_resampled)))

values <- seq(
    from = 10,
    to = 100,
    by = 10
)

values <- append(
    values,
    95,
    after = 9
)

labels <- c(
    "Tree cover",
    "Shrubland",
    "Grassland",
    "Cropland",
    "Built-up",
    "Bare/sparse vegetation",
    "Snow and Ice",
    "Permanent water bodies",
    "Herbaceous wetland",
    "Mangroves",
    "Moss and lichen"
)

codebook <- data.frame(cbind(values, labels))
actual_labels <- subset(codebook, values %in% actual_values)

names(city)

map <- ggplot() +
    tidyterra::geom_spatraster(
        data = as.factor(city_land_cover_resampled),
        use_coltab = TRUE,
        maxcell = Inf
    ) +
    tidyterra::scale_fill_coltab(
        data = as.factor(city_land_cover_resampled),
        name = "Land cover classes",
        labels = actual_labels$labels
    ) +
    geom_sf(
        data = city_sf,
        fill = "transparent",
        color = "white",
        linewidth = .5
    ) +
    theme_void()
