# 0. Install and load required packages
required_packages <- c("readr", "dplyr", "ggplot2", "sf",
                       "rnaturalearth", "rnaturalearthdata", "ggrepel", 
                       "lubridate", "viridis", "patchwork")

if (getOption("repos")["CRAN"] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org/"))
}

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) {
  install.packages(new_packages)
}

suppressPackageStartupMessages({
    library(readr)
    library(dplyr)
    library(ggplot2)
    library(sf)
    library(rnaturalearth)
    library(rnaturalearthdata)
    library(ggrepel)
    library(lubridate)
    library(viridis) 
    library(patchwork)
})

# Create a dedicated directory for PNG plots
png_plots_dir <- "NorthPoleLocationsPlots"
if (!dir.exists(png_plots_dir)) {
  dir.create(png_plots_dir)
}

# --- Section 1: IGRF North Pole Visualization ---
cat("--- IGRF North Pole Analysis ---\n")

file_path_north_pole <- "NorthPoleLocations/Archive/IGRF_North_Pole_Locations_(1590–2025).csv"
north_pole_data_raw <- read_table(
  file_path_north_pole,
  col_names = c("N_Lon_Raw", "N_Lat_Raw", "Year_Raw"),
  col_types = "ddd",
  show_col_types = FALSE
)

north_pole_processed_df <- north_pole_data_raw %>%
  mutate(
    Year = floor(as.numeric(Year_Raw)),
    N_Lat = as.numeric(N_Lat_Raw),
    N_Lon_Original = as.numeric(N_Lon_Raw),
    N_Lon = ifelse(N_Lon_Original > 180, N_Lon_Original - 360, N_Lon_Original)
  ) %>%
  select(Year, N_Lat, N_Lon, N_Lon_Original) %>%
  filter(!is.na(Year) & !is.na(N_Lat) & !is.na(N_Lon)) %>%
  arrange(Year)

world_map <- ne_countries(scale = "medium", returnclass = "sf")

north_pole_sf <- north_pole_processed_df %>%
  st_as_sf(coords = c("N_Lon", "N_Lat"), crs = 4326)

north_pole_path_sf <- north_pole_sf %>%
  arrange(Year) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING")

crs_north_pole_stereo <- paste0("+proj=laea +lat_0=90 +lon_0=", 
                                round(median(north_pole_processed_df$N_Lon, na.rm=TRUE),0), 
                                " +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

p_igrf_north_map <- ggplot() +
  geom_sf(data = world_map, fill = "gray85", color = "white", linewidth = 0.1) +
  geom_sf(data = north_pole_path_sf, color = "darkorange", linewidth = 1.2, alpha = 0.9) +
  geom_sf(data = north_pole_sf, aes(color = Year), size = 2.5, alpha = 0.9) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  coord_sf(crs = crs_north_pole_stereo, xlim = c(-2800000, 2800000), ylim = c(-2800000, 2800000)) +
  labs(title = "Drift of the Geomagnetic North Pole (IGRF Model)",
       subtitle = paste(min(north_pole_processed_df$Year, na.rm=TRUE), "-", max(north_pole_processed_df$Year, na.rm=TRUE)),
       color = "Year", x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.background = element_rect(fill = "lightcyan"),
        panel.grid.major = element_line(color = "gray70", linetype = "dashed", linewidth = 0.25))

ggsave("igrf_north_pole_wander_map.png", plot = p_igrf_north_map, width = 7, height = 7, dpi = 300)
ggsave(file.path(png_plots_dir, "igrf_north_pole_wander_map.png"), plot = p_igrf_north_map, width = 7, height = 7, dpi = 300)

label_interval_n <- ifelse(nrow(north_pole_sf) > 80, 50, 25)
years_to_label_n_igrf <- north_pole_sf %>% 
    filter(Year %in% seq(from = min(Year, na.rm=TRUE), to = max(Year, na.rm=TRUE), by = label_interval_n) | 
           Year == min(Year, na.rm=TRUE) | Year == max(Year, na.rm=TRUE))

p_igrf_north_map_labels <- p_igrf_north_map +
  geom_sf_text(data = years_to_label_n_igrf, aes(label = Year), 
               size = 2.2, color = "black", nudge_y = 180000, check_overlap = TRUE)

ggsave("igrf_north_pole_wander_map_labels.png", plot = p_igrf_north_map_labels, width = 7, height = 7, dpi = 300)
ggsave(file.path(png_plots_dir, "igrf_north_pole_wander_map_labels.png"), plot = p_igrf_north_map_labels, width = 7, height = 7, dpi = 300)

p_igrf_north_lat_time <- ggplot(north_pole_processed_df, aes(x = Year, y = N_Lat)) +
  geom_line(color = "firebrick", linewidth = 1) + geom_point(color = "firebrick", size = 1.5, alpha = 0.6) +
  labs(title = "Geomagnetic North Pole Latitude (IGRF)", x = "Year", y = "Latitude (°N)") + theme_minimal()
ggsave("igrf_north_pole_latitude_vs_time.png", plot = p_igrf_north_lat_time, width = 9, height = 5, dpi = 300)
ggsave(file.path(png_plots_dir, "igrf_north_pole_latitude_vs_time.png"), plot = p_igrf_north_lat_time, width = 9, height = 5, dpi = 300)

p_igrf_north_lon_time <- ggplot(north_pole_processed_df, aes(x = Year, y = N_Lon)) +
  geom_line(color = "darkgreen", linewidth = 1) + geom_point(color = "darkgreen", size = 1.5, alpha = 0.6) +
  labs(title = "Geomagnetic North Pole Longitude (IGRF, normalized)", x = "Year", y = "Longitude (°)") + theme_minimal()
ggsave("igrf_north_pole_longitude_vs_time.png", plot = p_igrf_north_lon_time, width = 9, height = 5, dpi = 300)
ggsave(file.path(png_plots_dir, "igrf_north_pole_longitude_vs_time.png"), plot = p_igrf_north_lon_time, width = 9, height = 5, dpi = 300)

if (requireNamespace("patchwork", quietly = TRUE)) {
  p_igrf_north_combined_ts <- p_igrf_north_lat_time / p_igrf_north_lon_time + plot_annotation(title = "Changes in Geomagnetic North Pole Position (IGRF)")
  ggsave("igrf_north_pole_lat_lon_vs_time_combined.png", plot = p_igrf_north_combined_ts, width = 9, height = 8, dpi = 300)
  ggsave(file.path(png_plots_dir, "igrf_north_pole_lat_lon_vs_time_combined.png"), plot = p_igrf_north_combined_ts, width = 9, height = 8, dpi = 300)
}

# --- Section 2: IGRF South Pole Visualization ---
cat("--- IGRF South Pole Analysis ---\n")

file_path_south_pole <- "C:/Users/barte/OneDrive/Pulpit/R/NorthPoleLocations/Archive/IGRF_South_Pole_Locations_(1590–2025).csv"
south_pole_data_raw <- read_table(
  file_path_south_pole,
  col_names = c("S_Lon_Raw", "S_Lat_Raw", "Year_Raw"),
  col_types = "ddd",
  show_col_types = FALSE
)

south_pole_processed_df <- south_pole_data_raw %>%
  mutate(
    Year = floor(as.numeric(Year_Raw)),
    S_Lat = as.numeric(S_Lat_Raw),
    S_Lon_Original = as.numeric(S_Lon_Raw),
    S_Lon = ifelse(S_Lon_Original > 180, S_Lon_Original - 360, S_Lon_Original)
  ) %>%
  select(Year, S_Lat, S_Lon, S_Lon_Original) %>%
  filter(!is.na(Year) & !is.na(S_Lat) & !is.na(S_Lon)) %>%
  arrange(Year)

south_pole_sf <- south_pole_processed_df %>%
  st_as_sf(coords = c("S_Lon", "S_Lat"), crs = 4326)

south_pole_path_sf <- south_pole_sf %>%
  arrange(Year) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING")

crs_south_pole_stereo <- paste0("+proj=laea +lat_0=-90 +lon_0=", 
                                round(median(south_pole_processed_df$S_Lon, na.rm=TRUE),0), 
                                " +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

p_igrf_south_map <- ggplot() +
  geom_sf(data = world_map, fill = "gray85", color = "white", linewidth = 0.1) +
  geom_sf(data = south_pole_path_sf, color = "dodgerblue3", linewidth = 1.2, alpha = 0.9) +
  geom_sf(data = south_pole_sf, aes(color = Year), size = 2.5, alpha = 0.9) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  coord_sf(crs = crs_south_pole_stereo, xlim = c(-2800000, 2800000), ylim = c(-2800000, 2800000)) +
  labs(title = "Drift of the Geomagnetic South Pole (IGRF Model)",
       subtitle = paste(min(south_pole_processed_df$Year, na.rm=TRUE), "-", max(south_pole_processed_df$Year, na.rm=TRUE)),
       color = "Year", x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.background = element_rect(fill = "lightcyan"),
        panel.grid.major = element_line(color = "gray70", linetype = "dashed", linewidth = 0.25))

ggsave("igrf_south_pole_wander_map.png", plot = p_igrf_south_map, width = 7, height = 7, dpi = 300)
ggsave(file.path(png_plots_dir, "igrf_south_pole_wander_map.png"), plot = p_igrf_south_map, width = 7, height = 7, dpi = 300)

label_interval_s <- ifelse(nrow(south_pole_sf) > 80, 50, 25)
years_to_label_s_igrf <- south_pole_sf %>% 
    filter(Year %in% seq(from = min(Year, na.rm=TRUE), to = max(Year, na.rm=TRUE), by = label_interval_s) | 
           Year == min(Year, na.rm=TRUE) | Year == max(Year, na.rm=TRUE))

p_igrf_south_map_labels <- p_igrf_south_map +
  geom_sf_text(data = years_to_label_s_igrf, aes(label = Year), 
               size = 2.2, color = "black", nudge_y = 180000, check_overlap = TRUE)

ggsave("igrf_south_pole_wander_map_labels.png", plot = p_igrf_south_map_labels, width = 7, height = 7, dpi = 300)
ggsave(file.path(png_plots_dir, "igrf_south_pole_wander_map_labels.png"), plot = p_igrf_south_map_labels, width = 7, height = 7, dpi = 300)

p_igrf_south_lat_time <- ggplot(south_pole_processed_df, aes(x = Year, y = S_Lat)) +
  geom_line(color = "dodgerblue3", linewidth = 1) + geom_point(color = "dodgerblue3", size = 1.5, alpha = 0.6) +
  labs(title = "Geomagnetic South Pole Latitude (IGRF)", x = "Year", y = "Latitude (°S)") + theme_minimal()
ggsave("igrf_south_pole_latitude_vs_time.png", plot = p_igrf_south_lat_time, width = 9, height = 5, dpi = 300)
ggsave(file.path(png_plots_dir, "igrf_south_pole_latitude_vs_time.png"), plot = p_igrf_south_lat_time, width = 9, height = 5, dpi = 300)

p_igrf_south_lon_time <- ggplot(south_pole_processed_df, aes(x = Year, y = S_Lon)) +
  geom_line(color = "darkblue", linewidth = 1) + geom_point(color = "darkblue", size = 1.5, alpha = 0.6) +
  labs(title = "Geomagnetic South Pole Longitude (IGRF, normalized)", x = "Year", y = "Longitude (°)") + theme_minimal()
ggsave("igrf_south_pole_longitude_vs_time.png", plot = p_igrf_south_lon_time, width = 9, height = 5, dpi = 300)
ggsave(file.path(png_plots_dir, "igrf_south_pole_longitude_vs_time.png"), plot = p_igrf_south_lon_time, width = 9, height = 5, dpi = 300)

if (requireNamespace("patchwork", quietly = TRUE)) {
  p_igrf_south_combined_ts <- p_igrf_south_lat_time / p_igrf_south_lon_time + plot_annotation(title = "Changes in Geomagnetic South Pole Position (IGRF)")
  ggsave("igrf_south_pole_lat_lon_vs_time_combined.png", plot = p_igrf_south_combined_ts, width = 9, height = 8, dpi = 300)
  ggsave(file.path(png_plots_dir, "igrf_south_pole_lat_lon_vs_time_combined.png"), plot = p_igrf_south_combined_ts, width = 9, height = 8, dpi = 300)
}

# --- Section 3: WMM North and South Pole Visualization (2020–2025) ---
cat("--- WMM North and South Pole Analysis (2020–2025) ---\n")

wmm_north_file <- "C:/Users/barte/OneDrive/Pulpit/R/NorthPoleLocations/WMM_North_Pole_Locations_(20202025).csv"
wmm_south_file <- "C:/Users/barte/OneDrive/Pulpit/R/NorthPoleLocations/WMM_South_Pole_Locations_(20202025).csv"

wmm_north_raw <- read_table(wmm_north_file, col_names = c("Lon", "Lat", "Year"), col_types = "ddd", show_col_types = FALSE)
wmm_south_raw <- read_table(wmm_south_file, col_names = c("Lon", "Lat", "Year"), col_types = "ddd", show_col_types = FALSE)

wmm_north_df <- wmm_north_raw %>%
  mutate(Lon = ifelse(Lon > 180, Lon - 360, Lon)) %>%
  arrange(Year)
wmm_south_df <- wmm_south_raw %>%
  mutate(Lon = ifelse(Lon > 180, Lon - 360, Lon)) %>%
  arrange(Year)

wmm_north_sf <- wmm_north_df %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
wmm_south_sf <- wmm_south_df %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326)

wmm_north_path <- wmm_north_sf %>% summarise(geometry = st_combine(geometry)) %>% st_cast("LINESTRING")
wmm_south_path <- wmm_south_sf %>% summarise(geometry = st_combine(geometry)) %>% st_cast("LINESTRING")

crs_north_wmm <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
crs_south_wmm <- "+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

p_wmm_north <- ggplot() +
  geom_sf(data = world_map, fill = "gray85", color = "white", linewidth = 0.1) +
  geom_sf(data = wmm_north_path, color = "red", linewidth = 1.2, alpha = 0.9) +
  geom_sf(data = wmm_north_sf, aes(color = Year), size = 3, alpha = 0.9) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  coord_sf(crs = crs_north_wmm, xlim = c(-2800000, 2800000), ylim = c(-2800000, 2800000)) +
  labs(title = "WMM Geomagnetic North Pole Drift (2020–2025)", color = "Year") +
  theme_minimal(base_size = 10)

ggsave("wmm_north_pole_wander_map.png", plot = p_wmm_north, width = 7, height = 7, dpi = 300)
ggsave(file.path(png_plots_dir, "wmm_north_pole_wander_map.png"), plot = p_wmm_north, width = 7, height = 7, dpi = 300)

p_wmm_south <- ggplot() +
  geom_sf(data = world_map, fill = "gray85", color = "white", linewidth = 0.1) +
  geom_sf(data = wmm_south_path, color = "blue", linewidth = 1.2, alpha = 0.9) +
  geom_sf(data = wmm_south_sf, aes(color = Year), size = 3, alpha = 0.9) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  coord_sf(crs = crs_south_wmm, xlim = c(-2800000, 2800000), ylim = c(-2800000, 2800000)) +
  labs(title = "WMM Geomagnetic South Pole Drift (2020–2025)", color = "Year") +
  theme_minimal(base_size = 10)

ggsave("wmm_south_pole_wander_map.png", plot = p_wmm_south, width = 7, height = 7, dpi = 300)
ggsave(file.path(png_plots_dir, "wmm_south_pole_wander_map.png"), plot = p_wmm_south, width = 7, height = 7, dpi = 300)

# --- Section 4: GRSM Declination Data Visualization ---
cat("--- GRSM Declination Data Analysis ---\n")

file_path_grsm <- "C:/Users/barte/OneDrive/Pulpit/R/archive (1)/Archive/GRSM_COMPASS_DECLINATION_5646692726181302872.csv"
grsm_data_raw <- read_csv(file_path_grsm, show_col_types = FALSE)

grsm_processed_df <- grsm_data_raw %>%
  mutate(
    LAT = as.numeric(LAT),
    LON = as.numeric(LON),
    DECLINATION = as.numeric(DECLINATION),
    ANNUAL_DRIFT = as.numeric(ANNUAL_DRIFT),
    INCLINATION = as.numeric(INCLINATION)
  ) %>%
  filter(!is.na(LAT) & !is.na(LON) & !is.na(DECLINATION))

grsm_sf <- grsm_processed_df %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326)

states_map <- ne_states(country = "United States of America", returnclass = "sf")
bbox_grsm <- st_bbox(grsm_sf)

p_grsm_decl_map <- ggplot() +
  geom_sf(data = states_map, fill = "gray90", color = "white") +
  geom_sf(data = grsm_sf, aes(color = DECLINATION), size = 2, alpha = 0.7) +
  scale_color_viridis_c(option = "cividis", name = "Declination (°)") +
  coord_sf(xlim = c(bbox_grsm["xmin"] - 0.1, bbox_grsm["xmax"] + 0.1), 
           ylim = c(bbox_grsm["ymin"] - 0.1, bbox_grsm["ymax"] + 0.1),
           datum = NA) +
  labs(title = "Magnetic Declination at GRSM Points",
       subtitle = "Point color indicates declination value",
       x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 10) +
  theme(panel.background = element_rect(fill = "ivory"))

ggsave("grsm_declination_map.png", plot = p_grsm_decl_map, width = 8, height = 7, dpi = 300)
ggsave(file.path(png_plots_dir, "grsm_declination_map.png"), plot = p_grsm_decl_map, width = 8, height = 7, dpi = 300)

p_grsm_decl_hist <- ggplot(grsm_processed_df, aes(x = DECLINATION)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.25, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "firebrick", linewidth = 1) +
  labs(title = "Distribution of Magnetic Declination (GRSM)",
       x = "Declination (°)", y = "Density") +
  theme_minimal()

ggsave("grsm_declination_histogram.png", plot = p_grsm_decl_hist, width = 7, height = 5, dpi = 300)
ggsave(file.path(png_plots_dir, "grsm_declination_histogram.png"), plot = p_grsm_decl_hist, width = 7, height = 5, dpi = 300)

p_grsm_decl_vs_lat <- ggplot(grsm_processed_df, aes(x = LAT, y = DECLINATION)) +
  geom_point(aes(color = LON), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "navy") +
  scale_color_viridis_c(option = "magma", name = "Longitude (°)") +
  labs(title = "Declination vs Latitude (GRSM)",
       x = "Latitude (°N)", y = "Declination (°)") +
  theme_minimal()

ggsave("grsm_declination_vs_latitude.png", plot = p_grsm_decl_vs_lat, width = 8, height = 6, dpi = 300)
ggsave(file.path(png_plots_dir, "grsm_declination_vs_latitude.png"), plot = p_grsm_decl_vs_lat, width = 8, height = 6, dpi = 300)

p_grsm_decl_vs_lon <- ggplot(grsm_processed_df, aes(x = LON, y = DECLINATION)) +
  geom_point(aes(color = LAT), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  scale_color_viridis_c(option = "viridis", name = "Latitude (°)") +
  labs(title = "Declination vs Longitude (GRSM)",
       x = "Longitude (°)", y = "Declination (°)") +
  theme_minimal()

ggsave("grsm_declination_vs_longitude.png", plot = p_grsm_decl_vs_lon, width = 8, height = 6, dpi = 300)
ggsave(file.path(png_plots_dir, "grsm_declination_vs_longitude.png"), plot = p_grsm_decl_vs_lon, width = 8, height = 6, dpi = 300)

if ("ANNUAL_DRIFT" %in% colnames(grsm_processed_df) && sum(!is.na(grsm_processed_df$ANNUAL_DRIFT)) > 0) {
  p_grsm_drift_hist <- ggplot(filter(grsm_processed_df, !is.na(ANNUAL_DRIFT)), aes(x = ANNUAL_DRIFT)) +
    geom_histogram(aes(y = ..density..), binwidth = 0.005, fill = "darkorange", color = "black", alpha = 0.7) +
    geom_density(color = "purple", linewidth = 1) +
    labs(title = "Distribution of Annual Declination Drift (GRSM)",
         x = "Annual Drift (°/year)", y = "Density") +
    theme_minimal()
  ggsave("grsm_annual_drift_histogram.png", plot = p_grsm_drift_hist, width = 7, height = 5, dpi = 300)
  ggsave(file.path(png_plots_dir, "grsm_annual_drift_histogram.png"), plot = p_grsm_drift_hist, width = 7, height = 5, dpi = 300)
}

cat("All planned visualizations have been generated and saved.\n")
cat("Output directory:\n")
cat(getwd(), "\n")
cat("All PNG plots are also saved in:\n")
cat(normalizePath(png_plots_dir), "\n")