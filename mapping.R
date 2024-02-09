# library.install("dplyr")
# library.install("sf")
# library.install("ggplot2")
# library.install("leaflet")
# library.install("htmlwidgets")
library(dplyr)
library(sf)
library(ggplot2)
library(leaflet)
library(htmlwidgets)

# Read data
ped_data <- read.csv("Data/2020_Person_Level_Crash_Details.csv")
rta_data <- read.csv("Data/RTA_Bus_Stops.csv")

# Process pedestrian data, reconfigure lat + long, filter for unique crash instances
ped_data_injury <- ped_data %>%
  filter(!NON_MTRST_TYPE_CL %in% "") %>%
  rename(
    "Latitude" = LAT,
    "Longitude" = LON
  ) %>%
  distinct(CRASH_NUMB, .keep_all = TRUE)


# Process bus stop data for WRTA
rta_data_worc <- rta_data %>%
  filter(Agency %in% "WRTA") %>%
  rename(
    "Latitude" = stop_lat,
    "Longitude" = stop_lon
  )

# Convert to sf objects
ped_sf <- st_as_sf(ped_data_injury, coords = c("Longitude", "Latitude"), crs = 4326)
rta_sf <- st_as_sf(rta_data_worc, coords = c("Longitude", "Latitude"), crs = 4326)

# Define bounding box around Worcester, MA
worcester_bbox <- st_bbox(st_sfc(st_point(c(-71.8023, 42.2626)), crs = 4326))

# Buffer bus stops by 50 feet + summarize total crashes
ped_crashes_near_bus_stops <- st_intersection(ped_sf, st_buffer(rta_sf, dist = 50)) %>%
  summarize(total_crashes = n())

rta_sf_buffered <- st_buffer(rta_sf, dist = 50)

# Buffer and summarize most dangerous stops
killer_stops <- st_intersection(ped_sf, st_buffer(rta_sf, dist = 50)) %>%
  group_by(stop_name) %>%
  summarize(total_crashes = n()) %>%
  rename(
    Stop = stop_name,
    Crashes = total_crashes
  ) %>%
  arrange(-Crashes) %>%
  filter(!Stop %in% "PARK AVE + CHANDLER") %>%
  head(20)

Dangerous_Stops <- killer_stops %>%
  st_drop_geometry()

# Display dangerous stops in a table
kable(Dangerous_Stops, format = "html", caption = "Dangerous Stops") %>%
  kable_styling(full_width = FALSE)

# Total crashes in each dataset
total_crashes_ped_data <- nrow(ped_sf)
total_crashes_rta_data <- nrow(rta_sf)

# Concentration ratio
concentration_ratio <- ped_crashes_near_bus_stops$total_crashes / total_crashes_ped_data

# Simplify geometries
rta_sf_buffered_simplified <- st_simplify(rta_sf_buffered, dTolerance = 0.005)
ped_sf_simplified <- st_simplify(ped_sf, dTolerance = 0.005)

# Create leaflet map focused on Worcester, MA
map <- leaflet() %>%
  setView(lng = -71.8023, lat = 42.2626, zoom = 12) %>%
  addTiles(group = "Base Map") %>%
  addPolygons(
    data = rta_sf_buffered_simplified,
    group = "Buffered Areas",
    fillColor = "lightblue",
    color = "blue",
    weight = 2,
    opacity = 0.5,
    fillOpacity = 0.3
  ) %>%
  addCircleMarkers(
    data = ped_sf_simplified,
    group = "Pedestrian Crashes",
    radius = 2,
    color = "red",
    fillOpacity = 0.8
  )

# Save the map as an HTML file — not working for some reason (?) — export map from print
# saveWidget(map, file = "map.html", selfcontained = TRUE)

# Print the map
print(map)

# Display results if wanted
cat("Total crashes within 50 feet of bus stops in ped_data_injury:", ped_crashes_near_bus_stops$total_crashes, "\n")
cat("Total crashes in ped_data_injury:", total_crashes_ped_data, "\n")
cat("Concentration ratio:", concentration_ratio, "\n")
