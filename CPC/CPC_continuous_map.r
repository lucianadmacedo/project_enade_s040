rm (list = ls())

# --- 1. Load Libraries ---
# Make sure to install these packages if you haven't already:
# install.packages(c("readxl", "dplyr", "sf", "geobr", "leaflet", "htmltools"))
library(readxl)
library(dplyr)
library(sf)
library(geobr)
library(leaflet)
library(htmltools)

# --- 2. Load and Prepare Your CPC Data ---
cpc_data <- read_excel("CPC_2023.xlsx", sheet = "CPC_2023")

cpc_by_city <- cpc_data %>%
  select(
    cpc_continuous = `CPC (Contínuo)`,
    city_code = `Código do Município`, 
    city_name = `Município do Curso`,
    state_acronym = `Sigla da UF`
  ) %>%
  filter(!is.na(cpc_continuous) & cpc_continuous != "" & !is.na(city_code)) %>%
  group_by(city_code, city_name, state_acronym) %>%
  summarise(
    avg_cpc = mean(cpc_continuous, na.rm = TRUE),
    course_count = n()
  ) %>%
  ungroup()

# --- 3. Get Brazil Geographic Data ---
all_mun <- read_municipality(year = 2020)

# --- 4. Join Your Data with the Geographic Data ---
map_data <- all_mun %>%
  inner_join(cpc_by_city, by = c("code_muni" = "city_code"))

# --- 5. *** NEW STEP ADDED: Calculate Center Points (Longitude & Latitude) *** ---
# This is the crucial step that was missing.
# It finds the center of each city's shape and creates 'lon' and 'lat' columns.
map_data <- map_data %>%
  mutate(lon = st_coordinates(st_centroid(geom))[,1],
         lat = st_coordinates(st_centroid(geom))[,2])

# --- 6. Create the Interactive Map using Leaflet Circle Markers ---
cpc_palette <- colorNumeric(
  palette = "YlOrRd",
  domain = map_data$avg_cpc,
  na.color = "#E0E0E0"
)

map_labels <- paste0(
  "<strong>", map_data$city_name, ", ", map_data$state_acronym, "</strong><br/>",
  "Average CPC Score: ", round(map_data$avg_cpc, 2), "<br/>",
  "Number of Courses in Dataset: ", format(map_data$course_count, big.mark = ",")
) %>% lapply(HTML)

# Build the final map object
interactive_map_city <- leaflet(map_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -55, lat = -14, zoom = 4) %>%
  addCircleMarkers(
    lng = ~lon, # *** CORRECTED: Uses the new 'lon' column
    lat = ~lat, # *** CORRECTED: Uses the new 'lat' column
    color = ~cpc_palette(avg_cpc),
    radius = ~sqrt(course_count) + 2,
    stroke = FALSE, 
    fillOpacity = 0.7,
    popup = map_labels
  ) %>%
  addLegend(
    pal = cpc_palette,
    values = ~avg_cpc,
    opacity = 0.7,
    title = "Average CPC Score",
    position = "bottomright"
  )

# Display the map
interactive_map_city
