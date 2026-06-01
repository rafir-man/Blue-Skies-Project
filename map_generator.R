library(leaflet)
library(dplyr)
library(htmlwidgets)

# 1. Load the new GRANULAR dataset
airports_raw <- read.csv("my_airports.csv")

# =========================================================================
# ALLIANCE MAINTENANCE 
# Note: With new database format, adding or removing airlines is 
# now as simple as adding or deleting their specific rows in the CSV file!
# =========================================================================

# 2. Dynamically Compress, Sort, and Format the Data for the Map
map_data <- airports_raw %>%
  arrange(desc(weekly_pax)) %>%  # <--- THIS IS THE NEW LINE
  group_by(iata, lat, lon) %>%
  summarize(
    total_pax = sum(weekly_pax),
    airline_count = n_distinct(airline),
    breakdown = paste(paste0("• <b>", airline, ":</b> ", format(weekly_pax, big.mark=",")), collapse = "<br>"),
    .groups = "drop"
  )

# 3. Define Map Palette
hub_palette <- colorNumeric(palette = "YlOrRd", domain = map_data$airline_count)

# 4. Generate the Map using the aggregated 'map_data'
blue_skies_map <- leaflet(data = map_data) %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Mode") %>% 
  addProviderTiles(providers$CartoDB.Positron, group = "Light Mode") %>% 
  addCircleMarkers(
    ~lon, ~lat,
    radius = ~(sqrt(total_pax) / 200) + 3, 
    color = ~hub_palette(airline_count),
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.8,
    popup = ~paste0(
      "<div style='font-family: Arial, sans-serif; color: #333;'>",
      "<b style='font-size:14px;'>", iata, "</b><br><hr style='margin:4px 0'>",
      "<div style='margin-bottom: 6px;'><b>Total Hub Pax:</b> ", format(total_pax, big.mark=","), "</div>",
      "<b>Airlines Present (", airline_count, "):</b><br>", breakdown,
      "</div>"
    )
  ) %>%
  addLayersControl(
    baseGroups = c("Dark Mode", "Light Mode"),
    options = layersControlOptions(collapsed = FALSE),
    position = "topright"
  ) %>%
  addControl(
    html = "<div style='background: rgba(255, 255, 255, 0.85); color: #222222; padding: 6px 10px; border-radius: 4px; font-family: Arial, sans-serif; font-size: 11px; box-shadow: 0 1px 3px rgba(0,0,0,0.2); border: 1px solid #ccc;'>
      <strong>Blue Skies Network:</strong> Includes hubs with &ge; 250k weekly passengers. Counts are approximate.
    </div>",
    position = "bottomleft"
  ) %>%
  addLegend(
    "bottomright", 
    pal = hub_palette, 
    values = ~airline_count,
    title = "Airline Count", 
    opacity = 1
  )

# 5. Export for GitHub
saveWidget(blue_skies_map, file = "index.html", selfcontained = TRUE)
blue_skies_map