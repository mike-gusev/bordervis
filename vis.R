#building a leaflet of u.s./mexico county-level data
library(rgeos)
library(leaflet)
library(tidyverse)

#data imbuing
load("data/spatial/border_map.RData")
load("data/border_data.RData")
border_map_imbued <- border_map
border_map_imbued@data <- border_map@data %>% 
  left_join(border_data, by=c("NAME_1", "NAME_2")) %>%
  cbind(as.data.frame(t(sapply(slot(border_map_imbued, 'polygons'), 
                               function(i) slot(i, 'labpt'))))) %>%
  rename(lat=V1, lon=V2)

#check data
border_nulls <- border_map_imbued@data %>% map( ~sum(is.na(.x))) %>% unlist()
border_nulls[17:22]
border_map_imbued@data %>% filter(is.na(murder_2016) | is.na(murder_rate_2016)| is.na(population_2016) | is.na(pop_dens_sqkm_2016)) %>% dplyr::select(OBJECTID, NAME_1, NAME_2, area_sqkm_2010:poverty_rate_2015) %>% View()

#border shape
load("data/spatial/border_line.RData")

vars <- tribble(
  ~var, ~title, ~unit,
  "murder_rate_2016", "Murder Rate (2016)", "murders per 100,000 people",
  "pop_dens_sqkm_2016", "Population Density (2016)", "people per sq. km.",
  "gini_2010", "Gini Index (2010)", "",
  "poverty_rate_2015", "Poverty Rate (2015)", "% of pop. living in poverty"
)

my_popup <- function(ind) {
  paste0(
    "<b>", border_map_imbued@data$NAME_2, ", ", border_map_imbued@data$NAME_1, "</b>",
    "<br>", border_map_imbued@data[[ind]], " ", vars[vars$var==ind, "unit"]
  )
}


make_border_vis <- function() {
  border_vis <- leaflet(border_map_imbued) %>% addTiles()
  for (i in vars$var) {
    vals <- border_map_imbued@data[[i]]
    npal <- colorNumeric(domain = vals, palette="Greens", na.color="#A9A9A9")
    qpal <- colorQuantile(domain = vals, palette="Greens", na.color="#A9A9A9")
    if (i=="murder_rate_2016") border_vis <- border_vis %>% 
      addPolygons(fillColor=~npal(vals),
                  weight=1.5,
                  color="black",
                  group=paste0(vars[vars$var==i, "title"]),
                  popup=~my_popup(i),
                  fillOpacity=0.8) %>%
      addLegend(position="bottomright", 
                pal=npal, 
                values=vals, 
                na.label="no data",
                group=paste0(vars[vars$var==i, "title"]),
                title=paste0(vars[vars$var==i, "title"]))
    else border_vis <- border_vis %>% 
      addPolygons(fillColor=~qpal(vals),
                  weight=1.5, color="black",
                  group=paste0(vars[vars$var==i, "title"]),
                popup=~my_popup(i),
                fillOpacity=0.8) %>%
      addLegend(position="bottomleft",
                pal=qpal,
                values=vals, 
                group=paste0(vars[vars$var==i, "title"]),
                title=paste0(vars[vars$var==i, "title"]),
                na.label="no data",
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0(cuts[-n], "-", cuts[-1])
                })
  }
  border_vis <- border_vis %>% 
  addLayersControl(baseGroups=vars$title) %>%
  addPolylines(data=border_line, weight=3, color="black")
  border_vis
}
border_vis <- make_border_vis()

#save your daily thing
library(htmlwidgets)
save(border_vis, file="vis.html")