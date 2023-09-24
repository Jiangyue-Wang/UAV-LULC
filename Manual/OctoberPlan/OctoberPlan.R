### load library----
library(tidyverse)
library(sf)
library(terra)
library(basemaps)
library(ggmap)
library(ggsn)
### read data----
directory <- "Manual/OctoberPlan/"
points <- dir(directory) %>% 
  as_tibble() %>% 
  rename(filename = value) %>% 
  filter(str_detect(string = filename, pattern = "kml")) %>%
  filter(str_detect(string = filename, pattern = "F"))%>% 
  rowwise() %>% 
  mutate(filename = paste0(directory, filename))

schedule_points <- c()
for( i in 1:nrow(points)){
  tmp_p <- st_read(points$filename[i])
  
  schedule_points <- bind_rows(schedule_points, tmp_p)
  rm(tmp_p)
  gc()
}

### make figure----
schedule_points[,c("x","y")] <- st_coordinates(schedule_points)[,c("X","Y")]
schedule_points[,"Days"] <- str_sub(schedule_points$Name,start = 2, end = 2)

ext <- st_bbox(schedule_points)
names(ext) <- c("left", "bottom","right","top")
ext[c(1,2)] <- floor(ext[c(1,2)]*10)/10
ext[c(3,4)] <- ceiling(ext[c(3,4)]*10)/10

ggmap(get_map(ext, source = "stamen",maptype = "terrain")) + geom_point(data = schedule_points, aes(x = x, y = y, colour = Days), size = 1) + theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))+scalebar(x.min = 97.2, x.max = 98.4, y.min = 38.2, y.max = 39,dist = 20, dist_unit = "km",transform = TRUE, model = "WGS84", location = "bottomleft", st.bottom = F, st.size = 2, border.size = 0.3, anchor = c(x = 97.25, y = 38.25)) + xlab("Longitude") + ylab("Latitude")
ggsave("Manual/OctoberPlan/OctoberPlan.png", width = 6, height = 4, dpi = 600)

st_write(schedule_points, "Manual/OctoberPlan/OctoberPlan.kml")
