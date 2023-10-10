# load library
library(RSQLite)
library(exifr)
library(tidyverse)
library(basemaps)
library(sf)
library(ggmap)
library(readxl)
library(ggsn)
# parameter setting
filepath <- "/Volumes/Jackal/SuliHabitat"
folders<-list.dirs(filepath) %>% as_tibble()%>% filter(str_detect(value,pattern = "-")|str_detect(value,pattern = "D"))

# record table
recordTable <- data.frame()
for(i in 1:nrow(folders)){
  tmp <- read_exif(paste0(folders$value[i],"/",dir(folders$value[i]))) %>% filter(FileType !="MP4") %>%filter(FileType !="DNG") %>% select(SourceFile,DateTimeOriginal, Subject, GPSLatitude, GPSLongitude)
  recordTable <- bind_rows(recordTable, tmp)
}
# saveRDS(recordTable, "intermediate_rds/recordTable_tmp.rds")
record_add <- read_excel("~/Desktop/phD/Field-work/Ghahe/Habitat-point-add.xlsx") %>% mutate(DateTimeOriginal = lubridate::as_datetime(DateTimeOriginal))


recordTable %>% mutate(DateTimeOriginal = lubridate::as_datetime(DateTimeOriginal)) %>%filter(Subject%in%c("Bareland", "Grassland", "Jiji Grass", "Seabuckthorn", "Shrubland"))%>%bind_rows(record_add) %>% select(Subject)%>%table()

recordTable %>% mutate(DateTimeOriginal = lubridate::as_datetime(DateTimeOriginal)) %>%filter(Subject%in%c("Bareland", "Grassland", "Jiji Grass", "Seabuckthorn", "Shrubland"))%>%bind_rows(record_add) %>% nrow()

Suli_points <- recordTable %>% mutate(DateTimeOriginal = lubridate::as_datetime(DateTimeOriginal)) %>%filter(Subject%in%c("Bareland", "Grassland", "Jiji Grass", "Seabuckthorn", "Shrubland"))%>%bind_rows(record_add) %>%mutate(Subject = as.factor(Subject))

# write.csv(Suli_points, "field-data/field-data.csv", row.names = F)
Suli_points <- read.csv("field-data/field-data.csv")
## read snow polygons
snowdir <- "field-data/Snow/"
snowfile <- dir(snowdir)
for(i in 1:length(snowfile)){
  tmp <- read_sf(paste0(snowdir, snowfile[i]))
  tmp <- st_zm(tmp, drop = T, what = "ZM")
  assign(paste0("Snow",i), tmp)
}
snow <- st_union(bind_rows(Snow1,Snow2,Snow3,Snow4,Snow5,Snow6,Snow7,Snow8,Snow9,Snow10))
points_num <- round(nrow(Suli_points)/length(unique(Suli_points$Subject)),-1)
set.seed(321)
snow_points <- st_sample(snow, size = points_num) %>% unlist() %>% matrix(ncol=2, byrow = T)

snow_rows <- data.frame(SourceFile = "GoogleEarth", DateTimeOriginal = NA, Subject = "Snow", GPSLatitude = snow_points[,2], GPSLongitude = snow_points[,1])

Suli_points <- bind_rows(Suli_points, snow_rows)
## write shapefile
st_write(st_as_sf(Suli_points[,3:5], coords = c("GPSLongitude", "GPSLatitude"))%>%st_set_crs(4326), "field-data/field-data.shp", append=F)
# ext <- sf::st_bbox(st_as_sf(Suli_points, coords = c("GPSLongitude", "GPSLatitude"))) %>% sf::st_set_crs(4326)
ext <- st_bbox(st_read("field-data/field-data.shp"))
names(ext) <- c("left", "bottom","right","top")
ext[c(1,2)] <- floor(ext[c(1,2)]*10)/10
ext[c(3,4)] <- ceiling(ext[c(3,4)]*10)/10

ggmap(get_map(ext, source = "stamen",maptype = "terrain")) + geom_point(data = Suli_points, aes(x = GPSLongitude, y = GPSLatitude, colour = Subject), size = 1, alpha = 0.3) + theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))+scalebar(x.min = 97.1, x.max = 98.5, y.min = 38.2, y.max = 39,dist = 20, dist_unit = "km",transform = TRUE, model = "WGS84", location = "bottomleft", st.bottom = F, st.size = 2, border.size = 0.3, anchor = c(x = 97.15, y = 38.33))
ggsave("figures/field-sampling.pdf", width = 6, height = 4, dpi = 600)
