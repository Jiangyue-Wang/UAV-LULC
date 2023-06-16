# load library
library(RSQLite)
library(exifr)
library(tidyverse)
library(basemaps)
library(sf)
library(ggmap)
library(readxl)
# parameter setting
filepath <- "/Volumes/LEOPARDS/SuliHabitat"
folders<-list.dirs(filepath) %>% as_tibble()%>% filter(str_detect(value,pattern = "-"))

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

write.csv(Suli_points, "field-data/field-data.csv", row.names = F)

st_write(st_as_sf(Suli_points[,3:5], coords = c("GPSLongitude", "GPSLatitude"))%>%st_set_crs(4326), "field-data/field-data.shp", append=F)
# ext <- sf::st_bbox(st_as_sf(Suli_points, coords = c("GPSLongitude", "GPSLatitude"))) %>% sf::st_set_crs(4326)
ext <- st_bbox(st_read("downloaded-datasets/Suli/suli.shp"))
names(ext) <- c("left", "bottom","right","top")

ggmap(get_map(ext, source = "stamen",maptype = "terrain")) + geom_point(data = Suli_points, aes(x = GPSLongitude, y = GPSLatitude, colour = Subject)) 


