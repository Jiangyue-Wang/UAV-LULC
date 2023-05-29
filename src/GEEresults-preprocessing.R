### load library-------
library(sf)
library(terra)
library(tidyverse)
library(tidyterra)
library(googledrive)

### parameter setting
drive_auth_configure(api_key = "AIzaSyDW3lUI-g29R8KjhZInaKb44Sx6flrtlCw")
drive_deauth()
Sys.setenv(http_proxy = "http://192.168.2.108:4780/")
Sys.setenv(https_proxy = "https://192.168.2.108:4780/")
### download data from google drive--------
filelist <- paste0("~/Month",rep(str_pad(1:12, width = 2, side = "left", pad = "0"),each = 6),"/","myExportImageTask-00000",rep(c("00000","06912"), each = 36),"-00000",rep(c("00000","06912","13824"),24),".tif")

outfile <- paste0("cloudfree-10year-composite/Month",rep(str_pad(1:12, width = 2, side = "left", pad = "0"),each = 6), "-",rep(c(1:6),12))

for(i in 1:length(filelist)){
  drive_download(
    filelist[i],
    path = outfile[i],
    type = "tif",
    overwrite = TRUE
  )
}
## Error in curl::curl_fetch_memory(url, handle = handle) : 
## Failed to connect to www.googleapis.com port 443: Timed out
## fail fail fail fuck I downloaded by hand

### save download monthly file directory
downloadedlist <- data.frame(month = rep(1:12,each=6),filedir = paste0("cloudfree-10year-composite/Month",rep(str_pad(1:12, width = 2, side = "left", pad = "0"),each = 6),"/","myExportImageTask-00000",rep(c("00000","06912"),36),"-00000",rep(c("00000","06912","13824"),24),".tif"))
write_rds(downloadedlist, "intermediate_rds/cloudfree-10year-composite_filelist.rds")
