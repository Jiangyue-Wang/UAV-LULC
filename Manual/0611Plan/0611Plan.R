filepath <- "Manual/0611Plan/"
file <- dir(filepath)

for(i in 1:length(file)){
  assign(paste0("pol",i),sf::st_read(paste0(filepath, file[i])))
}

pol0611 <- dplyr::bind_rows(pol1, pol2, pol3, pol4, pol5, pol6, pol7, pol8)
pol0611 <- sf::st_zm(pol0611, drop=T, what="ZM")
sf::write_sf(pol0611, "Manual/0611plan/0611plan.kml")

table(unlist(stringr::str_split(pol0611$Description, pattern = "-")))
