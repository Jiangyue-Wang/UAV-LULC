filepath <- "Manual/0612Plan/"
file <- dir(filepath)

for(i in 1:length(file)){
  assign(paste0("pol",i),sf::st_read(paste0(filepath, file[i])))
}

pol0612 <- dplyr::bind_rows(pol1, pol2, pol3, pol4, pol5)
pol0612 <- sf::st_zm(pol0612, drop=T, what="ZM")
sf::write_sf(pol0612, "Manual/0612plan/0612plan.kml")

table(unlist(stringr::str_split(pol0612$Description, pattern = "-")))
