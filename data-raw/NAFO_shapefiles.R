## code to prepare `NAFO_shapefiles` dataset goes here

path <- file.path("../marea-esr-generator/data")
load(file.path(path, "NAFOSubunits_sf.rda"))

NAFO_shapefiles <- NAFOSubunits_sf

usethis::use_data(NAFO_shapefiles, overwrite = TRUE)
