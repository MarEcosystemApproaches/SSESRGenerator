## code to prepare `objectives` dataset goes here

path <- file.path("../marea-esr-generator/data/objectives")
load(file.path(path, "NAFO4Xesr_objectives.Rdata"))

ESR_objectives <- NAFO4Xesr_objectives

usethis::use_data(ESR_objectives, overwrite = TRUE)
