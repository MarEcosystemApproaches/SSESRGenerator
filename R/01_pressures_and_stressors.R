# Template function for Pressures and Stressors Section

# internal variable template functions ------------------------------------
.nao_template <- function(.region){

  not_blank <-
    c(
      '### North Atlantic Oscillation (NAO); global scale',
      '',
      '[Hebert et al. 2024](https://publications.gc.ca/collections/collection_2024/mpo-dfo/Fs97-18-380-eng.pdf) (see Section 2.1)',
      '',
      '```{r plot-nao, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap="The North Atlantic Oscillation (NAO) index, defined as the winter (December, January, February, March) 500 mb pressure Principal Component Analysis which is representative of the difference between the Icelandic low and Azores high. "}',
      '',
      'data("nao")',
      'plot(nao, style = "default") + theme_light(base_size = 8)',
      '',
      '```',
      '',
      '<!-- add interpretation here -->',
      '',
      '',
      '\\newpage',
      '',
      ''
    )

  blank <-
    c(
      '### North Atlantic Oscillation (NAO); global scale',
      '',
      '[Hebert et al. 2024](https://publications.gc.ca/collections/collection_2024/mpo-dfo/Fs97-18-380-eng.pdf) (see Section 2.1)',
      '',
      '```{r plot-nao, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap="The North Atlantic Oscillation (NAO) index, defined as the winter (December, January, February, March) 500 mb pressure Principal Component Analysis which is representative of the difference between the Icelandic low and Azores high. "}',
      '',
      '# add NAO data, if available',
      '',
      '```',
      '',
      '<!-- add interpretation here -->',
      '',
      '',
      '\\newpage',
      '',
      ''
    )

  if(.region == "blank"){
    out <- blank
  } else {
    out <- not_blank
  }
  return(out)

}

.amo_template <- function(.region){

  not_blank <-
    c(
      '### Atlantic Multidecadal Oscillation (AMO); global scale',
      '',
      'The AMO describes long-duration changes in the sea surface temperature of the North Atlantic Ocean, with cool and warm phases that may last for 20-40 years.',
      '',
      '```{r plot-amo, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap="The Atlantic Multidecadal Oscillation (AMO) index"}',
      '',
      'data("amo")',
      'plot(amo, style = "anomaly") + theme_light(base_size = 8)',
      '',
      '```',
      '',
      '<!-- add interpretation here -->',
      '',
      '',
      '\\newpage',
      '',
      ''
    )

  blank <-
    c(
      '### Atlantic Multidecadal Oscillation (AMO); global scale',
      '',
      'The AMO describes long-duration changes in the sea surface temperature of the North Atlantic Ocean, with cool and warm phases that may last for 20-40 years.',
      '',
      '```{r plot-amo, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap="The Atlantic Multidecadal Oscillation (AMO) index"}',
      '',
      '# add AMO data, if available',
      '',
      '```',
      '',
      '<!-- add interpretation here -->',
      '',
      '',
      '\\newpage',
      '',
      ''
    )


  if(.region == "blank"){
    out <- blank
  } else {
    out <- not_blank
  }
  return(out)

}


.azmp_satellite_temperature_template <- function(.region){

  .region_code <- switch(.region,
                         "4X" = 'c("4XSS")',
                         "4W" = 'c("4W")',
                         "4V" = 'c("4Vn", "4Vs")',
                         "ESS" = 'c("4Vn", "4Vs", "4W")',
                         "WSS" = 'c("4XSS")'
  )

  .region_code_text <- .region_code %>% str_remove_all('"') %>% str_remove("c") %>%
    str_remove("\\(") %>%
    str_remove("\\)")

  not_blank <-
    c(
      glue::glue('### AZMP Surface Temperature from Satellite Data; local scale {.region_code_text}'),
      '',
      '[Casault et al. 2020](https://casaultb.github.io/azmpdata/)',
      '',
      sprintf('```{r plot-azmp-satellite-temperature, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap="Sea surface temperature from satellite data for region %s"}',.region),
      '',
      '# NOTE: azmp_satellite_temperature regions include:',
      '# 4Vn, 4Vs, 4W, 4XeGoM+BoF, 4XeGoMBoF, 4XSS ',
      '',
      '# load temperature data',
      'data("azmp_satellite_temperature")',
      '',
      '# subset to region',
      glue::glue("azmp_satellite_temperature_region <-
         ea.subset(azmp_satellite_temperature, 'region', {.region_code})"),
      "rm(azmp_satellite_temperature)",
      glue::glue('azmp_satellite_temperature_region@meta$region <- "{.region_code_text}"'),
      '',
      '# plot',
      'plot(azmp_satellite_temperature_region, style = "default") + theme_light(base_size = 8)',
      '',
      '```',
      '',
      '<!-- add interpretation here -->',
      '',
      '',
      '\\newpage',
      '',
      ''
    )

  blank <-
    c(
      '### AZMP Surface Temperature from Satellite Data',
      '',
      '[Casault et al. 2020](https://casaultb.github.io/azmpdata/)',
      '',
      '```{r plot-azmp-satellite-temperature, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap="Sea surface temperature from satellite data"}',
      '',
      '# add AZMP surface temperature data, if available',
      '',
      '```',
      '',
      '<!-- add interpretation here -->',
      '',
      '',
      '\\newpage',
      '',
      ''
    )

  if(.region == "blank"){
    out <- blank
  } else {
    out <- not_blank
  }
  return(out)

}


.azmp_surface_temperature_template <- function(.region){

  not_blank <-
    c(
      '### AZMP Surface Temperature; local scale (Scotian Shelf Box)',
      '',
      '[Casault et al. 2020](https://casaultb.github.io/azmpdata/)',
      '',
      '```{r plot-azmp-surface-temperature, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap="Surface temperature in the Scotian Shelf Region."}',
      '',
      'data("azmp_surface_temperature")',
      'plot(azmp_surface_temperature, style = "default") + theme_light(base_size = 8)',
      '',
      '```',
      '',
      '<!-- add interpretation here -->',
      '',
      '',
      '\\newpage',
      '',
      ''
    )

  blank <-
    c(
      '### AZMP Surface Temperature; local scale (Scotian Shelf Box)',
      '',
      '[Casault et al. 2020](https://casaultb.github.io/azmpdata/)',
      '',
      '```{r plot-azmp-surface-temperature, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap="Surface temperature in the Scotian Shelf Region."}',
      '',
      '# add surface temperature data, if available',
      '',
      '```',
      '',
      '<!-- add interpretation here -->',
      '',
      '',
      '\\newpage',
      '',
      ''
    )

  if(.region == "blank"){
    out <- blank
  } else {
    out <- not_blank
  }
  return(out)


}


.azmp_bottom_temperature_template <- function(.region){

  .region_code <- switch(.region,
                         "4X" = 'c("4X")',
                         "4W" = 'c("4W")',
                         "4V" = 'c("4Vn", "4Vs")',
                         "ESS" = 'c("4Vn", "4Vs", "4W")',
                         "WSS" = 'c("4X")'
  )

  .region_code_text <- .region_code %>% str_remove_all('"') %>% str_remove("c") %>%
    str_remove("\\(") %>%
    str_remove("\\)")

  not_blank <- c(
    glue::glue('### AZMP Sea Bottom Temperature; local scale {.region_code_text}'),
    '',
    '[Casault et al. 2020](https://casaultb.github.io/azmpdata/)',
    '',
    sprintf('```{r plot-azmp-bottom-temperature, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap="Sea bottom temperature for region %s"}',.region),
    '',
    '# NOTE: azmp_bottom_temperature regions include:',
    '# Cabot Strait, E Georges Bank, Emerald Basin, Georges Basin, Lurcher Shoal, Misaine Bank, 4Vn, 4Vs, 4W, 4XeGoMBoF, 4XSS, 4X ',
    '',
    '# load data',
    'data("azmp_bottom_temperature")',
    '',
    '# subset to region',
    glue::glue("azmp_bottom_temperature_region <-
         ea.subset(azmp_bottom_temperature, 'region', {.region_code})"),
    "rm(azmp_bottom_temperature)",
    glue::glue('azmp_bottom_temperature_region@meta$region <- "{.region_code_text}"'),
    '',
    '# plot',
    'plot(azmp_bottom_temperature_region, style = "default") + theme_light(base_size = 8)',
    '',
    '```',
    '',
    '<!-- add interpretation here -->',
    '',
    '',
    '\\newpage',
    '',
    ''
  )

  blank <- c(
    '### AZMP Sea Bottom Temperature',
    '',
    '[Casault et al. 2020](https://casaultb.github.io/azmpdata/)',
    '',
    '```{r plot-azmp-bottom-temperature, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap="Sea bottom temperature"}',
    '',
    '# add bottom temperature data, if available',
    '',
    '```',
    '',
    '<!-- add interpretation here -->',
    '',
    '',
    '\\newpage',
    '',
    ''
  )

  if(.region == "blank"){
    out <- blank
  } else {
    out <- not_blank
  }
  return(out)

}

.azmp_stratification_template <- function(.region){

  not_blank <- c(
    '### AZMP Stratification; local scale (Scotian Shelf Box)',
    '',
    '[Layton et al. 2025](https://publications.gc.ca/collections/collection_2025/mpo-dfo/Fs97-18-403-eng.pdf)',
    '',
    '```{r plot-azmp-stratification, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap="Stratification in the Scotian Shelf Region."}',
    '',
    'data("azmp_stratification")',
    'plot(azmp_stratification, style = "default") + theme_light(base_size = 8)',
    '',
    '```',
    '',
    '<!-- add interpretation here -->',
    '',
    '',
    '\\newpage',
    '',
    ''
  )

  blank <-  c(
    '### AZMP Stratification; local scale (Scotian Shelf Box)',
    '',
    '[Layton et al. 2025](https://publications.gc.ca/collections/collection_2025/mpo-dfo/Fs97-18-403-eng.pdf)',
    '',
    '```{r plot-azmp-stratification, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap="Stratification in the Scotian Shelf Region."}',
    '',
    '# add Stratification data, if available',
    '',
    '```',
    '',
    '<!-- add interpretation here -->',
    '',
    '',
    '\\newpage',
    '',
    ''
  )

  if(.region == "blank"){
    out <- blank
  } else {
    out <- not_blank
  }
  return(out)

}

.azmp_salinity_template <- function(.region){

  not_blank <- c(
    '### AZMP Salinity; local scale (Scotian Shelf Box)',
    '',
    '[Layton et al. 2025](https://publications.gc.ca/collections/collection_2025/mpo-dfo/Fs97-18-403-eng.pdf)',
    '',
    '```{r plot-azmp-salinity, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap="Salinity in the Scotian Shelf Region."}',
    '',
    'data("azmp_salinity")',
    'plot(azmp_salinity, style = "default") + theme_light(base_size = 8)',
    '',
    '```',
    '',
    '<!-- add interpretation here -->',
    '',
    '',
    '\\newpage',
    '',
    ''
  )

  blank <- c(
    '### AZMP Salinity; local scale (Scotian Shelf Box)',
    '',
    '[Layton et al. 2025](https://publications.gc.ca/collections/collection_2025/mpo-dfo/Fs97-18-403-eng.pdf)',
    '',
    '```{r plot-azmp-salinity, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap="Salinity in the Scotian Shelf Region."}',
    '',
    '# add salinty data, if available',
    '',
    '```',
    '',
    '<!-- add interpretation here -->',
    '',
    '',
    '\\newpage',
    '',
    ''
  )

  if(.region == "blank"){
    out <- blank
  } else {
    out <- not_blank
  }
  return(out)

}


.ocean_acidification_template <- function(.region){

  not_blank <-
    c(
      '### Ocean Acifification',
      '',
      "Template data not available.",
      "",
      '```{r plot-ocean-acidification, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add ocean acidifcation data, if available.',
      '',
      '```',
      '',
      '<!-- add interpretation here -->',
      '',
      '',
      '\\newpage',
      '',
      ''
    )

  blank <-
    c(
      '### Ocean Acifification',
      '',
      "",
      '```{r plot-ocean-acidification, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add ocean acidifcation data, if available.',
      '',
      '```',
      '',
      '<!-- add interpretation here -->',
      '',
      '',
      '\\newpage',
      '',
      ''
    )

  if(.region == "blank"){
    out <- blank
  } else {
    out <- not_blank
  }
  return(out)


}


.future_projections_template <- function(.region){

  not_blank <-
    c(
      "### Future Projections and Model Outputs",
      "",
      "Template data not available.",
      "",
      '```{r plot-future-projections, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add future projections data, if available.',
      '',
      '```',
      '',
      '',
      '\\newpage',
      '',
      ''
    )

  blank <-
    c(
      "### Future Projections and Model Outputs",
      "",
      '```{r plot-future-projections, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add future projections data, if available.',
      '',
      '```',
      '',
      '',
      '\\newpage',
      '',
      ''
    )

  if(.region == "blank"){
    out <- blank
  } else {
    out <- not_blank
  }
  return(out)

}

# main pressures and stressures function -----------------------------------
#' Add Pressures and Stressors section to Rmd report template
#'
#' @param .region Target region of report. Inherited from `build_esr_template()`.
#' @param .ps_vars List of Pressures and Stressors variables to include. Inherited from `build_esr_template()`.
#'
#' @returns R markdown chunk for pressures and stressors section.
#' @export
pressures_and_stressors <- function(.region, .ps_vars){


  general_intro <- c( "# A. Pressures and Stressors {#pressuresandstressors}","")

  climate_oceanography_header <- c("## Climate and Oceanography",
                                   "",
                                   "The management area is strongly influenced by natural variability of the climate system (e.g., Atlantic Multidecadal Oscillation, North Atlantic Oscillation) and, therefore, long-term monitoring is needed to determine how anthropogenic climate change is affecting ocean temperatures and other ocean properties. Changes in climate can directly and indirectly impact stock status by affecting the distribution, seasonal timing, physiology, migration and spawning of marine species.",
                                   "")

  climate_oceanography_body <- c(
    if("amo" %in% .ps_vars) .amo_template(.region),
    if("nao" %in% .ps_vars) .nao_template(.region),
    if("azmp_satellite_temperature" %in% .ps_vars) .azmp_satellite_temperature_template(.region),
    if("azmp_surface_temperature" %in% .ps_vars) .azmp_surface_temperature_template(.region),
    if("azmp_bottom_temperature" %in% .ps_vars) .azmp_bottom_temperature_template(.region),
    if("azmp_stratification" %in% .ps_vars) .azmp_stratification_template(.region),
    if("azmp_salinity" %in% .ps_vars) .azmp_salinity_template(.region),
    if("ocean_acidification" %in% .ps_vars) .ocean_acidification_template(.region)
  )

  long_term_climate_change_header <- c("## Longer-Term Climate Change","")

  long_term_climate_change_body <- c(
    if("future_projections" %in% .ps_vars) .future_projections_template(.region)
  )

  out <- c(general_intro,
           climate_oceanography_header,
           climate_oceanography_body,
           long_term_climate_change_header,
           long_term_climate_change_body)

  # make sure out ends with newpage, otherwise, add it.
  # (this occurs when not all ps_vars are selected)
  if(!out[length(out)-2] == "\\newpage"){
    out <- c(out,"\\newpage","","")
  }

  return(out)


}
