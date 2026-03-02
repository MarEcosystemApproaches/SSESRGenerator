# Template function for Ecological Indicators Section

# internal variable template functions ------------------------------------
.commercial_fishing_template <- function(.region){

  .region_code <- switch(.region,
                         "4X" = 'c("4X")',
                         "4W" = 'c("4W")',
                         "4V" = 'c("4VS", "4VN")',
                         "4VS" = 'c("4VS")',
                         "4VN" = 'c("4VN")',
                         "ESS" = 'c("ESS")',
                         "WSS" = 'c("WSS")'
  )

  not_blank <- c(
    glue::glue('### Fisheries Landings; local scale {.region_code}'),
    '',
    'Commercial fishing is an activity that impacts the ecology of a management area through removals of fish from the ecosystem',
    '',
    sprintf('```{r plot-commercial-fishing, fig.align="center", fig.pos = "H", fig.width = 7, fig.height=4.5,fig.cap="Commercial fisheries landings for target groups in region %s"}',.region),
    '',
    '',
    'data("eco_indicators")',
    '# subset to target region',
    glue::glue('eco_indicators_region <- ea.subset(eco_indicators,"region",{.region_code})'),
    'rm(eco_indicators)',
    '',
    'plot_commercial_fishing <- function(fishery){',
    '  ',
    '  ea_data_fishery <- ea_data(',
    '    data = eco_indicators_region[["data"]][c("year",paste0("landings_",fishery,".L_value"))],',
    '    value_col =  paste0("landings_",fishery,".L_value"),',
    '    data_type =  paste0(str_to_title(fishery), " Landings"),',
    sprintf('    region = "%s",',.region),
    sprintf('    location_descriptor = "%s NAFO management region",',.region),
    '    units = "tonnes",',
    '    source_citation = eco_indicators_region@meta$source_citation',
    '  )',
    '  ',
    '  ea_fishery_plot <- plot(ea_data_fishery, style = "histogram")',
    '  return(ea_fishery_plot)',
    '  ',
    '}',
    '',
    '# make fisheries plot for each group',
    'fishing_plot_clupeids <- plot_commercial_fishing("CLUPEIDS")',
    'fishing_plot_finfish <- plot_commercial_fishing("FINFISH")',
    'fishing_plot_flatfish  <- plot_commercial_fishing("FLATFISH")',
    'fishing_plot_forage <- plot_commercial_fishing("FORAGE")',
    'fishing_plot_gadoids  <- plot_commercial_fishing("GADOIDS")',
    'fishing_plot_groundfish <- plot_commercial_fishing("GROUNDFISH")',
    '# Note that data for other groups may be available.',
    '# Check eco_indicators columns in marea.',
    '',
    '# merge with patchwork',
    'fishing_plot_full <- ',
    '  (fishing_plot_clupeids + ',
    '     fishing_plot_finfish + ',
    '     fishing_plot_flatfish + ',
    '     fishing_plot_forage + ',
    '     fishing_plot_gadoids + ',
    '     fishing_plot_groundfish) &',
    '  theme_light(base_size = 9) ',
    '',
    'fishing_plot_full',
    '',
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
      '### Fisheries Landings',
      '',
      'Commercial fishing is an activity that impacts the ecology of a management area through removals of fish from the ecosystem',
      '',
      '```{r plot-commercial-fishing, fig.align="center", fig.pos = "H", fig.width = 7, fig.height=4.5,fig.cap="Commercial fisheries landings for target groups"}',
      '',
      '# insert commercial fishing data here, if available.',
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


.fishing_pressure_template <- function(.region){

  .region_code <- switch(.region,
                         "4X" = 'c("4X")',
                         "4W" = 'c("4W")',
                         "4V" = 'c("4VS", "4VN")',
                         "4VS" = 'c("4VS")',
                         "4VN" = 'c("4VN")',
                         "ESS" = 'c("ESS")',
                         "WSS" = 'c("WSS")'
  )

  not_blank <-
    c(
      glue::glue('### Fisheries Pressure; local scale {.region_code}'),
      '',
      'This indicator measures the level of exploitation or total fishing pressure at the ecosystem level (Landings/Biomass). Change in this indicator can result from change in biomass, landings or both.',
      '',
      sprintf('```{r plot-fishing-pressure, fig.align="center", fig.pos = "H", fig.width = 7, fig.height=4.5,fig.cap="Fishing pressure for target groups in region %s"}',.region),
      '',
      '',
      'data("eco_indicators")',
      '# subset to target region',
      glue::glue('eco_indicators_region <- ea.subset(eco_indicators,"region",{.region_code})'),
      'rm(eco_indicators)',
      '',
      'plot_fishing_pressure <- function(fishery){',
      '  ',
      '  ea_data_fp <- ea_data(',
      '    data = eco_indicators_region[["data"]][c("year",paste0("FP_",fishery,"_value"))],',
      '    value_col =  paste0("FP_",fishery,"_value"),',
      '    data_type = paste0(str_to_title(fishery), " FP"),',
      sprintf('    region = "%s",',.region),
      sprintf('    location_descriptor = "%s NAFO management region",',.region),
      '    units = "",',
      '    source_citation = eco_indicators_region@meta$source_citation',
      '  )',
      '  ',
      '  ea_fp_plot <- plot(ea_data_fp, style = "plain")',
      '  return(ea_fp_plot)',
      '  ',
      '}',
      '',
      '# make fisheries plot for each group',
      'fp_plot_clupeids <- plot_fishing_pressure("CLUPEIDS")',
      'fp_plot_finfish <- plot_fishing_pressure("FINFISH")',
      'fp_plot_flatfish  <- plot_fishing_pressure("FLATFISH")',
      'fp_plot_forage <- plot_fishing_pressure("FORAGE")',
      'fp_plot_gadoids  <- plot_fishing_pressure("GADOIDS")',
      'fp_plot_groundfish <- plot_fishing_pressure("GROUNDFISH")',
      '# Note that data for other groups may be available.',
      '# Check eco_indicators columns in marea.',
      '',
      '# merge with patchwork',
      'fp_plot_full <- ',
      '  (fp_plot_clupeids + ',
      '     fp_plot_finfish + ',
      '     fp_plot_flatfish + ',
      '     fp_plot_forage + ',
      '     fp_plot_gadoids + ',
      '     fp_plot_groundfish) &',
      '  theme_light(base_size = 9) ',
      '',
      'fp_plot_full',
      '',
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
      '### Fisheries Pressure',
      '',
      'This indicator measures the level of exploitation or total fishing pressure at the ecosystem level (Landings/Biomass). Change in this indicator can result from change in biomass, landings or both.',
      '',
      '```{r plot-fishing-pressure, fig.align="center", fig.pos = "H", fig.width = 7, fig.height=4.5,fig.cap="Fishing pressure for target groups"}',
      '',
      '# insert fishing pressure data here, if available.',
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

.cumulative_impacts_template <- function(.region){

  not_blank <-
    c(
      '### Cumulative Impacts',
      '',
      "Template data not available.",
      "",
      '```{r plot-cumulative-impacts, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Cumulative impacts data, if available.',
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
      '### Cumulative Impacts',
      '',
      '```{r plot-cumulative-impacts, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Cumulative impacts data, if available.',
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


.nitrate_template <- function(.region){

  not_blank <-
    c(
      '### Nitrate',
      '',
      "Template data not available.",
      "",
      '```{r plot-nitrate, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Nitrate data, if available.',
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
      '### Nitrate',
      '',
      '```{r plot-nitrate, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Nitrate data, if available.',
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


.chlorophyll_template <- function(.region){

  not_blank <-
    c(
      '### Chlorophyll',
      '',
      "Template data not available.",
      "",
      '```{r plot-chlorophyll, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Chlorophyll data, if available.',
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
      '### Chlorophyll',
      '',
      '```{r plot-chlorophyll, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Chlorophyll data, if available.',
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

.chlorophyll_template <- function(.region){

  not_blank <-
    c(
      '### Bloom Statistics',
      '',
      "Template data not available.",
      "",
      '```{r plot-bloom-stat, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Bloom Statistic data, if available.',
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
      '### Bloom Statistics',
      "",
      '```{r plot-bloom-stat, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Bloom Statistic data, if available.',
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

.c_finmarchicus_template <- function(.region){

  not_blank <-
    c(
      '### Bloom Statistics',
      '',
      "Template data not available.",
      "",
      '```{r plot-bloom-stat, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Bloom Statistic data, if available.',
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
      '### Bloom Statistics',
      '',
      '```{r plot-bloom-stat, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Bloom Statistic data, if available.',
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

.zoo_biomass_template <- function(.region){

  not_blank <-
    c(
      '### Zooplankton Biomass',
      '',
      "Template data not available.",
      "",
      '```{r plot-zoo-biomass, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Zooplankton Biomass data, if available.',
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
      '### Zooplankton Biomass',
      '',
      "Template data not available.",
      "",
      '```{r plot-zoo-biomass, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Zooplankton Biomass data, if available.',
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


.large_fish_template <- function(.region){

  not_blank <-
    c(
      '### Large Fish Indicator',
      '',
      "Template data not available.",
      "",
      '```{r plot-large-fish, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Large Fish Indicator data, if available.',
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
      '### Large Fish Indicator',
      '',
      '```{r plot-large-fish, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Large Fish Indicator data, if available.',
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


.biodiversity_vars_template <- function(.region){

  .region_code <- switch(.region,
                         "4X" = 'c("4X")',
                         "4W" = 'c("4W")',
                         "4V" = 'c("4VS", "4VN")',
                         "4VS" = 'c("4VS")',
                         "4VN" = 'c("4VN")',
                         "ESS" = 'c("ESS")',
                         "WSS" = 'c("WSS")'
  )


  not_blank <-
    c(
      glue::glue('### Shannon Diversity Index, Margalef Richness, Heip’s Evenness; local scale {.region_code}'),
      '',
      'Shannon Diversity Index accounts for both abundance and evenness of species present in a community. Margalef Richness measures the number of species present accounting for sampling effects. Heips Evenness measures how equally the species richness contributes to the total abundance or biomass of the community. Due to improvements in species identification over time, long-term Shannon Diversity and Margalef richness can be misleading, only years 2000-2020 were used here.',
      '',
      sprintf('```{r plot-biodiversity-vars, fig.align="center", fig.pos = "H", fig.width = 7, fig.height=2.5,fig.cap="Biodiversity Variables for target groups in region %s"}',.region),
      '',
      'data("eco_indicators")',
      '# subset to target region',
      glue::glue('eco_indicators_region <- ea.subset(eco_indicators,"region",{.region_code})'),
      'rm(eco_indicators)',
      '',
      'plot_biodiversity_vars <- function(var_col, var_name){',
      '  ',
      '  ea_data_biodiversity <- ea_data(',
      '    data = eco_indicators_region[["data"]][c("year",var_col)],',
      '    value_col =  var_col,',
      '    data_type = var_name,',
      '    region = "ESS",',
      '    location_descriptor = "ESS NAFO management region",',
      '    units = "",',
      '    source_citation = eco_indicators_region@meta$source_citation',
      '  )',
      '  ',
      '  ea_biodiversity_plot <- plot(ea_data_biodiversity, style = "plain")',
      '  return(ea_biodiversity_plot)',
      '  ',
      '}',
      '',
      'bio_plot_shannon <- plot_biodiversity_vars("ShannonDiversity_ALL_value","Shannon Diversity")',
      'bio_plot_margalef <- plot_biodiversity_vars("MargalefRichness_ALL_value","Margalef Richness")',
      'bio_plot_heips <- plot_biodiversity_vars("Heips_ALL_value","Heips Evenness")',
      '',
      '',
      '# merge with patchwork',
      'bio_plot_full <- ',
      '  (bio_plot_shannon + ',
      '     bio_plot_margalef + ',
      '     bio_plot_heips) &',
      '  theme_light(base_size = 9) ',
      '',
      'bio_plot_full',
      '',
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
      '### Shannon Diversity Index, Margalef Richness, Heip’s Evenness; local scale',
      '',
      'Shannon Diversity Index accounts for both abundance and evenness of species present in a community. Margalef Richness measures the number of species present accounting for sampling effects. Heips Evenness measures how equally the species richness contributes to the total abundance or biomass of the community. Due to improvements in species identification over time, long-term Shannon Diversity and Margalef richness can be misleading, only years 2000-2020 were used here.',
      '',
      '```{r plot-biodiversity-vars, fig.align="center", fig.pos = "H", fig.width = 7, fig.height=2.5,fig.cap="Biodiversity Variables"}',
      '',
      '# add biodiversity data here, if available.',
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


.guild_biomass_template <- function(.region){

  .region_code <- switch(.region,
                         "4X" = 'c("4X")',
                         "4W" = 'c("4W")',
                         "4V" = 'c("4VS", "4VN")',
                         "4VS" = 'c("4VS")',
                         "4VN" = 'c("4VN")',
                         "ESS" = 'c("ESS")',
                         "WSS" = 'c("WSS")'
  )

  not_blank <-
    c(
      glue::glue('### Guild-Level Biomass; local scale {.region_code}'),
      '',
      'Guild-level biomasses address structural attributes of food webs, and can also serve as proxies for ecosystem functioning ([Tam et al. 2017](https://academic.oup.com/icesjms/article/74/7/2040/2970046)). This indicator includes multiple guilds for fish and invertebrates from the RV Survey.',
      '',
      sprintf('```{r plot-guild-biomass, fig.align="center", fig.pos = "H", fig.width = 7, fig.height=4.5,fig.cap="Biomass of target groups from RV surveys in region %s"}',.region),
      '',
      'data("eco_indicators")',
      '# subset to target region',
      glue::glue('eco_indicators_region <- ea.subset(eco_indicators,"region",{.region_code})'),
      'rm(eco_indicators)',
      '',
      'plot_guild_biomass <- function(guild){',
      '  ',
      '  ea_data_guild <- ea_data(',
      '    data = eco_indicators_region[["data"]][c("year",paste0("BIOMASS_",guild,"_value"))],',
      '    value_col =  paste0("BIOMASS_",guild,"_value"),',
      '    data_type = paste0(str_to_title(guild), " Biomass"),',
      sprintf('    region = "%s",',.region),
      sprintf('    location_descriptor = "%s NAFO management region",',.region),
      '    units = "tonnes",',
      '    source_citation = eco_indicators_region@meta$source_citation',
      '  )',
      '  ',
      '  ea_guild_biomass_plot <- plot(ea_data_guild, style = "default")',
      '  return(ea_guild_biomass_plot)',
      '  ',
      '}',
      '',
      '# make fisheries plot for each group',
      'biomass_plot_clupeids <- plot_guild_biomass("CLUPEIDS")',
      'biomass_plot_finfish <- plot_guild_biomass("FINFISH")',
      'biomass_plot_flatfish  <- plot_guild_biomass("FLATFISH")',
      'biomass_plot_forage <- plot_guild_biomass("FORAGE")',
      'biomass_plot_gadoids  <- plot_guild_biomass("GADOIDS")',
      'biomass_plot_groundfish <- plot_guild_biomass("GROUNDFISH")',
      '# Note that data for other groups may be available. ',
      '# Check eco_indicators columns in marea.',
      '',
      '# merge with patchwork',
      'biomass_plot_full <- ',
      '  (biomass_plot_clupeids + ',
      '     biomass_plot_finfish + ',
      '     biomass_plot_flatfish + ',
      '     biomass_plot_forage + ',
      '     biomass_plot_gadoids + ',
      '     biomass_plot_groundfish) &',
      '  theme_light(base_size = 9) ',
      '',
      'biomass_plot_full',
      '',
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
      '### Guild-Level Biomass',
      '',
      'Guild-level biomasses address structural attributes of food webs, and can also serve as proxies for ecosystem functioning ([Tam et al. 2017](https://academic.oup.com/icesjms/article/74/7/2040/2970046)). This indicator includes multiple guilds for fish and invertebrates from the RV Survey.',
      '',
      '```{r plot-guild-biomass, fig.align="center", fig.pos = "H", fig.width = 7, fig.height=4.5,fig.cap="Biomass of target groups"}',
      '',
      '# add guild-level biomass data here, if available.',
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


.nontarget_species_template <- function(.region){

  not_blank <-
    c(
      '### Non-Target Species',
      '',
      "Template data not available.",
      "",
      '```{r plot-non-target-species, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Non-target species data, if available.',
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
      '### Non-Target Species',
      '',
      '```{r plot-non-target-species, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Non-target species data, if available.',
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


# main Ecological Inidicators function -----------------------------------
#' Add Ecological Indicators to template report
#'
#' @param .region Region of template report. Inherited from `build_esr_template()`.
#' @param .ecol_vars Ecological Variables to include in template report. Inherited from `build_esr_template()`.
#'
#' @returns Rmd chunk(s) for ecological variables in template report.
#' @export
ecological_indicators <- function(.region, .ecol_vars){


  general_intro <- c( "# B. Ecological {#ecological}","")

  commercial_fisheries_header <- c("## Commercial Fisheries","")
  commercial_fisheries_body <- c(
    if("commercial_fishing" %in% .ecol_vars) .commercial_fishing_template(.region),
    if("fishing_pressure" %in% .ecol_vars) .fishing_pressure_template(.region)
  )

  habitat_header <- c("## Habitat","")
  habitat_body <- c(
    if("cumulative_impacts" %in% .ecol_vars) .cumulative_impacts_template(.region)
  )

  productivity_header <- c("## Productivity","")
  productivity_body <- c(
    if("nitrate" %in% .ecol_vars) .nitrate_template(.region),
    if("chlorophyll" %in% .ecol_vars) .chlorophyll_template(.region),
    if("bloom_statistic" %in% .ecol_vars) .bloom_statistic_template(.region),
    if(".c_finmarchicus" %in% .ecol_vars) .c_finmarchicus_template(.region),
    if("zoo_biomass" %in% .ecol_vars) .zoo_biomass_template(.region),
    if("large_fish" %in% .ecol_vars) .large_fish_template(.region)
  )

  biodiversity_header <- c("## Biodiversity","")
  biodiversity_body <- c(
    if("biodiversity_vars" %in% .ecol_vars) .biodiversity_vars_template(.region),
    if("guild_biomass" %in% .ecol_vars) .guild_biomass_template(.region)
  )

  nontarget_species_header <- c("## Non-Target Species","")
  nontarget_species_body <- c(
    if("nontarget_species" %in% .ecol_vars) .nontarget_species_template(.region)
  )

  out <- c(general_intro,
           commercial_fisheries_header,
           commercial_fisheries_body,
           habitat_header,
           habitat_body,
           productivity_header,
           productivity_body,
           biodiversity_header,
           biodiversity_body,
           nontarget_species_header,
           nontarget_species_body
  )

  # make sure out ends with newpage, otherwise, add it.
  # (this occurs when not all ecol_vars are selected)
  if(!out[length(out)-2] == "\\newpage"){
    out <- c(out,"\\newpage","","")
  }


  return(out)


}
