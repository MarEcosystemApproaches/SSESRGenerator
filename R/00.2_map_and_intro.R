#' Generate Map and Intro Page for template ESR
#'
#' @param .region Target region for report. Inherited from `build_esr_report()`.
#'
#' @returns Page to generate map and intro page for ESR repor template
#' @export
#'
#' @examples map_and_intro("4W")
map_and_intro <- function(.region){

  .region_code <- switch(.region,
                         "4X" = 'c("4X")',
                         "4W" = 'c("4W")',
                         "4V" = 'c("4V")',
                         "ESS" = 'c("4V","4W")',
                         "WSS" = 'c("4X")'
  )

  not_blank <- c(
    sprintf('```{r make-map-ess, out.width="75%%", fig.align="center", fig.cap="Map of Region: %s"}',.region),
    '',
    '# load spatial data',
    'data("NAFO_shapefiles")',
    'invisible(get(data(list = "coastline", package = "marea")))',
    'bbox <- c(xmin = -75, xmax = -50, ymin = 35, ymax = 50)',
    '',
    'target_region <- NAFO_shapefiles %>%',
    glue::glue('filter(NAFO_1 %in% {.region_code})'),
    '',
    '# make map',
    'target_area_map <- ggplot() +',
    '  geom_sf(data = target_region, fill = "darkblue") +',
    '  geom_sf(data = coastline) +',
    '  coord_sf(xlim = c(bbox[["xmin"]], bbox[["xmax"]]),',
    '           ylim = c(bbox[["ymin"]], bbox[["ymax"]]),',
    '           expand=F) +',
    '  theme_light()',
    '',
    '# print map',
    'target_area_map',
    '',
    '```',
    '',
    ''
  )

  blank <- c(
    '```{r make-map-ess, out.width="75%", fig.align="center", fig.cap="Map of Study Region"}',
    '',
    '# load spatial data',
    'invisible(get(data(list = "coastline", package = "marea")))',
    'bbox <- c(xmin = -75, xmax = -50, ymin = 35, ymax = 50)',
    '',
    '# make map',
    'target_area_map <- ggplot() +',
    '  geom_sf(data = coastline) +',
    '  coord_sf(xlim = c(bbox[["xmin"]], bbox[["xmax"]]),',
    '           ylim = c(bbox[["ymin"]], bbox[["ymax"]]),',
    '           expand=F) +',
    '  theme_light()',
    '',
    '# print map',
    'target_area_map',
    '',
    '```',
    '',
    ''
  )


  intro <- c(
    '# Introduction {#introduction}',
    '',
    '### About this report',
    "",
    "This Ecosystem Summary Report supports DFO's efforts to incorporate ecosystem approaches into Fisheries Science and Resource Management in the Maritimes Region. The purpose of this report is to synthesize ecosystem information to inform an ecosystem approach, as highlighted in the modernized Fisheries Act ([2019; Bill C-68](https://lop.parl.ca/sites/PublicWebsite/default/en_CA/ResearchPublications/LegislativeSummaries/421C68E)). It aligns objectives broadly to those of the [Maritimes Ecosystem Based Management Framework](https://sites.google.com/view/ebminitiative/ebm-initiative-home), and regional/area-based objectives that contribute to Scientific Advice and Management Plans or other Harvest Strategies. The aim of this Ecosystem Summary Report is to provide an integrative suite of ecosystem indicators relevant to a specific fisheries management area that can support Ecosystem Approaches to Fisheries Management ([EAFM](https://www.dfo-mpo.gc.ca/fisheries-peches/initiatives/ecosystems-approach-approche-ecosystemique/index-eng.html)) and Ecosystem Based Fisheries Management ([EBFM](https://sites.google.com/view/maritimesebfmwg/home)) at DFO.",
    "",
    "### Report structure",
    "",
    'This report outlines ecosystem objectives related to a specific management area (Table 1), and reviews relevant ecosystem indicator trends and the status of the most recent year(s) relative to a reference point (if available) or a long-term average. The results are synthesized to outline potential implications for the management area examined, but not specifically related to a focal stock or species. Where relevant this document provides links to reports and resources for details and methodology. This report summarizes general ecosystem trends to support integration of ecosystem information and expertise into Science Advice (e.g. stock assessment) or Resource Management (e.g. Integrated Fisheries Management Plans), but is intended for a variety of users.',
    "",
    "This is a summary of the best available information describing the ecosystem specific to management areas within the Maritimes Region.",
    "",
    "",
    '\\newpage',
    ""
  )


  if(.region == "blank"){
    out <- c(blank, intro)
  } else {
    out <- c(not_blank, intro)
  }
  return(out)

}


