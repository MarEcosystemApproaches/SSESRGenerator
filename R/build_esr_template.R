#' Build Ecosystem Summary Report Template
#'
#' @param region the region for which to build template report. Options are: "4X", "4W", "4V", "4VN", "4VS", "ESS", "WSS", or "blank" for a blank template report
#' @param ps_vars Pressures and Stressors variables to include in a report. Options are: "nao", "amo", "azmp_satellite_temperature", "azmp_surface_temperature", "azmp_bottom_temperature", "azmp_stratification", "azmp_salinity", "ocean_acidification", "future_projections". Defaults to all.
#' @param ecol_vars Ecological Variables to include in report template. Options are "commercial_fishing", "fishing_pressure", "cumulative_impacts", "nitrate", "chlorophyll", "bloom_stat", ".c_finmarchicus_template", "zoo_biomass", "large_fish", "biodiversity_vars", "guild_biomass", "nontarget_species". Defaults to all.
#' @param econ_vars Economic Variabiles to include in report template. Options are "commercial_fisheries_value". Defaults to all.
#' @param out.dir the directory in which to save a generated report template.
#'
#' @returns a pre-filled .Rmd document containing script to generate a template Ecosystem Summary Report
#' @export
#'
#' @examples build_esr_template(region = "4W")
build_esr_template <- function(region,
                               ps_vars = c("nao",
                                           "amo",
                                           "azmp_satellite_temperature",
                                           "azmp_surface_temperature",
                                           "azmp_bottom_temperature",
                                           "azmp_stratification",
                                           "azmp_salinity",
                                           "ocean_acidification",
                                           "future_projections"),
                               ecol_vars = c("commercial_fishing",
                                             "fishing_pressure",
                                             "cumulative_impacts",
                                             "nitrate",
                                             "chlorophyll",
                                             "bloom_stat",
                                             ".c_finmarchicus_template",
                                             "zoo_biomass",
                                             "large_fish",
                                             "biodiversity_vars",
                                             "guild_biomass",
                                             "nontarget_species"
                               ),
                               econ_vars = c("commercial_fisheries_value"),
                               out.dir = "template_reports"){

  require(stringr)

  # check if target region is in the list of target regions
  if(!region %in% c("4X","4W","4V","4VN","4VS","ESS","WSS","blank"))
    stop('Template reports can only be generated for regions 4X, 4W, 4V, 4VN, 4VS, ESS, and WSS. use region = "blank" for empty template.')


  # first, source all functions in the sections/ folder,
  # which will contain the bodies of all the sections individually.
  sections_path <- "report_generation_functions/template_report_helpers"
  section_files <- list.files(sections_path, full.names = T)
  lapply(section_files, source)


  # check if template file already exists
  path <- paste0("template_reports/",region,"esr_template_",Sys.Date(),".Rmd")

  # check if there's already a page for indicator
  if (file.exists(path)) {

    cat("Warning: Template Report already exists.\n",
        "File: ", path, "\n",
        "Do you want to proceed?\n",
        "The existing file will be lost.")
    ans <- readline(("Type 'y' to overwrite or 'n' to cancel: "))

    # stop if answer isn't y or n
    if(!ans %in% c("y","n")){
      stop("Answer must be 'y' or 'n' ", call. = FALSE)
    }

    # stop if answer is n
    if(ans == c("n")){
      message("Cancelled. Existing template report kept: ", path)
      return(invisible(FALSE)) # end function
    }

    # if yes, print that the chapter will be overwritten
    message("Overwriting existing template report: ", path)
  }


  # start building RMD report -----------------------------------------------
  lines <- c(

    # 00.1 R Markdown Header
    rmd_header(.region = region),

    # 00.2 Target area map and intro
    map_and_intro(.region = region),

    # 00.3 Print Objectives Table
    print_objectives_table(.region = region),

    # 01. Pressures and Stressors
    pressures_and_stressors(.region = region, .ps_vars = ps_vars),

    # 02. Ecological Indicators
    ecological_indicators(.region = region, .ecol_vars = ecol_vars),

    # 03. Economic Indicators
    economic_indicators(.region = region, .econ_vars = econ_vars),

    # 04. Social-Cultural Indicators
    social_cultural(),

    # 05. Governance
    governance()


  )


  # start building RMD report -----------------------------------------------
  dir.create(out.dir, showWarnings = F)
  out.path <- file.path(out.dir, paste0(region,"_","esr_template_",Sys.Date(),".Rmd"))
  writeLines(lines, out.path)
  message("Created page template: ", out.path)
  invisible(out.path)


}
