# Template functions for Economic Indicators Section

# internal variable template functions ------------------------------------
.commercial_fishing_value_template <- function(.region){

  not_blank <-
    c(
      '### Landing Value by Species Category',
      '',
      "Template data not available.",
      "",
      '```{r plot-commercial-fisheries-value, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Commercial Fisheries data, if available.',
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
      '### Landing Value by Species Category',
      '',
      '```{r plot-commercial-fisheries-value, fig.align="center", echo = FALSE,fig.pos="H", fig.width = 5, fig.height=3,fig.cap=""}',
      '',
      '# Add Commercial Fisheries data, if available.',
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



# Main Economic Variables function -----------------------------------
#' Add Economic Indicators section to template report
#'
#' @param .region Target region of template report. Inherited from `build_esr_template()`.
#' @param .econ_vars Economic variables to include in template report. Inherited from `build_esr_template()`.
#'
#' @returns Rmd chunk(s) for economic indicators in template report.
#' @export
economic_indicators <- function(.region, .econ_vars){


  general_intro <- c( "# C. Economic {#economic}","")

  commercial_fisheries_value_header <- c("## Commercial Fisheries",
                                         "",
                                         "Commercial fishing is an important activity in the Maritimes Region that provides both cultural and economic value to coastal communities.",
                                         "")
  commercial_fisheries_value_body <- c(
    if("commercial_fisheries_value" %in% .econ_vars) .commercial_fishing_value_template(.region)
  )

  out <- c(general_intro,
           commercial_fisheries_value_header,
           commercial_fisheries_value_body
  )

  # make sure out ends with newpage, otherwise, add it.
  # (this occurs when not all econ_vars are selected)
  if(!out[length(out)-2] == "\\newpage"){
    out <- c(out,"\\newpage","","")
  }


  return(out)


}
