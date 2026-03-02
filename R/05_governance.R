# Template function for Governance Indicators Section


# main Governance function -----------------------------------
#' Add Governance section to template Rmd report.
#'
#' @returns Rmd section for governance in template report.
#' @export
governance <- function(){

  general_intro <- c( "# E. Governance {#governance}","")

  no_data <- c("Currently unavailable","","","\\newpage","","")

  out <- c(
    general_intro,
    no_data
  )

  return(out)


}
