# Template function for Social-Cultural Indicators Section


# main Social Cultural function -----------------------------------
#' Add Social Cultural indicators to template report
#'
#' @returns Social Cultural section in template Rmd report
#' @export
social_cultural <- function(){

  general_intro <- c( "# D. Social-Cultural {#social-cultural}","")

  no_data <- c("Currently unavailable","","","\\newpage","","")

  out <- c(
    general_intro,
    no_data
  )

  return(out)


}
