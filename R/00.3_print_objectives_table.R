#' Add Objectives Table for SS region
#'
#' @param .region Region of template report. Inherited from `build_esr_report()`.
#'
#' @returns code chunk in Rmd template to add objectives table.
#' @export
#'
#' @examples print_objectives_table("4W")
print_objectives_table <- function(.region){

  not_blank <-
    c(
      '',
      '# Ecosystem Objectives {#ecosystemobjectives}',
      '',
      'The Ecosystem objectives outline the ecosystem context and considerations for this management area.',
      '',
      '```{r , echo=FALSE, tab.cap="Ecosystem objectives related to EBM Framework Pillar, Management Components, and Indicators." }',
      '# load objectives table',
      'data("ESR_objectives")',
      '',
      'ESR_objectives %>%',
      '  as.data.frame() %>%',
      '  kable()',
      '',
      '```',
      '',
      '',
      '\\newpage ',
      '',
      ''
    )

  blank <-
    c(
      '',
      '# Ecosystem Objectives {#ecosystemobjectives}',
      '',
      'The Ecosystem objectives outline the ecosystem context and considerations for this management area.',
      '',
      '```{r , echo=FALSE, tab.cap="Ecosystem objectives related to EBM Framework Pillar, Management Components, and Indicators." }',
      '',
      '# Add objectives table here',
      '',
      '```',
      '',
      '',
      '\\newpage ',
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
