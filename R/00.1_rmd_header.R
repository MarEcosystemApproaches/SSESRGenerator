#' Create Report Header
#'
#' @param .region Region for report; inherited from `build_esr_template()`.
#'
#' @returns Header section for report: Rmd header, and code chunk for required packages.
#' @export
#'
#' @examples rmd_header("4W")
rmd_header <- function(.region){

  header <- c(
    '---',
    sprintf('title: "%s Ecosystem Summary Report pilot for Region %s"',str_sub(Sys.Date(),1,4),.region),
    'author: "Maritimes EBFM WG"',
    'date: "`r Sys.Date()`"',
    'output:',
    '  pdf_document:',
    '  toc: true',
    'toc_depth: 2',
    'df_print: kable',
    'highlight: haddock',
    'latex_engine: xelatex',
    'linkcolor: blue    ',
    'urlcolor: blue',
    'citecolor: red',
    'toccolor: blue',
    '---',
    '',
    '',
    ''
  )


  setup_chunk <- c(
    '```{r setup, include=FALSE}',
    '',
    'knitr::opts_chunk$set(echo = FALSE, message=FALSE, warning=FALSE)',
    '',
    '# load all necessary packages',
    'require(marea)',
    'require(dplyr)',
    'require(ggplot2)',
    'require(stringr)',
    'require(purrr)',
    'require(patchwork)',
    'require(knitr)',
    'require(glue)',
    'require(sf)',
    'require(kableExtra)',
    'require(SSESRGenerator)',
    #'',
    #'# set global data directory',
    #'dat.dir <- here::here("data")',
    '',
    '```',
    '',
    '',
    '\\newpage',
    '',
    ''
  )

  out <- c(header, setup_chunk)

  return(out)

}
