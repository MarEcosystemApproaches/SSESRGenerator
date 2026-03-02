# Generate Ecosystem Summary Report Templates for Scotian Shelf Regions


This package enables automated templating of Ecosystem Summary Report templates for target Scotian Shelf Regions. Use this package to generate .Rmd reports with pre-filled template sections for seven Scotian Shelf target regions, or a blank template report skeleton. 

Generated report templates include pre-defined outline sections for ESR objectives and pre-generated plots for target indicators, but require manual editing and interpretation of ESR data. 


## Download the package
```
devtools::install_github("MarEcosystemApproaches/SSESRGenerator")
library(SSESRGenerator)
```

## Generate Template Report
```
build_esr_template("4X")
```
This function saves a template report, entitled, 4X_esr_template_{date}.Rmd to a specified directory (or defaults to "template_reports"), which can then be manually edited for reporting. 
