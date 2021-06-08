globalVariables(names = c('no'))






.onAttach <- function(libname, pkgname) {
  citation ="To cite WIAD in publications, please cite both the paper and the R package:
  
- Bijan Seyednasrollah, Tim Rademacher, David Basler (2020), WIAD R Package: Wood
Image and Analysis Dataset

- Rademacher et al., (2021), The Wood Image and Analysis Dataset (WIAD): open-access
visual analysis tools to advance the ecological data revolution

To see options for other formats, use 'citation('wiad')'."
  
  packageStartupMessage(citation)
}
