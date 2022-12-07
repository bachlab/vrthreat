## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTES:

* checking dependencies in R code ... NOTE
  Namespaces in Imports field not imported from:
    'dfoptim' 'optimx'
    
  dfoptim and optimx are required by the function `lme4::allFit` but they are 
  not imported by the package `lme4` (they listed under suggested).

* checking R code for possible problems ... NOTE
  Undefined global functions or variables: ...
  
  These notes are due to using data-variables in `dplyr` functions and do not impede 
  functionality. Removing these notes would require replacing data-variables with
  a cumbersome workaround (see https://dplyr.tidyverse.org/articles/programming.html?q=R%20CMD#eliminating-r-cmd-check-notes)
  
## Downstream dependencies
This is a new package - there are no downstream dependencies.

  
  
