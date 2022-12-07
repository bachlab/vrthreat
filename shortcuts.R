rm(list = ls())
devtools::document()
devtools::install(build_vignettes = FALSE)
devtools::test()
devtools::build_vignettes()

pkgdown::build_site()

devtools::check()
devtools::build()
