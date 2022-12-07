rm(list = ls())
devtools::document()
devtools::install(build_vignettes = FALSE)
devtools::test()
devtools::build_vignettes()

pkgdown::build_site()

devtools::check()
devtools::build()

devtools::spell_check()
devtools::check_rhub()
devtools::check_win_devel()

devtools::release()
