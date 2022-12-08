# VRThreat: Analysis R package

R package with generic analysis tools for VRThreat data generated with the VRthreat Unity package. This can be used to analyse data generated with publicly available releases of VRthreat games, collected on [OSF](https://osf.io/2b3k7/). Brought to you by [Bachlab](http://bachlab.org) at [Universität Bonn](https://www.uni-bonn.de/en) and [University College London](https://www.ucl.ac.uk)

Documentation: https://bachlab.github.io/vrthreat/index.html

# How to install the package from GitHub:

### Step 1: Install the devtools package:

```r
install.packages("devtools")
```

### Step 2: Install the vrthreat package:

```r
devtools::install_github("bachlab/vrthreat")
```

NOTE: the vrthreat package is not currently available on CRAN as it heavily uses tidyverse-style non-standard evaluation
which is not accepted on CRAN. This may change in the future.

## Scripts

Scripts containing the functions for this package are located in the `R` directory. All functions here are added and available to use when you install the package.

## Testing

Unit tests can be performed using the `devtools` and `testthat` packages if you clone the repository locally.

## Development

Several shortcut functions for development are in the `shortcuts.R` script. You can use these to build documentation, the documentation website, run tests, etc.

General information on package development: https://r-pkgs.org/man.html

Quick tips:

* Functions should be in scripts stored in the `R` folder.
* If you need to use functions from another package, you should call them explicitly (e.g. `dplyr::filter(...)`). Dependencies should be added to the imports in the DESCRIPTION file.
* You can use RStudio to [generate documentation skeleton for functions](https://stackoverflow.com/a/30675146/5024009).
* Follow the [Tidyverse Style Guide](https://style.tidyverse.org/).

## Some types of functions and naming conventions

* functions that deal with one or several individual movement data frames (e.g. tracker data) and return a new data frame (usually encapsulated in a list, for use with "mutate" or "summmarise")
* functions that deal with columns that contain embedded movement data frames: name corresponds to the name of the function that processes the individual movement data frames, with "_cols" appended
* functions that add a column to a movement data frame and return the same data frame without encapsulation (for use in `extract_` functions)
* functions that extract a scalar value from a movement data frame: `extract_...` (avoiding the word `summarise` which in the tidyverse stands for summarising a data frame across rows into a new data frame)
