
<!-- README.md is generated from README.Rmd. Please edit that file -->

# libminer

<!-- badges: start -->

[![R-CMD-check](https://github.com/bauersam/libminer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bauersam/libminer/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of libminer is to help you understand your libraries better and
learn how to write packages.

## Installation

You can install the development version of libminer from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bauersam/libminer")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(libminer)

lib_summary()
#>                                                                  Library
#> 1                                     C:/Program Files/R/R-4.3.1/library
#> 2                        C:/Users/fr8412/AppData/Local/R/win-library/4.3
#> 3 C:/Users/fr8412/AppData/Local/Temp/RtmpyWtJrR/temp_libpath381c4a337ad4
#>   n_packages
#> 1         30
#> 2         97
#> 3          1

#Also can calculate sizes
lib_summary(sizes=TRUE)
#>                                                                  Library
#> 1                                     C:/Program Files/R/R-4.3.1/library
#> 2                        C:/Users/fr8412/AppData/Local/R/win-library/4.3
#> 3 C:/Users/fr8412/AppData/Local/Temp/RtmpyWtJrR/temp_libpath381c4a337ad4
#>   n_packages  lib_size
#> 1         30  68858812
#> 2         97 163373521
#> 3          1     13456
```
