
<!-- README.md is generated from README.Rmd. Please edit that file -->

# percentageByGroup

<!-- badges: start -->
<!-- badges: end -->

The goal of percentageByGroup is to summarize the percentage of rows
that satisfies given condition(s) by group

## Installation

You can install the development version of percentageByGroup like so:

``` r
# install.packages("devtools")
devtools::install_github("stat545ubc-2022/assignment-b1-and-b2-czz1997")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## load the function
library(percentageByGroup)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
## load the dataset
suppressPackageStartupMessages(library(datateachr))
```

Calculate the percentage of buildings that has a sprinkler system by
year:

``` r
apt_buildings %>%
    filter(year_built >= 1950) %>%
    percentage_by_group('year_built', sprinkler_system == "YES")
#> # A tibble: 69 × 2
#>    year_built percentage
#>         <dbl>      <dbl>
#>  1       1950       0.42
#>  2       1951       0.26
#>  3       1952       0.29
#>  4       1953       0.47
#>  5       1954       0.33
#>  6       1955       0.39
#>  7       1956       0.45
#>  8       1957       0.54
#>  9       1958       0.46
#> 10       1959       0.67
#> # … with 59 more rows
```

Calculate the percentage of buildings that has at least 5 floors by year
and property type:

``` r
apt_buildings %>%
    filter(year_built >= 1950) %>%
    percentage_by_group(c('year_built', 'property_type'), no_of_storeys >= 5)
#> # A tibble: 163 × 3
#> # Groups:   year_built [69]
#>    year_built property_type  percentage
#>         <dbl> <chr>               <dbl>
#>  1       1950 PRIVATE              0.08
#>  2       1950 SOCIAL HOUSING       0.25
#>  3       1951 PRIVATE              0.07
#>  4       1952 PRIVATE              0.14
#>  5       1953 PRIVATE              0.21
#>  6       1953 SOCIAL HOUSING       0   
#>  7       1953 TCHC                 1   
#>  8       1954 PRIVATE              0.11
#>  9       1954 TCHC                 0   
#> 10       1955 PRIVATE              0.18
#> # … with 153 more rows
```
