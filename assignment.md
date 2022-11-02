Assignment B-1: Making a function
================

# Assignment B-1: Making a function

## Getting Started

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(datateachr))
```

## Exercise 1: Make a Function

``` r
#' @title Percentage by Group
#' @details
#' Summarize the percentage of rows that satisfies given condition(s) by group
#'
#' @param .data A data frame, data frame extension (e.g. a tibble),
#'  or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param group_by_cols Column names or ids to group the rows
#' @param conditions  Conditions that a row should satisfy to be considered in
#'  percentage calculation
#' @return A summarized dataframe with all combinations of group_by_cols and a
#'  new column "percentage"
percentage_by_group <- function(.data, group_by_cols, conditions = TRUE) {
    stopifnot(is.data.frame(.data))
    if(is.numeric(group_by_cols)) {
        stopifnot(floor(group_by_cols) == group_by_cols)
    }
    .data %>%
    drop_na(all_of(group_by_cols)) %>%
    group_by(across(all_of(group_by_cols))) %>%
    summarise(percentage = round(sum({{conditions}}) / n(), digits=2))
}
```

## Exercise 2: Document your Function

See above.

## Exercise 3: Include examples

Calculate the percentage of buildings that has a sprinkler system by
year:

``` r
apt_buildings %>%
    filter(year_built >= 1950) %>%
    drop_na(sprinkler_system) %>%
    percentage_by_group('year_built', sprinkler_system == "YES")
```

    ## # A tibble: 69 × 2
    ##    year_built percentage
    ##         <dbl>      <dbl>
    ##  1       1950       0.42
    ##  2       1951       0.32
    ##  3       1952       0.29
    ##  4       1953       0.48
    ##  5       1954       0.38
    ##  6       1955       0.41
    ##  7       1956       0.48
    ##  8       1957       0.56
    ##  9       1958       0.47
    ## 10       1959       0.7 
    ## # … with 59 more rows

Calculate the percentage of buildings that has at least 5 floors by year
and property type:

``` r
apt_buildings %>%
    filter(year_built >= 1950) %>%
    drop_na(year_built) %>%
    percentage_by_group(c('year_built', 'property_type'), no_of_storeys >= 5)
```

    ## `summarise()` has grouped output by 'year_built'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 163 × 3
    ## # Groups:   year_built [69]
    ##    year_built property_type  percentage
    ##         <dbl> <chr>               <dbl>
    ##  1       1950 PRIVATE              0.08
    ##  2       1950 SOCIAL HOUSING       0.25
    ##  3       1951 PRIVATE              0.07
    ##  4       1952 PRIVATE              0.14
    ##  5       1953 PRIVATE              0.21
    ##  6       1953 SOCIAL HOUSING       0   
    ##  7       1953 TCHC                 1   
    ##  8       1954 PRIVATE              0.11
    ##  9       1954 TCHC                 0   
    ## 10       1955 PRIVATE              0.18
    ## # … with 153 more rows

## Exercise 4: Test the Function

First, sample a small data frame for testing:

``` r
set.seed(123)
apt_buildings_small <- apt_buildings %>%
    filter(year_built >= 1955, year_built < 1960) %>%
    sample_n(50)
head(apt_buildings_small)
```

    ## # A tibble: 6 × 37
    ##      id air_co…¹ ameni…² balco…³ barri…⁴ bike_…⁵ exter…⁶ fire_…⁷ garba…⁸ heati…⁹
    ##   <dbl> <chr>    <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
    ## 1 12981 NONE     <NA>    NO      NO      Not Av… YES     YES     NO      HOT WA…
    ## 2 13270 INDIVID… <NA>    NO      NO      0 indo… NO      YES     NO      ELECTR…
    ## 3 11516 NONE     <NA>    YES     NO      20 ind… NO      YES     YES     HOT WA…
    ## 4 13672 NONE     <NA>    YES     YES     Not Av… NO      YES     YES     HOT WA…
    ## 5 11603 NONE     <NA>    YES     NO      10 ind… NO      YES     NO      HOT WA…
    ## 6 11054 NONE     <NA>    NO      NO      Not Av… NO      YES     NO      HOT WA…
    ## # … with 27 more variables: intercom <chr>, laundry_room <chr>,
    ## #   locker_or_storage_room <chr>, no_of_elevators <dbl>, parking_type <chr>,
    ## #   pets_allowed <chr>, prop_management_company_name <chr>,
    ## #   property_type <chr>, rsn <dbl>, separate_gas_meters <chr>,
    ## #   separate_hydro_meters <chr>, separate_water_meters <chr>,
    ## #   site_address <chr>, sprinkler_system <chr>, visitor_parking <chr>,
    ## #   ward <chr>, window_type <chr>, year_built <dbl>, year_registered <dbl>, …

Test with no NA’s:

``` r
result <- apt_buildings_small %>%
    percentage_by_group('year_built',
    !is.na(sprinkler_system) & sprinkler_system == "YES")
expected <- as_tibble(data.frame(
    year_built = c(1955, 1956, 1957, 1958, 1959),
    percentage = c(0.43, 0.25, 0.62, 0.64, 0.6))
)
test_that("Test with no NA’s", {
    expect_identical(result, expected)
})
```

    ## Test passed 😸

Test with NA’s:

``` r
apt_buildings_small[nrow(apt_buildings_small) + 1, ] <- NA # year_built column will have a NA value
result <- apt_buildings_small %>%
    percentage_by_group('year_built',
    !is.na(sprinkler_system) & sprinkler_system == "YES")
test_that("Test with NA’s", {
    expect_identical(result, expected)
})
```

    ## Test passed 🎉

Test with different type:

``` r
test_that("Test with different type", {
    expect_error(
        apt_buildings_small %>%
            percentage_by_group(c(1.1, 2, 3),
            !is.na(sprinkler_system) & sprinkler_system == "YES")
    )
})
```

    ## Test passed 🎉
