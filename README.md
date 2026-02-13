
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stickyr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/stickyr)](https://CRAN.R-project.org/package=stickyr)
[![R-CMD-check](https://github.com/UchidaMizuki/stickyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UchidaMizuki/stickyr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/UchidaMizuki/stickyr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/UchidaMizuki/stickyr?branch=main)
<!-- badges: end -->

stickyr provides data frames that hold certain columns and attributes
persistently for data processing in dplyr. It can also hide specific
columns.

## Installation

``` r
install.packages("stickyr")
```

### Development version

You can install the development version of stickyr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UchidaMizuki/stickyr")
```

## Example

``` r
library(stickyr)
library(dplyr)

sticky_starwars <- new_sticky_tibble(
  dplyr::starwars,
  cols = c(height, mass, birth_year),
  col_show = !birth_year,
  col_summary = list(height = mean, mass = sum, birth_year = median)
)
sticky_starwars
#> # A tibble: 87 × 13
#> # Stickers: height, mass
#>    name      height  mass hair_color skin_color eye_color sex   gender homeworld
#>    <chr>      <int> <dbl> <chr>      <chr>      <chr>     <chr> <chr>  <chr>    
#>  1 Luke Sky…    172    77 blond      fair       blue      male  mascu… Tatooine 
#>  2 C-3PO        167    75 <NA>       gold       yellow    none  mascu… Tatooine 
#>  3 R2-D2         96    32 <NA>       white, bl… red       none  mascu… Naboo    
#>  4 Darth Va…    202   136 none       white      yellow    male  mascu… Tatooine 
#>  5 Leia Org…    150    49 brown      light      brown     fema… femin… Alderaan 
#>  6 Owen Lars    178   120 brown, gr… light      blue      male  mascu… Tatooine 
#>  7 Beru Whi…    165    75 brown      light      blue      fema… femin… Tatooine 
#>  8 R5-D4         97    32 <NA>       white, red red       none  mascu… Tatooine 
#>  9 Biggs Da…    183    84 black      light      brown     male  mascu… Tatooine 
#> 10 Obi-Wan …    182    77 auburn, w… fair       blue-gray male  mascu… Stewjon  
#> # ℹ 77 more rows
#> # ℹ 4 more variables: species <chr>, films <list>, vehicles <list>,
#> #   starships <list>
```

### Select data

``` r
sticky_starwars |>
  select(name, species)
#> # A tibble: 87 × 4
#> # Stickers: height, mass
#>    name               species height  mass
#>    <chr>              <chr>    <int> <dbl>
#>  1 Luke Skywalker     Human      172    77
#>  2 C-3PO              Droid      167    75
#>  3 R2-D2              Droid       96    32
#>  4 Darth Vader        Human      202   136
#>  5 Leia Organa        Human      150    49
#>  6 Owen Lars          Human      178   120
#>  7 Beru Whitesun Lars Human      165    75
#>  8 R5-D4              Droid       97    32
#>  9 Biggs Darklighter  Human      183    84
#> 10 Obi-Wan Kenobi     Human      182    77
#> # ℹ 77 more rows
```

### Summarise data

``` r
sticky_starwars |>
  group_by(species) |>
  summarise()
#> # A tibble: 38 × 3
#> # Stickers: height, mass
#>    species   height  mass
#>    <chr>      <dbl> <dbl>
#>  1 Aleena       79     15
#>  2 Besalisk    198    102
#>  3 Cerean      198     82
#>  4 Chagrian    196     NA
#>  5 Clawdite    168     55
#>  6 Droid        NA     NA
#>  7 Dug         112     40
#>  8 Ewok         88     20
#>  9 Geonosian   183     80
#> 10 Gungan      209.    NA
#> # ℹ 28 more rows
```
