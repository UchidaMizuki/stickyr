
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
#> # A tibble: 87 × 14
#> # Stickers: height, mass, birth_year
#>    name     height  mass hair_color skin_color eye_color birth_year sex   gender
#>    <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
#>  1 Luke Sk…    172    77 blond      fair       blue            19   male  mascu…
#>  2 C-3PO       167    75 <NA>       gold       yellow         112   none  mascu…
#>  3 R2-D2        96    32 <NA>       white, bl… red             33   none  mascu…
#>  4 Darth V…    202   136 none       white      yellow          41.9 male  mascu…
#>  5 Leia Or…    150    49 brown      light      brown           19   fema… femin…
#>  6 Owen La…    178   120 brown, gr… light      blue            52   male  mascu…
#>  7 Beru Wh…    165    75 brown      light      blue            47   fema… femin…
#>  8 R5-D4        97    32 <NA>       white, red red             NA   none  mascu…
#>  9 Biggs D…    183    84 black      light      brown           24   male  mascu…
#> 10 Obi-Wan…    182    77 auburn, w… fair       blue-gray       57   male  mascu…
#> # ℹ 77 more rows
#> # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
#> #   vehicles <list>, starships <list>
```

### Select data

``` r
sticky_starwars |>
  select(name, species)
#> # A tibble: 87 × 5
#> # Stickers: height, mass, birth_year
#>    name               species height  mass birth_year
#>  * <chr>              <chr>    <int> <dbl>      <dbl>
#>  1 Luke Skywalker     Human      172    77       19  
#>  2 C-3PO              Droid      167    75      112  
#>  3 R2-D2              Droid       96    32       33  
#>  4 Darth Vader        Human      202   136       41.9
#>  5 Leia Organa        Human      150    49       19  
#>  6 Owen Lars          Human      178   120       52  
#>  7 Beru Whitesun Lars Human      165    75       47  
#>  8 R5-D4              Droid       97    32       NA  
#>  9 Biggs Darklighter  Human      183    84       24  
#> 10 Obi-Wan Kenobi     Human      182    77       57  
#> # ℹ 77 more rows
```

### Summarise data

``` r
sticky_starwars |>
  group_by(species) |>
  summarise()
#> # A tibble: 38 × 4
#> # Stickers: height, mass, birth_year
#>    species   height  mass birth_year
#>  * <chr>      <dbl> <dbl>      <dbl>
#>  1 Aleena       79     15         NA
#>  2 Besalisk    198    102         NA
#>  3 Cerean      198     82         92
#>  4 Chagrian    196     NA         NA
#>  5 Clawdite    168     55         NA
#>  6 Droid        NA     NA         NA
#>  7 Dug         112     40         NA
#>  8 Ewok         88     20          8
#>  9 Geonosian   183     80         NA
#> 10 Gungan      209.    NA         NA
#> # ℹ 28 more rows
```
