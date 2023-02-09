
<!-- README.md is generated from README.Rmd. Please edit that file -->

# surveymeta

<!-- badges: start -->
<!-- badges: end -->

Surveymeta is an API client package for the [NADA
API](https://catalog.ihsn.org/api-documentation/catalog/#), allowing
users to retrieve survey metadata from the International Household
Survey Network (IHSN) central data catalog and other repositories
following the NADA standard, such as the [World Bank micro data
library](https://microdata.worldbank.org/index.php/home).

## Installation

You can install the development version of surveymeta from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("northeastloon/surveymeta")
```

## Example

List surveys from 2015-2018:

``` r
library(surveymeta)

get_metadata("http://catalog.ihsn.org/index.php/api/catalog", from =2015, to = 2018, ps =ie4, var_meta = FALSE)
```

Or all metadata files created during January 2023:

``` r

get_metadata("http://catalog.ihsn.org/index.php/api/catalog", created = "2023/01/01-2023/01/31", ps =ie4, var_meta = FALSE)
```

specify `var_meta` as `TRUE` to extract variable metadata for (five)
surveys

``` r
get_metadata("http://catalog.ihsn.org/index.php/api/catalog", created = "2023/01/01-2023/01/31", ps =5, var_meta = TRUE)
```
