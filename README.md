
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jogger

<!-- badges: start -->
<!-- badges: end -->

### Retrieve area boundaries and data from the ONS Open Geography Portal

**This README is now out of date, as the package has been refactored.**

**A new README will be written to document the new approach.**

The main function of this package is to download area lookups and
boundaries (in GeoJSON format) using the ONS Open Geography API, for all
sub-areas - at a specified level - within a specified area. The main
script will return a data frame with the sub-area geometry column, as an
`sf` object ready to be visualised as a map. It’s initially just for
areas within England and Wales, with a hope to add Scottish geographies
at a later date. And currently just for the following area levels: LSOA,
MSOA, Ward, LAD/LTLA, UTLA/CTY, CAUTH, RGN *\[subject to change\]*

## Installation

``` r
remotes::install_github("francisbarton/jogger")
```

## Examples

``` r
library(jogger)
library(dplyr, quietly = TRUE)
library(tmap)

tmap::tmap_mode("plot")

geo_get("wd", "Swindon", "lad") %>% 
  tmap::tm_shape() +
  tmap::tm_borders()
```

<img src="man/figures/README-example-1-1.png" width="100%" />

``` r
geo_get("msoa", "Swansea", "lad", return_centroids = TRUE) %>%
  head(5)
#> Joining, by = "msoa11nm"
#> Joining, by = c("lsoa11cd", "lsoa11nm")
#> Joining, by = c("lsoa11cd", "lsoa11nm")
#> Simple feature collection with 5 features and 7 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -4.101869 ymin: 51.62307 xmax: -3.941649 ymax: 51.63971
#> Geodetic CRS:  WGS 84
#>    msoa11cd    msoa11nm    msoa11nmw                 msoa11hclnm
#> 1 W02000187 Swansea 020 Abertawe 020      Dunvant & Upper Killay
#> 2 W02000185 Swansea 018 Abertawe 018 Llanmorlais & Three Crosses
#> 3 W02000184 Swansea 017 Abertawe 017                     Cockett
#> 4 W02000183 Swansea 016 Abertawe 016                     Landore
#> 5 W02000182 Swansea 015 Abertawe 015                    Cwmbwrla
#>            msoa11hclnmw   lad20cd lad20nm                   geometry
#> 1 Dynfant a Chilâ Uchaf W06000011 Swansea POINT (-4.031534 51.62307)
#> 2 Llanmorlais a'r Crwys W06000011 Swansea POINT (-4.101869 51.63796)
#> 3               Y Cocyd W06000011 Swansea POINT (-3.980032 51.63668)
#> 4               Glandwr W06000011 Swansea POINT (-3.941649 51.63632)
#> 5              Cwmbwrla W06000011 Swansea POINT (-3.951766 51.63971)
```

``` r
geo_get("lsoa", "Zetland", "ward", shape_fields = TRUE)
#> Simple feature collection with 3 features and 6 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -1.059949 ymin: 54.6023 xmax: -1.036318 ymax: 54.61806
#> Geodetic CRS:  WGS 84
#>    lsoa11cd                  lsoa11nm    wd20cd  wd20nm shape_area shape_length
#> 1 E01012184 Redcar and Cleveland 001D E05012460 Zetland   262702.7     2949.721
#> 2 E01012185 Redcar and Cleveland 004F E05012460 Zetland   189191.0     2470.998
#> 3 E01012186 Redcar and Cleveland 004G E05012460 Zetland   518788.5     4897.473
#>                         geometry
#> 1 POLYGON ((-1.053543 54.6166...
#> 2 POLYGON ((-1.047871 54.6075...
#> 3 POLYGON ((-1.0503 54.61492,...
```

``` r
geo_get(bounds_level = "lad",
  within = "Gloucestershire",
  within_level = "cty",
  return_style = "simple",
  return_boundaries = FALSE)
#> # A tibble: 6 x 4
#>   lad20cd   lad20nm        cty20cd   cty20nm        
#>   <chr>     <chr>          <chr>     <chr>          
#> 1 E07000078 Cheltenham     E10000013 Gloucestershire
#> 2 E07000079 Cotswold       E10000013 Gloucestershire
#> 3 E07000080 Forest of Dean E10000013 Gloucestershire
#> 4 E07000081 Gloucester     E10000013 Gloucestershire
#> 5 E07000082 Stroud         E10000013 Gloucestershire
#> 6 E07000083 Tewkesbury     E10000013 Gloucestershire
```

Return a bare API query ready to be run or checked externally:

``` r
build_api_query(
  # currently you just have to know which ref to use - see build_api_query.R
  ref = 4, 
  where_level = "cauth20nm",
  where = "Greater Manchester",
  sr = 27700
)
#> [1] "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD20_CAUTH20_EN_LU/FeatureServer/0/query?where=%20(CAUTH20NM%20=%20'GREATER%20MANCHESTER')%20&outFields=*&resultType=standard&returnDistinctValues=true&f=json"
```

## Contributing

Suggestions are welcome, preferably posted as an issue on GitHub.
Contributions as pull requests are also welcome.

You are also welcome to email me with comments or ideas. I’d be glad of
suggestions for improvement, or extra features.

Improvements to the naming of key functions and their parameters
especially welcome. Contact details are on my GitHub profile.

This project has a Contributor Code of Conduct:

### Code of Conduct

Please note that the `jogger` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## News

See [NEWS.md](NEWS.md) for version notes

### Licences

The code in this repo is MIT licensed.

The data that the code helps you retrieve is issued under a variety of
licences, including:

-   the [Open Government Licence
    v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)

> Licensing statement [as stipulated by the
> ONS](https://www.ons.gov.uk/methodology/geography/licences):
>
> -   Source: Office for National Statistics licensed under the [Open
>     Government Licence
>     v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)
> -   Contains OS data © Crown copyright and database right 2021.
