
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jogger

<!-- badges: start -->
<!-- badges: end -->

### Retrieve area boundaries and data from the ONS Open Geography Portal

The main function of this package is to download area lookups and
boundaries (in GeoJSON format) using the ONS Open Geography API, for all
sub-areas - at a specified level - within a specified area. The main
script will return a data frame with the sub-area geometry column, as an
`sf` object ready to be visualised as a map. It’s initially just for
areas within England and Wales, with a hope to add Scottish geographies
at a later date. And currently just for the following area levels: LSOA,
MSOA, Ward, LAD/LTLA, UTLA/CTY, CAUTH, RGN *\[subject to change\]*

## Installation

    remotes::install_github("francisbarton/jogger")

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
geo_get("msoa", "Swansea", "lad", centroid_fields = TRUE) %>%
  head(10)
#> 
#> -- Column specification --------------------------------------------------------
#> cols(
#>   msoa11cd = col_character(),
#>   msoa11nm = col_character(),
#>   msoa11nmw = col_character(),
#>   msoa11hclnm = col_character(),
#>   msoa11hclnmw = col_character(),
#>   Laname = col_character()
#> )
#> Joining, by = "msoa11nm"
#> Joining, by = c("lsoa11cd", "lsoa11nm")
#> Joining, by = c("lsoa11cd", "lsoa11nm")
#> Simple feature collection with 10 features and 7 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -4.081328 ymin: 51.64419 xmax: -3.842743 ymax: 51.77423
#> Geodetic CRS:  WGS 84
#>     msoa11cd    msoa11nm    msoa11nmw            msoa11hclnm
#> 1  W02000169 Swansea 002 Abertawe 002         Clydach & Mawr
#> 2  W02000170 Swansea 003 Abertawe 003        Morriston North
#> 3  W02000171 Swansea 004 Abertawe 004 Llangyfelach & Tircoed
#> 4  W02000172 Swansea 005 Abertawe 005              Gorseinon
#> 5  W02000173 Swansea 006 Abertawe 006             Birchgrove
#> 6  W02000174 Swansea 007 Abertawe 007                Loughor
#> 7  W02000175 Swansea 008 Abertawe 008        Morriston South
#> 8  W02000176 Swansea 009 Abertawe 009            Mynydd-bach
#> 9  W02000177 Swansea 010 Abertawe 010             Llansamlet
#> 10 W02000178 Swansea 011 Abertawe 011               Penderry
#>               msoa11hclnmw   lad20cd lad20nm                       geometry
#> 1           Clydach a Mawr W06000011 Swansea MULTIPOLYGON (((-3.998642 5...
#> 2         Gogledd Treforys W06000011 Swansea MULTIPOLYGON (((-3.912776 5...
#> 3  Llangyfelach a Thircoed W06000011 Swansea MULTIPOLYGON (((-4.01002 51...
#> 4                Gorseinon W06000011 Swansea MULTIPOLYGON (((-4.05016 51...
#> 5                Gellifedw W06000011 Swansea MULTIPOLYGON (((-3.873473 5...
#> 6               Casllwchwr W06000011 Swansea MULTIPOLYGON (((-4.07236 51...
#> 7              De Treforys W06000011 Swansea MULTIPOLYGON (((-3.929079 5...
#> 8              Mynydd-bach W06000011 Swansea MULTIPOLYGON (((-3.937648 5...
#> 9               Llansamlet W06000011 Swansea MULTIPOLYGON (((-3.896356 5...
#> 10                 Penderi W06000011 Swansea MULTIPOLYGON (((-3.954672 5...
```

``` r
geo_get("lsoa", "Zetland", "ward", shape_fields = TRUE)
#> Simple feature collection with 3 features and 6 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -1.059949 ymin: 54.6023 xmax: -1.036318 ymax: 54.61806
#> Geodetic CRS:  WGS 84
#>    lsoa11cd                  lsoa11nm    wd20cd  wd20nm shape_area shape_length
#> 1 E01012184 Redcar and Cleveland 001D E05012460 Zetland   265580.9     3023.669
#> 2 E01012185 Redcar and Cleveland 004F E05012460 Zetland   189456.1     2617.889
#> 3 E01012186 Redcar and Cleveland 004G E05012460 Zetland   510045.3     5069.503
#>                         geometry
#> 1 POLYGON ((-1.053543 54.6166...
#> 2 POLYGON ((-1.047838 54.6074...
#> 3 POLYGON ((-1.047691 54.6129...
```

``` r
geo_get(bounds_level = "lad",
  within = "Gloucestershire",
  within_level = "cty",
  return_style = "simple",
  return_boundaries = FALSE)
#>     lad20cd        lad20nm   cty20cd         cty20nm
#> 1 E07000078     Cheltenham E10000013 Gloucestershire
#> 2 E07000079       Cotswold E10000013 Gloucestershire
#> 3 E07000080 Forest of Dean E10000013 Gloucestershire
#> 4 E07000081     Gloucester E10000013 Gloucestershire
#> 5 E07000082         Stroud E10000013 Gloucestershire
#> 6 E07000083     Tewkesbury E10000013 Gloucestershire
```

Return a bare API query ready to be run or checked externally:

``` r
build_api_query(
  # currently you just have to know which ref to use - see build_api_query.R
  ref = 4, 
  within_level = "cauth20nm",
  within = "Greater Manchester"
)
#> [1] "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD20_CAUTH20_EN_LU/FeatureServer/0/query?where=%20(CAUTH20NM%20%3D%20'GREATER%20MANCHESTER')%20&outFields=*&outSR=4326&f=json&returnDistinctValues=true"
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

See NEWS.md for version notes

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
