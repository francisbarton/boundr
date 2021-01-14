
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
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tmap)

tmap::tmap_mode("plot")
#> tmap mode set to plotting

geo_get("wd", "Swindon", "lad") %>% 
  tmap::tm_shape() +
  tmap::tm_borders()
#> Reading layer `ESRIJSON' from data source `https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Wards_December_2020_UK_BFC/FeatureServer/0/query?where=%20(WD20CD%20%3D%20'E05008962'%20OR%20WD20CD%20%3D%20'E05008963'%20OR%20WD20CD%20%3D%20'E05010757'%20OR%20WD20CD%20%3D%20'E05008965'%20OR%20WD20CD%20%3D%20'E05008966'%20OR%20WD20CD%20%3D%20'E05010755'%20OR%20WD20CD%20%3D%20'E05008967'%20OR%20WD20CD%20%3D%20'E05008954'%20OR%20WD20CD%20%3D%20'E05008968'%20OR%20WD20CD%20%3D%20'E05008955'%20OR%20WD20CD%20%3D%20'E05008969'%20OR%20WD20CD%20%3D%20'E05008970'%20OR%20WD20CD%20%3D%20'E05008971'%20OR%20WD20CD%20%3D%20'E05008972'%20OR%20WD20CD%20%3D%20'E05008956'%20OR%20WD20CD%20%3D%20'E05008957'%20OR%20WD20CD%20%3D%20'E05008958'%20OR%20WD20CD%20%3D%20'E05010756'%20OR%20WD20CD%20%3D%20'E05008960'%20OR%20WD20CD%20%3D%20'E05008961')%20&outFields=WD20CD&outSR=4326&f=json' using driver `ESRIJSON'
#> Simple feature collection with 20 features and 1 field
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: -1.865128 ymin: 51.48245 xmax: -1.602812 ymax: 51.69271
#> geographic CRS: WGS 84
#> Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1
```

<img src="man/figures/README-example-1-1.png" width="100%" />

``` r
geo_get("msoa", "Swansea", "lad", return_boundaries = FALSE) %>% 
  dplyr::slice_sample(n = 10)
#> Joining, by = c("lad20cd", "lad20nm")
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
#>     msoa11cd    msoa11nm    msoa11nmw                 msoa11hclnm
#> 1  W02000194 Swansea 027 Abertawe 027                      Sketty
#> 2  W02000179 Swansea 012 Abertawe 012                    Gowerton
#> 3  W02000171 Swansea 004 Abertawe 004      Llangyfelach & Tircoed
#> 4  W02000186 Swansea 019 Abertawe 019                    Townhill
#> 5  W02000174 Swansea 007 Abertawe 007                     Loughor
#> 6  W02000184 Swansea 017 Abertawe 017                     Cockett
#> 7  W02000176 Swansea 009 Abertawe 009                 Mynydd-bach
#> 8  W02000189 Swansea 022 Abertawe 022                     Ty-coch
#> 9  W02000185 Swansea 018 Abertawe 018 Llanmorlais & Three Crosses
#> 10 W02000178 Swansea 011 Abertawe 011                    Penderry
#>               msoa11hclnmw   lad20cd lad20nm lad20nmw
#> 1                    Sgeti W06000011 Swansea Abertawe
#> 2                  Tregwyr W06000011 Swansea Abertawe
#> 3  Llangyfelach a Thircoed W06000011 Swansea Abertawe
#> 4                 Townhill W06000011 Swansea Abertawe
#> 5               Casllwchwr W06000011 Swansea Abertawe
#> 6                  Y Cocyd W06000011 Swansea Abertawe
#> 7              Mynydd-bach W06000011 Swansea Abertawe
#> 8                  Ty-coch W06000011 Swansea Abertawe
#> 9    Llanmorlais a'r Crwys W06000011 Swansea Abertawe
#> 10                 Penderi W06000011 Swansea Abertawe
```

``` r
geo_get("lsoa", "Zetland", "ward", shape_fields = TRUE) %>%
  dplyr::arrange(desc(shape_area))
#> Reading layer `ESRIJSON' from data source `https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC_V2/FeatureServer/0/query?where=%20(LSOA11CD%20%3D%20'E01012184'%20OR%20LSOA11CD%20%3D%20'E01012185'%20OR%20LSOA11CD%20%3D%20'E01012186')%20&outFields=LSOA11CD,SHAPE__AREA,SHAPE__LENGTH&outSR=4326&f=json' using driver `ESRIJSON'
#> Simple feature collection with 3 features and 3 fields
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: -1.059949 ymin: 54.6023 xmax: -1.036318 ymax: 54.61806
#> geographic CRS: WGS 84
#> Simple feature collection with 3 features and 6 fields
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: -1.059949 ymin: 54.6023 xmax: -1.036318 ymax: 54.61806
#> geographic CRS: WGS 84
#>    lsoa11cd shape_area shape_length                  lsoa11nm    wd20cd  wd20nm
#> 1 E01012186   510045.3     5069.503 Redcar and Cleveland 004G E05012460 Zetland
#> 2 E01012184   265580.9     3023.669 Redcar and Cleveland 001D E05012460 Zetland
#> 3 E01012185   189456.1     2617.889 Redcar and Cleveland 004F E05012460 Zetland
#>                         geometry
#> 1 POLYGON ((-1.047691 54.6129...
#> 2 POLYGON ((-1.053543 54.6166...
#> 3 POLYGON ((-1.047838 54.6074...
```

``` r
geo_get(bounds_level = "lad",
        within = "Gloucestershire",
        within_level = "cty",
        return_style = "simple",
        centroid_fields = TRUE,
        return_boundaries = FALSE)
#>     lad20cd        lad20nm   cty20cd         cty20nm
#> 1 E07000082         Stroud E10000013 Gloucestershire
#> 2 E07000080 Forest of Dean E10000013 Gloucestershire
#> 3 E07000078     Cheltenham E10000013 Gloucestershire
#> 4 E07000081     Gloucester E10000013 Gloucestershire
#> 5 E07000079       Cotswold E10000013 Gloucestershire
#> 6 E07000083     Tewkesbury E10000013 Gloucestershire
```

Return a bare API query ready to be run or checked externally:

``` r
build_api_query(
  # currently you just have to know which ref to use - see build_api_query.R
  table_code_ref = 2, 
  within_level = "cauth19nm",
  within = "Greater Manchester"
)
#> [1] "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD20_CAUTH20_EN_LU/FeatureServer/0/query?where=%20(CAUTH19NM%20%3D%20'GREATER%20MANCHESTER')%20&outFields=*&outSR=4326&f=json"
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
