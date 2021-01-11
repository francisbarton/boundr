
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
library(tmap)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

tmap::tmap_mode("plot")
#> tmap mode set to plotting

geo_get("wd", "Swindon", "lad") %>% 
  tmap::qtm()
#> Reading layer `ESRIJSON' from data source `https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Wards_December_2020_UK_BFC/FeatureServer/0/query?where=%20(WD20CD%20%3D%20'E05008962'%20OR%20WD20CD%20%3D%20'E05008963'%20OR%20WD20CD%20%3D%20'E05010757'%20OR%20WD20CD%20%3D%20'E05008965'%20OR%20WD20CD%20%3D%20'E05008966'%20OR%20WD20CD%20%3D%20'E05010755'%20OR%20WD20CD%20%3D%20'E05008967'%20OR%20WD20CD%20%3D%20'E05008954'%20OR%20WD20CD%20%3D%20'E05008968'%20OR%20WD20CD%20%3D%20'E05008955'%20OR%20WD20CD%20%3D%20'E05008969'%20OR%20WD20CD%20%3D%20'E05008970'%20OR%20WD20CD%20%3D%20'E05008971'%20OR%20WD20CD%20%3D%20'E05008972'%20OR%20WD20CD%20%3D%20'E05008956'%20OR%20WD20CD%20%3D%20'E05008957'%20OR%20WD20CD%20%3D%20'E05008958'%20OR%20WD20CD%20%3D%20'E05010756'%20OR%20WD20CD%20%3D%20'E05008960'%20OR%20WD20CD%20%3D%20'E05008961')%20&outFields=WD20CD,WD20NM&outSR=4326&f=json' using driver `ESRIJSON'
#> Simple feature collection with 20 features and 2 fields
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: -1.865128 ymin: 51.48245 xmax: -1.602812 ymax: 51.69271
#> geographic CRS: WGS 84
#> Joining, by = c("wd20cd", "wd20nm")
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
#> 1  W02000185 Swansea 018 Abertawe 018 Llanmorlais & Three Crosses
#> 2  W02000195 Swansea 028 Abertawe 028         Mayals & Bishopston
#> 3  W02000178 Swansea 011 Abertawe 011                    Penderry
#> 4  W02000198 Swansea 031 Abertawe 031            Mumbles & Newton
#> 5  W02000198 Swansea 031 Abertawe 031            Mumbles & Newton
#> 6  W02000187 Swansea 020 Abertawe 020      Dunvant & Upper Killay
#> 7  W02000183 Swansea 016 Abertawe 016                     Landore
#> 8  W02000179 Swansea 012 Abertawe 012                    Gowerton
#> 9  W02000180 Swansea 013 Abertawe 013                   Ravenhill
#> 10 W02000171 Swansea 004 Abertawe 004      Llangyfelach & Tircoed
#>                   msoa11hclnmw    wd20cd       wd20nm   lad20cd lad20nm
#> 1        Llanmorlais a'r Crwys W05000974    Penclawdd W06000011 Swansea
#> 2  Mayals a Llandeilo Ferwallt W05000959   Bishopston W06000011 Swansea
#> 3                      Penderi W05000975     Penderry W06000011 Swansea
#> 4             Mwmbwls a Newton W05000538  Oystermouth W06000011 Swansea
#> 5             Mwmbwls a Newton W05000537       Newton W06000011 Swansea
#> 6        Dynfant a Chilâ Uchaf W05000965      Dunvant W06000011 Swansea
#> 7                      Glandwr W05000969      Landore W06000011 Swansea
#> 8                      Tregwyr W05000525     Gowerton W06000011 Swansea
#> 9                    Ravenhill W05000963      Cockett W06000011 Swansea
#> 10     Llangyfelach a Thircoed W05000970 Llangyfelach W06000011 Swansea
#>    lad20nmw
#> 1  Abertawe
#> 2  Abertawe
#> 3  Abertawe
#> 4  Abertawe
#> 5  Abertawe
#> 6  Abertawe
#> 7  Abertawe
#> 8  Abertawe
#> 9  Abertawe
#> 10 Abertawe
```

``` r
geo_get("lsoa", "Zetland", "ward", shape_fields = TRUE) %>%
  dplyr::arrange(desc(shape_area))
#> Reading layer `ESRIJSON' from data source `https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC_V2/FeatureServer/0/query?where=%20(LSOA11CD%20%3D%20'E01012184'%20OR%20LSOA11CD%20%3D%20'E01012185'%20OR%20LSOA11CD%20%3D%20'E01012186')%20&outFields=LSOA11CD,LSOA11NM,SHAPE__AREA,SHAPE__LENGTH&outSR=4326&f=json' using driver `ESRIJSON'
#> Simple feature collection with 3 features and 4 fields
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: -1.059949 ymin: 54.6023 xmax: -1.036318 ymax: 54.61806
#> geographic CRS: WGS 84
#> Joining, by = c("lsoa11cd", "lsoa11nm")
#> Simple feature collection with 3 features and 6 fields
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: -1.059949 ymin: 54.6023 xmax: -1.036318 ymax: 54.61806
#> geographic CRS: WGS 84
#>    lsoa11cd                  lsoa11nm shape_area shape_length    wd20cd  wd20nm
#> 1 E01012186 Redcar and Cleveland 004G   510045.3     5069.503 E05012460 Zetland
#> 2 E01012184 Redcar and Cleveland 001D   265580.9     3023.669 E05012460 Zetland
#> 3 E01012185 Redcar and Cleveland 004F   189456.1     2617.889 E05012460 Zetland
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
#> [1] "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD19_CAUTH19_EN_LU/FeatureServer/0/query?where=%20(CAUTH19NM%20%3D%20%27GREATER%20MANCHESTER%27)%20&outFields=*&returnDistinctValues=true&outSR=4326&f=json"
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
