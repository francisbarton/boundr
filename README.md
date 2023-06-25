
<!-- README.md is generated from README.Rmd. Please edit that file -->

# welcome to boundr

<!-- badges: start -->
<!-- badges: end -->

### Retrieve area boundaries and data from the ONS Open Geography Portal

`boundr` was previously
[`jogger`](https://github.com/francisbarton/jogger).

The main purpose of this package is to download area lookups and
boundaries (in GeoJSON format) using the ONS Open Geography API, for all
areas at a specified level within a specified area.

The main script will return a data frame with the sub-area geometry
column, as an `sf` object ready to be visualised as a map.

### Package logic

The structure of the project looks a bit like this:

    bounds() [main UI function]
       ^
       |
       |
        <-------  create_lookup_table()
       |                ^
       |                |
       |                 <--------- return_query_data()
       |                |
       |                 <--------- return_lookup_query_info() 
       |                                       \
       |                                        \
        <-------  return_bounds_data()           <----- opengeo_schema [data]
                        ^                       /              ^
                        |                      /               |
                         <--------  pull_geo_query_url()       |
                                                               |
                                                         build_schema()

When you call `bounds()` you specify a lower level area (eg ward) and a
higher level area (eg local authority), and you specify either the name
of the higher level area (or areas) or its code.

`return_lookup_query_info()` then finds the API query URL of a suitable
lookup table - one that contains columns for both your lower and higher
level areas. It does this by filtering `opengeo_schema`, which is a
cached copy of the various datasets available from the Open Geography
API Services list. This schema is available as internal data in the
package - but may need updating.

`create_lookup_table()` then builds a lookup table (a tibble) based on
all the areas you have said you are interested in. At the same time,
`return_bounds_data()` will - if you have specified you want spatial
boundaries data for your areas - retrieve the boundary data at your
chosen resolution for your lower level areas. These will then be joined
onto the lookup table and provided to you as an `sf` tibble.

## Installation

You can install this package from the `R` console by entering

`remotes::install_github("francisbarton/boundr")`

if you have the `remotes` package installed.

## Examples

### Basic lookup of areas within a larger area, by name

Returns sfc tibble with latest available data

``` r
bounds("msoa", "lad", "Swansea")
#> Simple feature collection with 150 features and 4 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -4.333587 ymin: 51.53577 xmax: -3.842747 ymax: 51.77423
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>     lsoa21cd     lsoa21nm   lad23cd lad23nm                       geometry
#> 1  W01000736 Swansea 028A W06000011 Swansea POLYGON ((-4.040871 51.5853...
#> 2  W01000737 Swansea 028B W06000011 Swansea MULTIPOLYGON (((-4.0361 51....
#> 3  W01000738 Swansea 014A W06000011 Swansea POLYGON ((-3.874889 51.6505...
#> 4  W01000739 Swansea 014B W06000011 Swansea POLYGON ((-3.914009 51.6401...
#> 5  W01000740 Swansea 014C W06000011 Swansea POLYGON ((-3.904865 51.6542...
#> 6  W01000741 Swansea 014D W06000011 Swansea POLYGON ((-3.90558 51.65353...
#> 7  W01000742 Swansea 016A W06000011 Swansea POLYGON ((-3.943777 51.6373...
#> 8  W01000744 Swansea 016B W06000011 Swansea POLYGON ((-3.948747 51.6326...
#> 9  W01000745 Swansea 025B W06000011 Swansea POLYGON ((-3.946384 51.6211...
#> 10 W01000746 Swansea 025C W06000011 Swansea POLYGON ((-3.946375 51.6278...
```

### Lookup areas with older dates

Trial and error may be required - or start by looking up what is
available on the OpenGeography site.

``` r
bounds("msoa", "lad", "Shepway", lookup_year = 2011, within_year = 2015)
#> Simple feature collection with 67 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 0.7770266 ymin: 50.91095 xmax: 1.221137 ymax: 51.205
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>     lsoa11cd     lsoa11nm   lad15cd lad15nm                       geometry
#> 1  E01024486 Shepway 011A E07000112 Shepway POLYGON ((0.9871731 51.0385...
#> 2  E01024487 Shepway 011B E07000112 Shepway POLYGON ((0.977373 51.0179,...
#> 3  E01024488 Shepway 011C E07000112 Shepway POLYGON ((0.9724648 51.0127...
#> 4  E01024489 Shepway 009A E07000112 Shepway POLYGON ((1.02856 51.05629,...
#> 5  E01024490 Shepway 001A E07000112 Shepway POLYGON ((1.089762 51.18973...
#> 6  E01024491 Shepway 005A E07000112 Shepway POLYGON ((1.13853 51.0908, ...
#> 7  E01024492 Shepway 002A E07000112 Shepway POLYGON ((1.141129 51.10232...
#> 8  E01024493 Shepway 005B E07000112 Shepway POLYGON ((1.146521 51.09351...
#> 9  E01024494 Shepway 005C E07000112 Shepway POLYGON ((1.135277 51.09801...
#> 10 E01024495 Shepway 005D E07000112 Shepway POLYGON ((1.1419 51.08616, ...
```

### Alternative ways to achieve a goal

In this case, the first option is notably quicker than the second,
because they use different tables to create the lookup table.

The second option fails to return boundaries unless the year option is
included, as it creates a lookup table for rgn23 and ctry23 but
boundaries are not available for rgn23 (only rgn22).

Different columns are returned by each command.

``` r
bounds("rgn", country_filter = "EN", resolution = "BUC")
#> Simple feature collection with 9 features and 8 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -6.419011 ymin: 49.86463 xmax: 1.768937 ymax: 55.81166
#> Geodetic CRS:  WGS 84
#>     rgn21cd                  rgn21nm  bng_e  bng_n      long      lat
#> 1 E12000007                   London 517515 178392 -0.308660 51.49227
#> 2 E12000001               North East 417314 600356 -1.728880 55.29701
#> 3 E12000005            West Midlands 386294 295477 -2.203580 52.55697
#> 4 E12000003 Yorkshire and The Humber 446902 448736 -1.287140 53.93264
#> 5 E12000006          East of England 571078 263235  0.504207 52.24073
#> 6 E12000004            East Midlands 477659 322635 -0.849690 52.79572
#> 7 E12000002               North West 350014 506279 -2.772390 54.44944
#> 8 E12000008               South East 470062 172924 -0.993110 51.45097
#> 9 E12000009               South West 285013 102567 -3.633460 50.81119
#>    shape_area shape_length                       geometry
#> 1  1594697003     256347.9 POLYGON ((-0.0968623 51.690...
#> 2  8675517225     804893.9 MULTIPOLYGON (((-1.540483 5...
#> 3 13003737567     953773.4 POLYGON ((-1.958916 53.2166...
#> 4 15560353265     982047.7 POLYGON ((-0.7829333 54.562...
#> 5 19584692923    1047595.8 MULTIPOLYGON (((1.604498 52...
#> 6 15810723352    1066495.4 POLYGON ((-0.2997642 53.614...
#> 7 14913854032    1066984.7 POLYGON ((-2.675591 55.1733...
#> 8 19399927121    1389475.4 MULTIPOLYGON (((-1.590507 5...
#> 9 24385283189    2378388.7 MULTIPOLYGON (((-6.398846 4...
bounds("rgn", "ctry", "England", lookup_year = 2022, resolution = "BUC")
#> Lookup table data ■■■■ 9% | ETA: 12sLookup table data ■■■■ 10% | ETA: 15sLookup
#> table data ■■■■ 11% | ETA: 14sLookup table data ■■■■■ 13% | ETA: 13sLookup
#> table data ■■■■■ 14% | ETA: 13sLookup table data ■■■■■■ 16% | ETA: 12sLookup
#> table data ■■■■■■ 17% | ETA: 12sLookup table data ■■■■■■■ 20% | ETA: 11sLookup
#> table data ■■■■■■■ 21% | ETA: 11sLookup table data ■■■■■■■■ 23% | ETA:
#> 11sLookup table data ■■■■■■■■ 24% | ETA: 10sLookup table data ■■■■■■■■■ 26% |
#> ETA: 10sLookup table data ■■■■■■■■■ 27% | ETA: 10sLookup table data ■■■■■■■■■■
#> 30% | ETA: 9sLookup table data ■■■■■■■■■■ 31% | ETA: 9sLookup table data
#> ■■■■■■■■■■■ 33% | ETA: 9sLookup table data ■■■■■■■■■■■ 34% | ETA: 9sLookup
#> table data ■■■■■■■■■■■■ 36% | ETA: 9sLookup table data ■■■■■■■■■■■■ 37% | ETA:
#> 8sLookup table data ■■■■■■■■■■■■■ 39% | ETA: 8sLookup table data ■■■■■■■■■■■■■
#> 40% | ETA: 8sLookup table data ■■■■■■■■■■■■■ 41% | ETA: 8sLookup table data
#> ■■■■■■■■■■■■■■ 43% | ETA: 8sLookup table data ■■■■■■■■■■■■■■ 44% | ETA:
#> 7sLookup table data ■■■■■■■■■■■■■■■ 46% | ETA: 7sLookup table data
#> ■■■■■■■■■■■■■■■ 47% | ETA: 7sLookup table data ■■■■■■■■■■■■■■■■ 50% | ETA:
#> 7sLookup table data ■■■■■■■■■■■■■■■■ 51% | ETA: 6sLookup table data
#> ■■■■■■■■■■■■■■■■■ 53% | ETA: 6sLookup table data ■■■■■■■■■■■■■■■■■ 54% | ETA:
#> 6sLookup table data ■■■■■■■■■■■■■■■■■■ 56% | ETA: 6sLookup table data
#> ■■■■■■■■■■■■■■■■■■ 57% | ETA: 6sLookup table data ■■■■■■■■■■■■■■■■■■■ 60% |
#> ETA: 5sLookup table data ■■■■■■■■■■■■■■■■■■■ 61% | ETA: 5sLookup table data
#> ■■■■■■■■■■■■■■■■■■■■ 63% | ETA: 5sLookup table data ■■■■■■■■■■■■■■■■■■■■ 64% |
#> ETA: 5sLookup table data ■■■■■■■■■■■■■■■■■■■■■ 66% | ETA: 4sLookup table data
#> ■■■■■■■■■■■■■■■■■■■■■ 67% | ETA: 4sLookup table data ■■■■■■■■■■■■■■■■■■■■■■ 69%
#> | ETA: 5sLookup table data ■■■■■■■■■■■■■■■■■■■■■■ 70% | ETA: 4sLookup table
#> data ■■■■■■■■■■■■■■■■■■■■■■ 71% | ETA: 4sLookup table data
#> ■■■■■■■■■■■■■■■■■■■■■■■ 73% | ETA: 4sLookup table data ■■■■■■■■■■■■■■■■■■■■■■■
#> 74% | ETA: 4sLookup table data ■■■■■■■■■■■■■■■■■■■■■■■■ 76% | ETA: 3sLookup
#> table data ■■■■■■■■■■■■■■■■■■■■■■■■ 77% | ETA: 3sLookup table data
#> ■■■■■■■■■■■■■■■■■■■■■■■■■ 80% | ETA: 3sLookup table data
#> ■■■■■■■■■■■■■■■■■■■■■■■■■ 81% | ETA: 3sLookup table data
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■ 83% | ETA: 2sLookup table data
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■ 84% | ETA: 2sLookup table data
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■ 87% | ETA: 2sLookup table data
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 89% | ETA: 2sLookup table data
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 90% | ETA: 1sLookup table data
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 91% | ETA: 1sLookup table data
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 93% | ETA: 1sLookup table data
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 96% | ETA: 1sLookup table data
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 97% | ETA: 0sLookup table data
#> ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 99% | ETA: 0s
#> Simple feature collection with 9 features and 4 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -6.360297 ymin: 49.88234 xmax: 1.763706 ymax: 55.81121
#> Geodetic CRS:  WGS 84
#>     rgn22cd                  rgn22nm  ctry22cd ctry22nm
#> 1 E12000001               North East E92000001  England
#> 2 E12000002               North West E92000001  England
#> 3 E12000003 Yorkshire and The Humber E92000001  England
#> 4 E12000004            East Midlands E92000001  England
#> 5 E12000005            West Midlands E92000001  England
#> 6 E12000006          East of England E92000001  England
#> 7 E12000007                   London E92000001  England
#> 8 E12000008               South East E92000001  England
#> 9 E12000009               South West E92000001  England
#>                         geometry
#> 1 MULTIPOLYGON (((-1.779088 5...
#> 2 MULTIPOLYGON (((-3.242936 5...
#> 3 MULTIPOLYGON (((-0.5207704 ...
#> 4 POLYGON ((-0.248476 53.5936...
#> 5 POLYGON ((-1.860732 53.1884...
#> 6 MULTIPOLYGON (((0.8374914 5...
#> 7 POLYGON ((-0.01191868 51.68...
#> 8 MULTIPOLYGON (((-1.55515 50...
#> 9 MULTIPOLYGON (((-6.349051 4...
```

### A lookup table for Welsh local authorities to Senedd electoral regions

Using `return_with = "full"` includes the Welsh language data columns.

``` r
create_lookup_table("ua", "sener", return_width = "full") |>
  dplyr::filter(sener22nm == "South Wales West")
#> # A tibble: 4 × 7
#>   wd22cd    wd22nm               ua22cd    ua22nm    ua22nmw sener22cd sener22nm
#>   <chr>     <chr>                <chr>     <chr>     <chr>   <chr>     <chr>    
#> 1 W05001076 Brynna and Llanharan W06000016 Rhondda … Rhondd… W10000009 South Wa…
#> 2 W05001084 Glyn-coch            W06000016 Rhondda … Rhondd… W10000009 South Wa…
#> 3 W05001088 Llanharry            W06000016 Rhondda … Rhondd… W10000009 South Wa…
#> 4 W05001099 Pontyclun West       W06000016 Rhondda … Rhondd… W10000009 South Wa…
```

``` r
bounds("parish", "utla", "Isles of Scilly") |>
  tmap::qtm()
```

<img src="man/figures/README-example-5-1.png" width="100%" />

``` r
bounds("spr")
#> Simple feature collection with 8 features and 8 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -8.649603 ymin: 54.63326 xmax: -0.7546071 ymax: 60.84569
#> Geodetic CRS:  WGS 84
#>     spr22cd               spr22nm  bng_e  bng_n     long      lat  shape_area
#> 1 S17000013 Mid Scotland and Fife 282520 731011 -3.90787 56.45655  9129823415
#> 2 S17000011 Highlands and Islands 244281 863126 -4.60963 57.63108 40126955263
#> 3 S17000020               Glasgow 264285 664694 -4.16969 55.85627   207721111
#> 4 S17000014   North East Scotland 352284 799608 -2.78889 57.08497  8957071558
#> 5 S17000019      Central Scotland 281356 669376 -3.89920 55.90282   940131195
#> 6 S17000018         West Scotland 239229 655570 -4.56416 55.76667  2288753142
#> 7 S17000015        South Scotland 300085 608202 -3.57755 55.35763 16115290761
#> 8 S17000012               Lothian 316704 666932 -3.33329 55.88829   858325682
#>   shape_length                       geometry
#> 1    612257.00 POLYGON ((-3.894724 56.9227...
#> 2   8790966.43 MULTIPOLYGON (((-5.572362 5...
#> 3     87267.24 POLYGON ((-4.276113 55.9296...
#> 4    561602.62 POLYGON ((-2.847909 57.7061...
#> 5    235997.35 POLYGON ((-3.733132 56.0618...
#> 6    563692.94 MULTIPOLYGON (((-5.07178 55...
#> 7    947497.50 MULTIPOLYGON (((-5.106759 5...
#> 8    193925.75 POLYGON ((-3.425965 55.9938...
```

``` r
sb <- bounds("msoa", "lad", "Swindon")
sp <- points("msoa", "utla", "Swindon")

sb |>
  ggplot2::ggplot() +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = sp, colour = "orange") +
  ggplot2::theme_void()
```

<img src="man/figures/README-example-7-1.png" width="100%" />

``` r
bounds("npark", within_names = "Bannau Brycheiniog", resolution = "BUC") |>
  ggplot2::ggplot() +
  ggplot2::geom_sf()
```

<img src="man/figures/README-example-8-1.png" width="100%" />

## Contributing

Suggestions for improvements are welcome, preferably posted as an issue
here on GitHub. Contributions as pull requests are also welcome.

Please note that `boundr` is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## News

See [NEWS.md](NEWS.md) for version notes.

### Licences

The code in this repo is MIT licensed.

The data that the code helps you retrieve is issued under a variety of
licences, including:

- the [Open Government Licence
  v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)

> Licensing statement [as stipulated by the
> ONS](https://www.ons.gov.uk/methodology/geography/licences):
>
> - Source: Office for National Statistics licensed under the [Open
>   Government Licence
>   v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)
> - Contains OS data © Crown copyright and database right 2021.
