
<!-- README.md is generated from README.Rmd. Please edit that file -->

# boundr

<!-- badges: start -->
<!-- badges: end -->

### Retrieve area boundaries and data from the ONS Open Geography Portal

`boundr` was previously
[`jogger`](https://github.com/francisbarton/jogger).

The main purpose of this package is to download area lookups and
boundaries (in GeoJSON format) using the ONS Open Geography API, for all
areas at a specified level within a specified area.

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
       |                 <--------- pull_lookup_query_url() 
       |                                       \
       |                                        \
        <-------  return_bounds_data()           <----- opengeo_schema [data]
                        ^                       /              ^
                        |                      /               |
                         <--------  pull_geo_query_url()       |
                                                               |
                                                         build_schema()

The main script will return a data frame with the sub-area geometry
column, as an `sf` object ready to be visualised as a map.

## Installation

You can install this package from the `R` console by entering

    remotes::install_github("francisbarton/boundr")

if you have the `remotes` package installed.

## Examples

`jogger` examples removed. New `boundr` examples to come!

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

-   the [Open Government Licence
    v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)

> Licensing statement [as stipulated by the
> ONS](https://www.ons.gov.uk/methodology/geography/licences):
>
> -   Source: Office for National Statistics licensed under the [Open
>     Government Licence
>     v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)
> -   Contains OS data © Crown copyright and database right 2021.
