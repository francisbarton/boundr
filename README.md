
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

- the [Open Government Licence
  v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)

> Licensing statement [as stipulated by the
> ONS](https://www.ons.gov.uk/methodology/geography/licences):
>
> - Source: Office for National Statistics licensed under the [Open
>   Government Licence
>   v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)
> - Contains OS data © Crown copyright and database right 2021.
