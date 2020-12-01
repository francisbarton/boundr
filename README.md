
# jogger

<!-- badges: start -->
<!-- badges: end -->

### Retrieve area boundaries and data from the ONS Open Geography Portal

The main function of this package is to download area boundaries
  (in GeoJSON format) using the ONS Open Geography API, for all sub-areas - at a
  specified level - within a specified area.
The main script will return a data frame with the sub-area geometry column,
  as an `sf` object ready to be visualised as a map.
Initially just for areas within England and Wales, with a hope to add
  Scottish geographies at a later date.
And currently just for the following area levels:
  LSOA, MSOA, Ward, LAD/LTLA, UTLA, CAUTH, RGN _[subject to change]_
  
_Changes since 2019 (currently -- Nov 2020 -- just the change in Buckinghamshire from two-tier to unitary) are not yet incorporated._

## Installation

``` r
remotes::install_github("francisbarton/jogger")
```

## Example

A basic example will be included here.


## Contributing

Suggestions are welcome, preferably posted as an issue on GitHub.
Contributions as pull requests are also welcome.

You are also welcome to email me with comments or ideas.

This project has a Contributor Code of Conduct.


## Licences

The code in this repo is MIT licensed.

The data that the code helps you retrieve is issued under a variety of licences,
including:

* the [Open Government Licence v3.0][ogl3]


Licensing statement [as stipulated by the ONS][ons-licence]:

* Source: Office for National Statistics licensed under the [Open Government Licence v3.0][ogl3]
* Contains OS data © Crown copyright and database right 2020.


[ogl3]: https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
[ons-licence]: https://www.ons.gov.uk/methodology/geography/licences

