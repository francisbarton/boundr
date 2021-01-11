# jogger 0.2.2

* Enhance examples in geo_get to better match README
* Create geo_get_bounds as a separate function and move out of geo_get to sep file
* Remove distinct option from params
* Add a few more options for lookups (realised LTLA to UTLA could easily be added)

# jogger 0.2.1

* Changed codes and boundary URLs to 2020 where possible
* Many tweaks required to build_api_query.R test!
* Fix to expand_lookup.R param docs!
* Add options for returning centroid and shape data fields with boundaries

# jogger 0.2.0

* Added a centroids query option (MSOA only for now) as well as boundaries
* Added in option to specify spatial reference code eg ESPG 3857
* Lots of tidying up the code - eg removed return_style = "all" option as
    unnecessary; create_custom_lookup did not need a spatial_ref option etc
* Improved function parameter documentation
* Included new static HoCL MSOA Names URL and removed old version parameter
* Started writing README.Rmd and included examples
* Sorted out which functions to @export. Examples in create_custom_lookup.R are now
    \dontrun as the function is not exported (examples irrelevant now really)
* Sorted out a batch size issue that was preventing longer queries from succeeding
* Added GitHub Actions for CMD Check and Github Pages/pkgdown
* TODO: Write tests for `geo_get`

# jogger 0.1.3

* Further improvements to documentation
* Wrote a few tests for build_api_query()
* Package builds OK
* Not all examples are working, not sure what the cause is
* Refactored and tidied up misc small parts of scripts


# jogger 0.1.2

* Amended `geo_get()` to be the main function - copied params documentation over from create_custom_lookup()
* Fixed various build errors mostly to do with namespace
* Realised that utils::URLencode() isn't vectorised so rewrote build_api_query() process to account for that
* Removed examples where no longer accurate (params have shifted)
* TODOs from 0.1.1 remain


# jogger 0.1.1

* Sorted out Imports in DESCRIPTION file
* check() package and build documentation
* TODO: document internal package data
* TODO: work on README
* TODO: examples


# jogger 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Added parameter documentation to build_api_query.R
* Deleted superseded files (initial attempts)
