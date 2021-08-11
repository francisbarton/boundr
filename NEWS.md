# 0.2.15

* Fixes internal data issue where datasets weren't being found
* Adds more internal datasets to internal package data
* Upgrades LAD boundaries query to 2021 boundaries
* Simplifies LSOA:MSOA lookup by using internal data for HoCL names CSV


# 0.2.13

* Fixes a bug where `within_cd` and `return_style = minimal` – a non-functional combination – was allowed through.
* Fixes an earlier mistake where I thought that all API URLs were "FeatureServer" now - but centroid queries still need "MapServer".


# 0.2.12

* Enables population-weighted centroid queries for OA and LSOA as well as MSOA now
* Fixes OA:LAD queries returning wards as intermediate levels instead of MSOAs


# 0.2.11

* Removes the `returnGeometry` option previously added, as this was seemingly causing a problem with centroid retrieval? I don't think it was really needed anyway

# 0.2.10

* adds the ability to retrieve lookups and geometries at a single level, i.e. where `bounds_level` and `within_level` are the same.


# 0.2.9.5

* tweaks to make the `within_cd` parameter actually work (I don't think I had tested this properly before).
* some tweaks to how the query URL string is built, replacing `%3D` with plain `=`. In attempt to simplify error-hunting. But worried that this may not work with all endpoints. Had to do some tweaking to get OA:WD lookup to work.
* changes to the `extract_lookup` function as the previous `pluck` call did not always work. New combination of `pluck` and `map_df` hopefully will work OK. But again, worried that this may break things that were previously working.
* This is why we should have proper tests in place, and I should be using git branches to test things I'm not sure about...


# 0.2.9.4
* Added in result_type parameter to `build_api_query` as I noticed that some queries weren't working properly. Turned out that lookups with more than 2000 results were being truncated. [The way to fix this](https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-layer-.htm) (see examples six and seven) is to specify a `resultType` of `"standard"` which allows responses up to 32,000 long. jogger's spatial queries were already batched into batches of 25/50 so this didn't come up as an issue.


# 0.2.9.3
* Further build_api tweaks

# 0.2.9.2

* update and fix examples
* update and re-knit README.Rmd

# jogger 0.2.9.1

* Bit of a refactor, remove duplications
* Move helper function get_serious to its own file
* Reorganised geo_get and geo_get_bounds, slimmed down & restructured

# jogger 0.2.9

* Finally removed the code that was there for 2-stage lookups - not going to happen
* Added in a self-built lookup for UTLA:RGN and UTLA:CTRY as ONS doesn't have these
* Finally did documentation of the internal datasets

# jogger 0.2.8

* Trying to fix lookup for MSOA centroids
* Reported [issue](https://github.com/francisbarton/jogger/issues/2) relating to different return_styles

# jogger 0.2.7

* Further query string updates, to incorp changes at OpenGeography
* (All query URLs are Feature Server now; no more Admin, as some were)
* Add returnDistinctValues=TRUE to standard query string (reduces d/loads?)
* Further tidying up of data in build_api_query.R

# jogger 0.2.6

* just some minor fixes (out of date rgn19 and ctry19 in geo_get.R)

# jogger 0.2.5

* Updated several URL details to latest on ONS OpenGeography

# jogger 0.2.4

* Added OA:Ward lookup capability
* Fixed OA:Ward lookup capability that I thought I'd added
* Enabled passing in a vector of codes -- or names -- as the `within` parameter to `geo_get()`. If you're passing in *codes*, set `within_cd = TRUE`
* Enabled HoCL MSOA Names inclusion when doing an OA-level query
* First steps towards enabling 2-step lookups
* OA:LAD lookup now defaults to returning OA-Ward-LAD rather than OA-LSOA-MSOA-LAD

# jogger 0.2.3

* Added (top secret!) capacity for Output Area queries
* By accident this also enabled (I think) LSOA/MSOA:RGN queries
* I discovered that the ONS query doesn't mind if you ask for duplicate field names in your query (as specifically requested return_fields) - it just gives you one of them. Very nicely behaved. (Why does this matter? Because there's no oa11nm and it would have been a real hassle to rejig my code to deal with e.g. three return fields instead of four - but instead I can just request oa11cd twice and it's fine.)
* In connection with the previous point, I also discovered that dplyr::select() is similarly well-behaved and doesn't mind if you pass it duplicate column names such as, I don't know, two lots of oa11cd for example. Presumably it just goes with the first one and ignores subsequent dupes
* Updated a few other query lookup URLs
* I still haven't written proper formal tests for many of the features
* Sprinkled a few TODO notes in the code for future work
* I also did a little bit more commenting. Need to do loads more.

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
