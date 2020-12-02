test_that(
  "Place names with spaces in get processed correctly", {
    expect_identical(
      build_api_query(
        table_code_ref = 2,
        type = "census",
        server = "feature",
        within_level = "cauth19nm",
        within = "Greater Manchester",
        fields = c("lad19cd", "lad19nm", "cauth19cd", "cauth19nm")
      ),

      # copied unchanged from query generated on OG site:
      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD19_CAUTH19_EN_LU/FeatureServer/0/query?where=CAUTH19NM%20%3D%20%27GREATER%20MANCHESTER%27&outFields=LAD19CD,LAD19NM,CAUTH19CD,CAUTH19NM&returnDistinctValues=true&outSR=4326&f=json"
    )
  }
)



test_that(
  "map queries work ok", {

    expect_identical(
      build_api_query(
        table_code_ref = 7,
        type = "census",
        server = "feature",
        within_level = "msoa11cd",
        within = c("E02006666", "E02006667", "E02006668"),
        fields = c("msoa11cd", "msoa11nm")
      ),

      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Middle_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC/FeatureServer/0/query?where=MSOA11CD%20%3D%20%27E02006666%27%20OR%20MSOA11CD%20%3D%20%27E02006667%27%20OR%20MSOA11CD%20%3D%20%27E02006668%27&outFields=MSOA11CD,MSOA11NM&returnDistinctValues=true&outSR=4326&f=json"
    )
  }
)



test_that(
  "map queries work ok 2", {

    expect_identical(
      build_api_query(
        table_code_ref = 9,
        type = "admin",
        server = "map",
        within_level = "lad19nm",
        within = c("Cheltenham", "Gloucester", "Stroud", "Cotswold", "Tewkesbury", "Forest of Dean"),
        fields = c("lad19cd", "lad19nm")
      ),

      "https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Local_Authority_Districts_December_2019_Boundaries_UK_BGC/MapServer/0/query?where=lad19nm%20%3D%20%27CHELTENHAM%27%20OR%20lad19nm%20%3D%20%27GLOUCESTER%27%20OR%20lad19nm%20%3D%20%27STROUD%27%20OR%20lad19nm%20%3D%20%27COTSWOLD%27%20OR%20lad19nm%20%3D%20%27TEWKESBURY%27%20OR%20lad19nm%20%3D%20%27FOREST%20OF%20DEAN%27&outFields=lad19cd,lad19nm&returnDistinctValues=true&outSR=4326&f=json"
    )
  }
)
