test_that(
  "Place names with spaces in get processed correctly", {
    expect_identical(
      build_api_query(
        table_code_ref = 2,
        type = "census",
        server = "feature",
        within_level = "cauth20nm",
        within = "Greater Manchester",
        fields = c("lad20cd", "lad20nm", "cauth20cd", "cauth20nm")
      ),

      # copied unchanged from query generated on OG site:
      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD20_CAUTH20_EN_LU/FeatureServer/0/query?where=%20(CAUTH20NM%20%3D%20'GREATER%20MANCHESTER')%20&outFields=LAD20CD,LAD20NM,CAUTH20CD,CAUTH20NM&outSR=4326&f=json"
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

      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Middle_Layer_Super_Output_Areas_December_2011_EW_BFC_V2/FeatureServer/0/query?where=%20(MSOA11CD%20%3D%20'E02006666'%20OR%20MSOA11CD%20%3D%20'E02006667'%20OR%20MSOA11CD%20%3D%20'E02006668')%20&outFields=MSOA11CD,MSOA11NM&outSR=4326&f=json"
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
        within_level = "lad20nm",
        within = c("Cheltenham", "Gloucester", "Stroud", "Cotswold", "Tewkesbury", "Forest of Dean"),
        fields = c("lad20cd", "lad20nm")
      ),

      "https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Local_Authority_Districts_May_2020_Boundaries_UK_BFC_V3/MapServer/0/query?where=%20(lad20nm%20%3D%20'CHELTENHAM'%20OR%20lad20nm%20%3D%20'GLOUCESTER'%20OR%20lad20nm%20%3D%20'STROUD'%20OR%20lad20nm%20%3D%20'COTSWOLD'%20OR%20lad20nm%20%3D%20'TEWKESBURY'%20OR%20lad20nm%20%3D%20'FOREST%20OF%20DEAN')%20&outFields=lad20cd,lad20nm&outSR=4326&f=json"
    )
  }
)
