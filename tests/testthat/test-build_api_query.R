test_that(
  "Place names with spaces in get processed correctly", {
    expect_identical(
      build_api_query(
        ref = 4,
        type = "census",
        within_level = "cauth20nm",
        within = "Greater Manchester",
        fields = c("lad20cd", "lad20nm", "cauth20cd", "cauth20nm")
      ),

      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD20_CAUTH20_EN_LU/FeatureServer/0/query?where=%20(CAUTH20NM%20%3D%20'GREATER%20MANCHESTER')%20&outFields=LAD20CD,LAD20NM,CAUTH20CD,CAUTH20NM&outSR=4326&f=json&returnDistinctValues=true"
    )
  }
)



test_that(
  "map queries work ok", {

    expect_identical(
      build_api_query(
        ref = 10,
        type = "census",
        within_level = "msoa11cd",
        within = c("E02006666", "E02006667", "E02006668"),
        fields = c("msoa11cd", "msoa11nm")
      ),

      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Middle_Layer_Super_Output_Areas_December_2011_EW_BFC_V2/FeatureServer/0/query?where=%20(MSOA11CD%20%3D%20'E02006666'%20OR%20MSOA11CD%20%3D%20'E02006667'%20OR%20MSOA11CD%20%3D%20'E02006668')%20&outFields=MSOA11CD,MSOA11NM&outSR=4326&f=json&returnDistinctValues=true"
    )
  }
)



test_that(
  "map queries work ok 2", {

    expect_identical(
      build_api_query(
        ref = 12,
        type = "census",
        within_level = "lad20nm",
        within = c("Cheltenham", "Gloucester", "Stroud", "Cotswold", "Tewkesbury", "Forest of Dean"),
        fields = c("lad20cd", "lad20nm")
      ),

      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2020_UK_BFC/FeatureServer/0/query?where=%20(LAD20NM%20%3D%20'CHELTENHAM'%20OR%20LAD20NM%20%3D%20'GLOUCESTER'%20OR%20LAD20NM%20%3D%20'STROUD'%20OR%20LAD20NM%20%3D%20'COTSWOLD'%20OR%20LAD20NM%20%3D%20'TEWKESBURY'%20OR%20LAD20NM%20%3D%20'FOREST%20OF%20DEAN')%20&outFields=LAD20CD,LAD20NM&outSR=4326&f=json&returnDistinctValues=true"
    )
  }
)
