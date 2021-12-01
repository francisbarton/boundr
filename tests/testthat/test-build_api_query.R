test_that(
  "Place names with spaces in get processed correctly", {
    expect_identical(
      build_api_query(
        ref = 4,
        where_level = "cauth20nm",
        where = "Greater Manchester",
        fields = c("lad20cd", "lad20nm", "cauth20cd", "cauth20nm")
      ),

      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD20_CAUTH20_EN_LU/FeatureServer/0/query?where=%20(CAUTH20NM%20=%20'GREATER%20MANCHESTER')%20&outFields=LAD20CD,LAD20NM,CAUTH20CD,CAUTH20NM&resultType=standard&returnDistinctValues=true&f=json"
    )
  }
)



test_that(
  "map queries work ok", {

    expect_identical(
      build_api_query(
        ref = 12,
        where_level = "msoa11cd",
        where = c("E02006666", "E02006667", "E02006668"),
        fields = c("msoa11cd", "msoa11nm")
      ),

      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Middle_Layer_Super_Output_Areas_DEC_2011_EW_BGC_V3/FeatureServer/0/query?where=%20(MSOA11CD%20=%20'E02006666'%20OR%20MSOA11CD%20=%20'E02006667'%20OR%20MSOA11CD%20=%20'E02006668')%20&outFields=MSOA11CD,MSOA11NM&outSR=4326&resultType=none&returnDistinctValues=true&f=json"
    )
  }
)



test_that(
  "map queries work ok 2", {

    expect_identical(
      build_api_query(
        ref = 14,
        where_level = "lad20nm",
        where = c("Cheltenham", "Gloucester", "Stroud", "Cotswold", "Tewkesbury", "Forest of Dean"),
        fields = c("lad20cd", "lad20nm", "BNG_E", "BNG_N"),
        sr = 7405
      ),

      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD_MAY_2021_UK_BGC/FeatureServer/0/query?where=%20(LAD20NM%20=%20'CHELTENHAM'%20OR%20LAD20NM%20=%20'GLOUCESTER'%20OR%20LAD20NM%20=%20'STROUD'%20OR%20LAD20NM%20=%20'COTSWOLD'%20OR%20LAD20NM%20=%20'TEWKESBURY'%20OR%20LAD20NM%20=%20'FOREST%20OF%20DEAN')%20&outFields=LAD20CD,LAD20NM,BNG_E,BNG_N&outSR=7405&resultType=none&returnDistinctValues=true&f=json"
    )
  }
)
