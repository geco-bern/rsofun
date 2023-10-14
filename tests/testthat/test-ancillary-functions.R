context("test ancillary functions")

test_that("test init data frame", {
  
  expect_type(
    init_dates_dataframe(
      yrstart = 2001,
      yrend = 2002,
      freq = "months"
    ),
    "list"
  )
  
  expect_type(
    init_dates_dataframe(
      yrstart = 2001,
      yrend = 2002,
      freq = "years"
    ),
    "list"
  )
})
