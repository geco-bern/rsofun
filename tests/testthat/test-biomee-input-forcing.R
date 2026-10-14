valid_biomee_forcing <- function() {
  data.frame(date = as.Date("2020-01-01") + 0:2, ppfd = c(0, 0.0002, 0.0005),
             temp = c(-20, 0, 25), vpd = c(0, 200, 1000), rain = c(0, 0.0001, 0),
             wind = c(0, 1, 2), patm = c(83000, 97000, 101000), co2 = 410)
}

test_that("BiomeE preserves valid forcing, metadata and subdaily dates", {
  forcing <- valid_biomee_forcing()
  forcing$hod <- 11.5
  forcing$unused_metadata <- NA_real_
  expect_identical(build_forcing(forcing), forcing)
  forcing$date[2] <- forcing$date[1]
  expect_identical(build_forcing(forcing), forcing)
  forcing$date <- as.POSIXct("2020-01-01", tz = "UTC") + 0:2 * 1800
  expect_identical(build_forcing(forcing), forcing)
})

test_that("BiomeE rejects nonfinite values in every core forcing variable", {
  for (variable in c("ppfd", "temp", "vpd", "rain", "wind", "patm", "co2")) {
    for (value in c(NA_real_, NaN, Inf, -Inf)) {
      forcing <- valid_biomee_forcing()
      forcing[[variable]][2] <- value
      expect_error(build_forcing(forcing), paste0("'", variable, "'.*row 2 \\(2020-01-02\\)"))
    }
  }
})

test_that("BiomeE rejects sentinels, negative fluxes and invalid units", {
  invalid <- list(ppfd = -1, temp = c(-9999, 273.15), vpd = -1,
                  rain = -1, wind = -1, patm = c(-6423391, 0, 97, 150000),
                  co2 = c(-9999, 0))
  for (variable in names(invalid)) {
    for (value in invalid[[variable]]) {
      forcing <- valid_biomee_forcing()
      forcing[[variable]][1] <- value
      expect_error(build_forcing(forcing), paste0("'", variable, "'.*row 1"))
    }
  }
  forcing <- valid_biomee_forcing()
  forcing$patm[1] <- -1
  forcing$rain[2] <- -1
  expect_error(build_forcing(forcing), "'patm'")
  expect_error(build_forcing(forcing), "'rain'")
})

test_that("BiomeE rejects malformed forcing before passing it to Fortran", {
  forcing <- valid_biomee_forcing()
  forcing$patm <- as.character(forcing$patm)
  expect_error(build_forcing(forcing), "'patm' must be a numeric vector")
  forcing$patm <- factor(forcing$patm)
  expect_error(build_forcing(forcing), "'patm' must be a numeric vector")
  forcing <- valid_biomee_forcing()
  forcing$date[2] <- NA
  expect_error(build_forcing(forcing), "without missing dates")
  forcing <- valid_biomee_forcing()
  expect_error(build_forcing(forcing[3:1, ]), "chronological order")
  forcing$date <- as.character(forcing$date)
  expect_error(build_forcing(forcing), "Date or POSIXt")
})
