context("run")

test_that("simple setup works", {
  ## TODO: use fortran implementation
  gpp_temp_test_for <- purrr::map( as.list( seq( 0, 35, length.out = 100 ) ), ~pmodel( temp = ., vpd = 100, co2 = 300, ppfd = 10, fapar = 0.7, elv = 300, implementation = "r", sofundir = settings_sims_simple$dir_sofun) ) %>% unlist()
  expect_is(gpp_temp_test_for, "numeric")
  
  ## R
  gpp_temp_test_r <- purrr::map( as.list( seq( 0, 35, length.out = 100 ) ), ~rpmodel( fpar = 0.7, ppfd = 10, co2 = 300, tc = ., cpalpha = 1.0, vpd = 100, elv = 300 ) ) %>% purrr::map("gpp") %>% unlist()
  expect_is(gpp_temp_test_r, "numeric")
  
  expect_equal(gpp_temp_test_for, gpp_temp_test_r)
})
