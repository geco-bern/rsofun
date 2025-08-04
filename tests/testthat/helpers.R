# define a better formatted version of testthat::expect_snapshot_value 
expect_snapshot_value_fmt <- function(
    x, 
    style = c("json", "json2", "deparse", "serialize"),
    cran = FALSE, tolerance = testthat_tolerance(), ..., variant = NULL) {
  # copy of expect_snapshot_value() but using constructive::construct()
  # for parsing of the values
  testthat:::edition_require(3, "expect_snapshot_value()")
  variant <- testthat:::check_variant(variant)
  lab <- rlang::quo_label(enquo(x))
  
  # Define formatting
  tmplate_at <- opts_atomic(compress = FALSE, one_liner = TRUE)
  tmplate_df <- opts_tbl_df(constructor = "tibble",   trailing_comma = TRUE, recycle = FALSE)
  tmplate_df2 <- opts_tbl_df(constructor = "tribble", trailing_comma = TRUE, recycle = FALSE)
  save <- function(x) {
    paste0(sprintf("# %s:\n", lab),
           paste0(as.character(constructive::construct(x, tmplate_df2, tmplate_at)$code),  # row-wise  (tribble)
                  collapse = "\n")
    )}
  load <- function(x) eval(parse(text = x))  # load <- function(x) testthat:::reparse(x)
  testthat:::with_is_snapshotting(force(x))
  testthat:::check_roundtrip(x, load(save(x)), label = lab, style = "constructive::construct", 
                             ..., tolerance = tolerance)
  testthat:::expect_snapshot_helper(lab, x, save = save, load = load, 
                                    cran = cran, ..., tolerance = tolerance, variant = variant, 
                                    trace_env = rlang::caller_env())
}
