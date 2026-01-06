# Initialises a tibble with dates

Creates a tibble with rows for each date from `'yrstart'` to `'yrend'`
in `'yyyy-mm-dd'` format. Intervals of dates are specified by argument
`'freq'`. ddf \<- init_dates_dataframe(2000, 2003, startmoy=1,
startdoy=1, freq="days", endmoy=12, enddom=31, noleap=FALSE)

## Usage

``` r
init_dates_dataframe(
  yrstart,
  yrend,
  startmoy = 1,
  startdoy = 1,
  freq = "days",
  endmoy = 12,
  enddom = 31,
  noleap = FALSE
)
```

## Arguments

- yrstart:

  An integer defining the start year of dates covered by the dataframe.

- yrend:

  An integer defining the end year of dates covered by the dataframe.

- startmoy:

  An integer defining the start month-of-year of dates covered by the
  dataframe. Defaults to 1.

- startdoy:

  An integer defining the start day-of-year of dates covered by the
  dataframe. Defaults to 1.

- freq:

  A character string specifying the time steps of dates (in rows).
  Defaults to `"days"`. Any of `"days", "months", "years"`. If
  `freq = "months"` the 15\\^{th}\\ day of the months is used as date,
  and if `freq = "years"` the 1\\^{st}\\ of January of each year is
  returned.

- endmoy:

  An integer defining the end month-of-year of dates covered by the
  dataframe. Defaults to 12.

- enddom:

  An integer defining the end day-of-year of dates covered by the
  dataframe. Defaults to 31.

- noleap:

  Whether leap years are ignored, that is, whether the 29\\^{th}\\ of
  February is removed. Defaults to `FALSE`.

## Value

A tibble with dates.
