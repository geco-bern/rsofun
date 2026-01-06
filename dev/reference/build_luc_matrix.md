# Build LUC matrix

Build land-use change (LUC) transition matrix from patterns.

## Usage

``` r
build_luc_matrix(patterns, n_lu, n_years, out = vector())
```

## Arguments

- patterns:

  A list of patterns. Each pattern must be a sequence of transition
  values whose size is a multiple of `n_lu`x`n_lu`. The `n_lu` first
  values are the transitions from each LU to the first LU, and so on. If
  the sequence contains more years than `n_years`, it will be truncated.

- n_lu:

  Number of land use types (LU).

- n_years:

  Number of years (i.e. length of the 3rd dimension).

- out:

  For internal use only.

## Value

An `n_lu`x`n_lu`x`n_years` transition matrix.

## Examples

``` r
# Example of building a 6 year-long transition matix consisting of 6 times 2x2 matrices

# A one time transfer of 0.5 of the total cell fraction from LU 2 to LU 1
pattern1 <- c(0, 0, 0.5, 0)
# The null pattern (no transition)
null_pattern <- rep(0, 4)
# A repeated self-transition of 0.1 of the total cell fraction from LU 2 to LU 2 every other year
pattern2 <- rep(c(c(0, 0, 0, 0.1), null_pattern), 3)

# Building the transition matrix
build_luc_matrix(list(pattern1, pattern2), 2, 6)
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]    0  0.5
#> [2,]    0  0.1
#> 
#> , , 2
#> 
#>      [,1] [,2]
#> [1,]    0    0
#> [2,]    0    0
#> 
#> , , 3
#> 
#>      [,1] [,2]
#> [1,]    0  0.0
#> [2,]    0  0.1
#> 
#> , , 4
#> 
#>      [,1] [,2]
#> [1,]    0    0
#> [2,]    0    0
#> 
#> , , 5
#> 
#>      [,1] [,2]
#> [1,]    0  0.0
#> [2,]    0  0.1
#> 
#> , , 6
#> 
#>      [,1] [,2]
#> [1,]    0    0
#> [2,]    0    0
#> 
```
