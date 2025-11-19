blockstrap
================

<!-- README.Rmd is generated from this file. Run devtools::build_readme() to knit. -->

# blockstrap <img src="https://img.shields.io/badge/status-experimental-orange" alt="Status: experimental"/>

Sample complete groups (“blocks”) from a grouped data frame. This
package implements a simple *block bootstrap* style sampler: instead of
sampling individual rows, you sample entire groups preserving the
intra-group structure.

> Assumption: The package name in `DESCRIPTION` is `blockstrap`
> (repository folder is `flockstrapr`). This README uses the
> `blockstrap` name; adjust if the package is renamed.

## Installation

``` r
# install.packages("devtools")
remotes::install_github("MikeLydeamore/flockstrapr")
```

## Motivation

When observations belonging to the same experimental unit are spread
across multiple rows (e.g. multiple measurements per subject, dose
combinations, time series segments), ordinary row-wise sampling breaks
these units apart. A block sampler keeps units intact by sampling at the
group level.

## Core function

`slice_block()` works on a *grouped* data frame. If you call it on an
ungrouped data frame, it throws a helpful error.

Key arguments:

- `n`: number of groups (blocks) to sample.
- `replace`: sample with replacement? Needed when `n` exceeds number of
  groups.
- `weight_by`: optional expression (unquoted) evaluated per-group to
  weight sampling probabilities.
- `...`: passed to base `sample()` allowing e.g. `set.seed()` beforehand
  or other sampling tweaks.

## Basic example

We use the built-in `ToothGrowth` dataset and treat each supplement-dose
combination as a block.

``` r
library(dplyr)
library(blockstrap)

set.seed(1)
ToothGrowth |>
  group_by(supp, dose) |>
  slice_block(n = 2)
```

    ## # A tibble: 20 × 3
    ## # Groups:   supp, dose [2]
    ##      len supp   dose
    ##    <dbl> <fct> <dbl>
    ##  1  15.2 OJ      0.5
    ##  2  21.5 OJ      0.5
    ##  3  17.6 OJ      0.5
    ##  4   9.7 OJ      0.5
    ##  5  14.5 OJ      0.5
    ##  6  10   OJ      0.5
    ##  7   8.2 OJ      0.5
    ##  8   9.4 OJ      0.5
    ##  9  16.5 OJ      0.5
    ## 10   9.7 OJ      0.5
    ## 11   4.2 VC      0.5
    ## 12  11.5 VC      0.5
    ## 13   7.3 VC      0.5
    ## 14   5.8 VC      0.5
    ## 15   6.4 VC      0.5
    ## 16  10   VC      0.5
    ## 17  11.2 VC      0.5
    ## 18  11.2 VC      0.5
    ## 19   5.2 VC      0.5
    ## 20   7   VC      0.5

## Sampling with replacement

If you want to sample more groups than exist, or allow repeats:

``` r
ToothGrowth |>
  group_by(supp, dose) |>
  slice_block(n = 10, replace = TRUE) |>
  count(supp, dose)
```

    ## # A tibble: 5 × 3
    ## # Groups:   supp, dose [5]
    ##   supp   dose     n
    ##   <fct> <dbl> <int>
    ## 1 OJ      0.5    20
    ## 2 OJ      1      20
    ## 3 OJ      2      30
    ## 4 VC      1      20
    ## 5 VC      2      10

Repeated blocks will appear multiple times (row counts summed
accordingly).

## Weighted sampling

Weight blocks by a statistic, e.g. mean tooth length, to favor larger
mean response groups:

``` r
set.seed(42)
weighted <- ToothGrowth |>
  group_by(supp, dose) |>
  slice_block(n = 3, weight_by = mean(len))
```

You can verify weighting bias by repeating and tallying frequencies:

``` r
set.seed(99)
rep_draws <- replicate(500, {
  ToothGrowth |> group_by(supp, dose) |> slice_block(n = 3, weight_by = mean(len)) |> distinct(supp, dose)
}, simplify = FALSE)

freqs <- bind_rows(rep_draws) |> count(supp, dose, name = "times") |> arrange(desc(times))
freqs
```

    ## # A tibble: 6 × 3
    ## # Groups:   supp, dose [6]
    ##   supp   dose times
    ##   <fct> <dbl> <int>
    ## 1 OJ      2     334
    ## 2 VC      2     326
    ## 3 OJ      1     292
    ## 4 VC      1     224
    ## 5 OJ      0.5   205
    ## 6 VC      0.5   119
