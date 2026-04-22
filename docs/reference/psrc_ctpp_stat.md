# Generic call for CTPP summary statistics

Given specific form by related
[`ctpp_stat`](https://psrc.github.io/psrcctpp/reference/ctpp_stat.md)
functions.

## Usage

``` r
psrc_ctpp_stat(df, group_vars, stat_type = "sum", incl_na = FALSE)
```

## Arguments

- df:

  result of get_psrc_ctpp function

- group_vars:

  grouping variables, original or added

- stat_type:

  for now, "sum" is only option

- incl_na:

  whether to include NA lines
