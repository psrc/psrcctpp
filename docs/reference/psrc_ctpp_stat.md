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

  result of get_psrc_ctpp function or a derived table that still
  preserves the package's standard columns, including `table_id`,
  `category`, `estimate`, and `estimate_moe`

- group_vars:

  grouping variables, original or added

- stat_type:

  summary statistic to compute. Supported values are `"sum"` and
  `"median"`.

- incl_na:

  whether to include NA lines
