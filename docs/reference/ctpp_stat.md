# CTPP summary statistics

CTPP summary statistics

Generate CTPP sums

Generate CTPP medians

## Usage

``` r
psrc_ctpp_sum(df, group_vars = "category", incl_na = TRUE)

psrc_ctpp_median(df, group_vars = "category", incl_na = TRUE)
```

## Arguments

- df:

  result of get_psrc_ctpp function or a derived table that still
  preserves the package's standard columns, including `table_id`,
  `category`, `estimate`, and `estimate_moe`

- group_vars:

  grouping variables, original or added

- incl_na:

  whether to include NA lines

## Value

A table with the variable labels, summary statistic, and margin of
error. Sum summaries return a propagated margin of error. Median
summaries preserve the standard `estimate_moe` column and fill it with
`NA_real_`, because a comparable median MOE is not currently computed.
If you recode `category`, preserve exactly one total row per group so
downstream helpers such as
[`ctpp_shares()`](https://psrc.github.io/psrcctpp/reference/ctpp_shares.md)
can still identify the denominator. Sum summaries with
`group_vars = NULL` warn when the input still contains a `"Total"` row
alongside component rows, because that full collapse will add the total
to its parts.
