# Add shares to CTPP table

Add shares to CTPP table

## Usage

``` r
ctpp_shares(df)
```

## Arguments

- df:

  dataframe, either result of get_psrc_ctpp or psrc_ctpp_stat, that
  retains the standard `category`, `estimate`, and `estimate_moe`
  columns. Category totals must remain identifiable with a value
  beginning with `"Total"`.

## Value

dataframe with share & share_moe. If `ctpp_shares()` cannot identify
exactly one total row per group, it warns and returns `df` unchanged.
