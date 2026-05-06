# Retrieve CTPP data

Retrieve CTPP data

## Usage

``` r
get_psrc_ctpp(scale, table_code, dyear = 2016, geoids = NULL, filepath = NULL)
```

## Arguments

- scale:

  "county", "place", or "tract" for residence/workplace tables;
  "county-county", "place-place", "place-county", "county-place", or
  "tract-tract" for O-D tables

- table_code:

  requested data table as string, e.g. "A302103"

- dyear:

  last of 5-year CTPP span, e.g. 2016 for ctpp1216 survey

- geoids:

  optional string vector of GEOID codes to limit the table

- filepath:

  optional network location of downloaded ftp CTPP files

## Value

A data table with stable column names used by downstream helpers.
Standard columns always include `table_id`, `category`, `estimate`, and
`estimate_moe`. Geography columns vary by table type and may include
`res_geoid`, `res_label`, `work_geoid`, and `work_label`. You may add or
transform columns before passing the result to
[`psrc_ctpp_sum()`](https://psrc.github.io/psrcctpp/reference/ctpp_stat.md)
or
[`ctpp_shares()`](https://psrc.github.io/psrcctpp/reference/ctpp_shares.md),
but if you recode `category` you should preserve a single total row per
grouping with a value beginning with `"Total"`.
