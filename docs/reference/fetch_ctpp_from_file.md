# Fetch ftp-format CTPP data from network location Helper to get_psrc_ctpp

Fetch ftp-format CTPP data from network location Helper to get_psrc_ctpp

## Usage

``` r
fetch_ctpp_from_file(scale, table_code, dyear = 2016, filepath = "default")
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

- filepath:

  optional network location of downloaded ftp CTPP files

## Value

data table
