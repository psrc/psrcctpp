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

data table
