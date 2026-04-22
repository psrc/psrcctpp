# Fetch CTPP data from AASHTO api Helper to get_psrc_ctpp

Fetch CTPP data from AASHTO api Helper to get_psrc_ctpp

## Usage

``` r
fetch_ctpp_from_api(scale, table_code, dyear, geoids)
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

## Value

data table
