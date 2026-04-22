# Fetch API results (with automatic pagination) Helper function sending the API call, iterating through all result pages

Fetch API results (with automatic pagination) Helper function sending
the API call, iterating through all result pages

## Usage

``` r
api_gofer(url, page_size = 1000L)
```

## Arguments

- url:

  api call (query params other than page/size may already be present)

- page_size:

  rows per request; larger values reduce round-trips (default 1000)

## Value

data.table of all result rows across all pages
