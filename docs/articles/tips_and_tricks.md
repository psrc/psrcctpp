# psrcctpp tips & tricks

### Querying by variable

In addition to requesting entire tables, the package/API can return
individual variables. If you prefer this, you’ll need to identify the
variable codes, so naturally there’s a **`ctpp_varsearch`** function to
assist; the only parameter is the table code in question. Once these
codes are identified, their character vector can be used as the second
argument (`table_code`)
[`get_psrc_ctpp()`](https://psrc.github.io/psrcctpp/reference/get_psrc_ctpp.md).

``` r

shhh <- suppressPackageStartupMessages
shhh(library(psrcctpp))
shhh(library(magrittr))
shhh(library(dplyr))
shhh(library(stringr))

ctpp_varsearch("A101101", year=2016) %>% head()
```

    ##          name           label
    ##        <char>          <char>
    ## 1: A101101_e2  Under 16 years
    ## 2: A101101_e1 Total, all ages
    ## 3: A101101_e3 16 and 17 years
    ## 4: A101101_e4  18 to 20 years
    ## 5: A101101_e5  21 to 24 years
    ## 6: A101101_e6  25 to 34 years

``` r

x <- get_psrc_ctpp(scale="tract", 
                   table_code=c("A101101_e1","A101101_e2"), # Variable list instead of table
                   dyear=2016)

mutate(x, res_label=str_sub(res_label, 7L, 14L),           # abbreviate to fit in frame
          category =str_sub(category, 1L, 15L)) %>%
  select(c(3,6:8)) %>% head()                   
```

    ##    res_label work_label        category estimate
    ##       <char>     <char>          <char>    <num>
    ## 1:   Tract 1       <NA> Total, all ages     7260
    ## 2:   Tract 1       <NA>  Under 16 years     1120
    ## 3:   Tract 2       <NA> Total, all ages     7900
    ## 4:   Tract 2       <NA>  Under 16 years     1150
    ## 5:   Tract 3       <NA> Total, all ages     2830
    ## 6:   Tract 3       <NA>  Under 16 years      555

### Bypassing the API

In the unlikely event of API downtime, an older method exists in the
package to read CTPP ftp files pre-downloaded to a network location (it
takes too long to download the entire ftp zipfile for each call). To use
this fallback option, specify `get_psrc_ctpp( ..., filepath="default")`
while on PSRC VPN. It is also possible to go completely offline by
saving a full copy to a local directory, then specifying that directory
in place of `"default"`. There is no API-free fallback for the table and
variable lookup functions, however.

``` r

y <- get_psrc_ctpp("tract", "A101101", 2016, filepath="default")

mutate(y, res_label=str_sub(res_label, 7L, 14L),    # abbreviate to fit in frame
          category =str_sub(category, 1L, 15L)) %>%
  select(c(3,6:8)) %>% head()    
```

    ##    res_label estimate estimate_moe category
    ##       <char>    <num>        <num>   <char>
    ## 1:   Tract 1     7260          481    Total
    ## 2:   Tract 2     7900          437    Total
    ## 3:   Tract 3     2830          223    Total
    ## 4:   Tract 4     6260          500    Total
    ## 5:   Tract 4     5165          481    Total
    ## 6:   Tract 5     3155          182    Total
