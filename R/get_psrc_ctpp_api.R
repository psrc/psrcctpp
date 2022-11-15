library(httr)
library(jsonlite)
library(data.table)
library(dplyr)
library(stringr)
library(purrr)

api_gofer <- function(url){
  h <- c("x-api-key"=Sys.getenv("CTPP_API_KEY"), "accept"="application/json")
  resp <- httr::GET(url, add_headers(.headers=h))
  if (http_type(resp)!="application/json") {
    stop("API did not return json", call.=FALSE)
  }
  result <- jsonlite::fromJSON(content(resp, "text", encoding="UTF-8"),
                               simplifyDataFrame=TRUE) %>% purrr::pluck("data")
  return(result)
}

ctpp_tblsearch <- function(regex, year=2016) {
  url <- paste0("https://ctpp.macrosysrt.com/api/groups?year=", year)
  result <- api_gofer(url) %>% setDT() %>%
    .[grepl(regex, description, ignore.case=TRUE)|
      grepl(regex, universe, ignore.case=TRUE)]
  return(result)
}

#' Retrieve CTPP data
#'
#' @param quarry either a table code as string--e.g. "A302103"--or string vector of variable codes
#' @param dyear last of 5-year CTPP span, e.g. 2016 for ctpp1216 survey
#' @param scale "county", "place", or "tract" for residence/workplace tables; "county-county", "place-place", "place-county", "county-place", or "tract-tract" for O-D tables
#' @param geoids optional string vector of GEOID codes to limit the table
#' @return data table
#'
#' @importFrom stringr str_sub str_extract str_replace
#' @import data.table
#' @export
get_psrc_ctpp <- function(quarry, year=2016, scale, geo=NULL){
  get_arg <- paste0("?get=",
                    if(length(quarry)>1){
                      paste0(quarry, collapse=",")
                    }else{
                      paste0("group%28", tolower(quarry))})
  scale_arg <- if(!grepl("-", scale)){paste0("&for=", scale)
                      }else{paste0("&for=", strsplit(scale, ",")[[1]],
                                 "&d-for=", strsplit(scale, ",")[[2]])}
  if(is.null(geo)){
    geo_arg <-paste0("&in=county%3A", paste0("530",c("33","35","53","61"), collapse="%2C"))
  }else{
    geo_len <- lapply(geo, length) %>% unique()
    geo_scale <- case_when(geo_len==5  ~"county",
                           geo_len==7  ~"place",
                           geo_len==11 ~"tract")
    geo_arg <- if(length(geo_len!=1)){NULL}else{
                paste0("&in=", geo_scale, "%3A", paste0(geo, collapse="%2C"))
    }
  }
  url <- paste0("https://ctpp.macrosysrt.com/api/data/", year, get_arg, scale_arg, geo_arg)
  result <- api_gofer(url)
  return(result)
}
