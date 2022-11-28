#' @importFrom magrittr %<>% %>%
#' @author Michael Jensen
NULL

globalVariables(c(":=", "!!", ".", "enquos"))

str2num <- function(x){as.numeric(stringr::str_replace_all(x,"(\\+/-)|,|\\*+",""))}
`%not_in%` <- Negate(`%in%`)

#' Fetch API results
#'
#' Helper function sending the API call & parsing its response
#'
#' @param url api call
#' @return data table
#'
#' @importFrom httr GET http_type add_headers content
#' @importFrom jsonlite fromJSON
api_gofer <- function(url){
  h <- c("x-api-key"=Sys.getenv("CTPP_API_KEY"), "accept"="application/json")
  resp <- GET(url, add_headers(.headers=h))
  if (http_type(resp)!="application/json") {
    stop("API did not return json", call.=FALSE)
  }
  result <- fromJSON(content(resp, "text", encoding="UTF-8"),
               simplifyDataFrame=TRUE) %>% purrr::pluck("data")
  return(result)
}

#' Search CTPP table codes
#'
#' @param regex string pattern to match
#' @param year last of 5-year CTPP span, e.g. 2016 for ctpp1216 survey
#' @return data table
#'
#' @import data.table
#' @export
ctpp_tblsearch <- function(regex, year=2016) {
  description <- universe <- NULL
  url <- paste0("https://ctpp.macrosysrt.com/api/groups?year=", year)
  result <- api_gofer(url) %>% setDT() %>%
    .[grepl(regex, description, ignore.case=TRUE)|
      grepl(regex, universe, ignore.case=TRUE)]
  return(result)
}

#' Retrieve CTPP data
#'
#' @param table_code either a table code as string--e.g. "A302103"--or string vector of variable codes
#' @param dyear last of 5-year CTPP span, e.g. 2016 for ctpp1216 survey
#' @param scale "county", "place", or "tract" for residence/workplace tables; "county-county", "place-place", "place-county", "county-place", or "tract-tract" for O-D tables
#' @param geoids optional string vector of GEOID codes to limit the table
#' @return data table
#'
#' @importFrom stringr str_sub str_extract str_replace str_split
#' @importFrom dplyr case_when
#' @import data.table
#' @export
get_psrc_ctpp_api <- function(table_code, dyear=2016, scale, geoids=NULL){
  name <- label <- valtype <- variable <- value <- geoid <- NULL
  psrc_counties <- c("033","035","053","061")
  # Build API call URL
  tbl <- mapply(str_sub, table_code, 1L,7L) %>% unique()
  if(length(tbl)>1){return("Requested variables must come from the same table.")}
  get_arg <- paste0("?get=",
                    if(length(table_code)>1){
                      paste0(table_code, collapse=",")
                    }else{
                      paste0("group%28", tolower(table_code), "%29")})
  scale_arg <- if(!grepl("-", scale)){paste0("&for=", scale)
                      }else{paste0("&for=", str_split(scale, ",")[[1]],
                                 "&d-for=", str_split(scale, ",")[[2]])}
  if(is.null(geoids)){
    if(scale %in% c("county","tract")){
      geo_arg <- paste0("&in=county%3A", paste0(psrc_counties, collapse="%2C"),"&in=state%3A53")
    }else{
      geo_arg <-        "&in=state%3A53"
    }
  }else{
    geo_len <- lapply(geoids, length) %>% unique()
    geo_scale <- case_when(geo_len==5  ~"county",
                           geo_len==7  ~"place",
                           geo_len==11 ~"tract")
    geo_arg <- if(length(geo_len!=1)){NULL}else{
                paste0("&in=", geo_scale, "%3A", paste0(geoids, collapse="%2C"))
    }
  }
  # Retrieve labels
  labels_url <- paste0("https://ctpp.macrosysrt.com/api/groups/",
                         tbl, "/variables?year=",dyear)
  labels <- api_gofer(labels_url) %>% setDT() %>%
    .[grepl((tbl), name, ignore.case=TRUE),.(name, label)] %>% unique
  # Get data
  data_url <- paste0("https://ctpp.macrosysrt.com/api/data/",
                dyear, get_arg, scale_arg, geo_arg,"&format=list")
  data_result <- api_gofer(data_url) %>% setDT() %>%
    melt(id.vars=c("geoid","name"), variable.factor=FALSE) %>%
    .[, `:=`(valtype=str_sub(str_extract(variable,"_(e|m)\\d+$"),2L,2L),
             value=str2num(value), table_id=str_sub(variable, 1L, 7L),
             geoid=str_replace(geoid,"[ABC]\\d+US",""))] %>%
    .[labels, variable:=label, on=.(variable=name)] %>%
    dcast(table_id + geoid + name + variable ~ valtype, value.var="value") %>%
    setnames(c("geoid", "name", "variable", "e", "m"),
             c("res_geoid", "res_label", "category", "estimate", "estimate_moe"),
             skip_absent=TRUE)
  return(data_result)
}
