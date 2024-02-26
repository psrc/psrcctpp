#' @importFrom magrittr %<>% %>%
#' @author Michael Jensen
NULL

globalVariables(c(":=", "!!", ".", "enquos"))
psrc_counties <- c("033","035","053","061")

str2num <- function(x){as.numeric(stringr::str_replace_all(x,"(\\+/-)|,|\\*+",""))}
`%not_in%` <- Negate(`%in%`)

#' Fetch API results
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
  if(http_type(resp)!="application/json"){
    stop("API did not return json", call.=FALSE)
  }
  result <- fromJSON(content(resp, "text", encoding="UTF-8"),
                     simplifyDataFrame=TRUE) %>% purrr::pluck("data")
  return(result)
}

#' Search CTPP table codes
#'
#' @param prefix 2-character pattern: see \code{vignette("psrcctpp_basics", package = "psrcctpp")}
#' @param regex string pattern to match table description
#' @param year last of 5-year CTPP span, e.g. 2016 for ctpp1216 survey
#' @return data table
#'
#' @import data.table
#' @importFrom stringr str_sub
#' @importFrom rlang is_empty
#' @export
ctpp_tblsearch <- function(prefix, regex, year=2016) {
  description <- universe <- name <- NULL # Declare for documentation purposes
  if(grepl("[AB\\?][123\\?]", prefix) & !is_empty(regex)){
  url <- paste0("https://ctppdata.transportation.dev/api/groups?year=", year)
  result <- api_gofer(url) %>% setDT() %>%
    .[grepl(prefix, str_sub(name, 1L, 2L)) & grepl(regex, description, ignore.case=TRUE)]
  return(result)
  }else{message("Parameters must be strings; prefix is length 2, i.e. A or")}
}

#' Search CTPP variable codes
#'
#' @param table_code requested data table as string, e.g. "A302103"
#' @param year last of 5-year CTPP span, e.g. 2016 for ctpp1216 survey
#' @return data table
#'
#' @import data.table
#' @export
ctpp_varsearch <- function(table_code, year=2016){
  group <- name <- NULL # Declare for documentation purposes
  url <- paste0("https://ctppdata.transportation.dev/api/groups/", table_code, "/variables?year=", year)
  result <- api_gofer(url) %>% setDT() %>% .[group==table_code & grepl("_e", name)] %>%
    .[,group:=NULL]
  return(result)
}

#' Correspondence table for CTPP codes, table type, and scale
#'
#' @param scale "county", "place", or "tract" for residence/workplace tables;
#' "county-county", "place-place", "place-county", "county-place", or "tract-tract" for flow tables
#' @param table_code requested data table as string, e.g. "A302103"
#' @return correspondence table
#'
scale_code_lookup <- function(scale, table_code){
  scale_label <- table_type <- NULL # Declare for documentation purposes
  scale_refs <- data.frame(
  scale_id   =c(2:3,5,11,22:23,25,31,42:43,45,50:51,54),
  table_type =c(rep(1,4),rep(2,4),rep(3,6)),
  scale_label=c(rep(c("state","county","place","tract"),2),
                "state-state","county-county","place-place","place-county","county-place","tract-tract"),
  res_len    =c(2,5,7,11, rep(NA_integer_,4), 2,5,7,7,5,11),
  work_len   =c(rep(NA_integer_,4), 2,5,7,11, 2,5,7,5,7,11)) %>% setDT() %>%
  .[scale_label==scale & as.integer(str_sub(table_code,2L,2L))==table_type]
return(scale_refs)
}

#' Fetch ftp-format CTPP data from network location
#' Helper to get_psrc_ctpp
#'
#' @param scale "county", "place", or "tract" for residence/workplace tables;
#' "county-county", "place-place", "place-county", "county-place", or "tract-tract" for O-D tables
#' @param table_code requested data table as string, e.g. "A302103"
#' @param dyear last of 5-year CTPP span, e.g. 2016 for ctpp1216 survey
#' @param filepath optional network location of downloaded ftp CTPP files
#' @return data table
#' @importFrom stringr str_sub str_extract str_replace
#' @importFrom dplyr if_else
#' @import data.table
fetch_ctpp_from_file <- function(scale, table_code, dyear, filepath="default"){
  scale_filter <- scale_label <- dir <- targetfile <- geoid <- ldesc <- name <- NULL  # Declare for documentation purposes
  #rgeo <- wgeo <- geo_lookup <- GEOID <- LDESC <- NULL
  res_geoid <- work_geoid <- res_label <- work_label <- table_type <- category <- NULL
  # Create geography prefix lookup per scale
  scale_ref <- scale_code_lookup(scale, table_code)
  # Value & geography lookup table ETL
  dir <- if_else(dyear==2010,"2006_2010/","2012_2016/") %>%
    paste0(if_else(filepath!="default", filepath, "X:/DSA/Census/CTPP/"), .)
  val_lookup <- paste0(dir,"acs_ctpp_2012thru2016_table_shell.txt") %>% fread(showProgress=FALSE) %>%
      setnames(tolower(colnames(.))) %>% setnames("tblid", "table_id") %>% setkeyv(c("table_id","lineno"))

  # Steps for ftp files already saved to network file
  # scale_filter <- paste0("^C", sprintf("%02i",scale_ref$scale_id), "\\w+",collapse="|")
  # rgeo <- paste0(dir,"acs_ctpp_2012thru2016_res_geo.txt") %>% fread() %>% .[,5:6] %>%
  #   .[grepl(scale_filter, GEOID)] %>% .[, GEOID:=str_replace(GEOID, "^C\\w+US", "")]
  # wgeo <- paste0(dir,"acs_ctpp_2012thru2016_pow_geo.txt") %>% fread() %>% .[,5:6] %>%
  #   .[grepl(scale_filter, GEOID)] %>% .[, GEOID:=str_replace(geoid, "^C\\w+US", "")]
  # geo_lookup <- rbind(rgeo, wgeo) %>% unique() %>% setnames(tolower(colnames(.))) %>% setkeyv(c("geoid"))
  # rm(rgeo, wgeo)
  geo_lookup <- paste0(dir,"acs_ctpp_2012thru2016_all_geo.txt") %>%
    fread(colClasses=rep("character",2),showProgress=FALSE) %>%
    setnames(tolower(colnames(.))) %>% setkeyv(c("geoid"))

  # Load primary datatable; filter by scale, geoid; attach value and geography labels
  targetfile <- paste0("WA_", as.character(dyear-4), "thru", dyear, "_") %>% paste0(dir, ., table_code, ".csv")
  dt <- fread(targetfile, showProgress=FALSE) %>% setnames(tolower(colnames(.))) %>%
    .[, c("est", "moe"):=lapply(.SD, str2num), .SDcols=c("est", "moe")] %>%
    .[grepl(paste0("^C", sprintf("%02i", scale_ref$scale_id)), geoid)] %>%
    setnames("tblid", "table_id")  %>% setkeyv(c("table_id","lineno")) %>%
    .[val_lookup, category:=ldesc, on=key(.)]
  dt[, (c("res_geoid","res_label","work_geoid","work_label")):=NA_character_]
  if(scale_ref$table_type %in% c(1,3)){
    dt %<>% .[, res_geoid:=str_sub(str_extract(geoid,"US\\d+"),3L,(2+scale_ref$res_len))]
    dt[geo_lookup, res_label:=name, on=.(res_geoid=geoid)]
  }
  if(scale_ref$table_type %in% c(2,3)){
    dt %<>% .[, work_geoid:=str_sub(str_extract(geoid,"US\\d+"),-(scale_ref$work_len))]
    dt[geo_lookup, work_label:=name, on=.(work_geoid=geoid)]
  }
  dt %<>% .[, c("geoid","source","lineno"):=NULL] %>%
    setnames(c("est", "moe"), c("estimate", "estimate_moe"))
  return(dt)
}

#' Fetch CTPP data from AASHTO api
#' Helper to get_psrc_ctpp
#'
#' @param scale "county", "place", or "tract" for residence/workplace tables;
#' "county-county", "place-place", "place-county", "county-place", or "tract-tract" for O-D tables
#' @param table_code requested data table as string, e.g. "A302103"
#' @param dyear last of 5-year CTPP span, e.g. 2016 for ctpp1216 survey
#' @param geoids optional string vector of GEOID codes to limit the table
#' @return data table
#' @importFrom stringr str_sub str_extract str_replace str_split
#' @importFrom dplyr case_when
#' @import data.table
fetch_ctpp_from_api <- function(scale, table_code, dyear, geoids){
  name <- label <- valtype <- variable <- value <- geoid <- NULL
  scale_ref <- scale_code_lookup(scale, table_code)
  psrc_counties <- c("033","035","053","061")

  # Build API call URL
  tbl <- mapply(str_sub, table_code, 1L,7L) %>% unique()
  if(length(tbl)>1){return("Requested variables must come from the same table.")}
  get_arg <- paste0("?get=",
                    if(length(table_code)>1){
                      paste0(c(tolower(table_code),
                             (str_replace(tolower(table_code),"e","m"))), # add codes for moe
                             collapse="%2C")
                    }else{
                      paste0("group%28", tolower(table_code), "%29")})
  scale_arg <- if(!grepl("-", scale)){paste0("&for=", scale)
  }else{paste0("&for=", str_split(scale, "-")[[1]][1],
               "&d-for=", str_split(scale, "-")[[1]][2])}
  if(is.null(geoids)){
    if(grep("^\\w+\\b", scale, value=TRUE) %in% c("county","tract")){
      geo_arg <- paste0("&in=county%3A", paste0(psrc_counties, collapse="%2C"),"&in=state%3A53")
    }else{
      geo_arg <- "&in=state%3A53"
    }
    if(grepl("-\\w+$", scale)){
      if(grep("-\\w+$", scale, value=TRUE) %in% c("-county","-tract")){
      geo_arg %<>% paste0("&d-in=county%3A", paste0(psrc_counties, collapse="%2C"),"&d-in=state%3A53")
      }else if(grep("-\\w+$", scale, value=TRUE)=="-place"){
      geo_arg %<>% "&d-in=state%3A53"
      }
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
  labels_url <- paste0("https://ctppdata.transportation.dev/api/groups/",
                       tbl, "/variables?year=",dyear)
  labels <- api_gofer(labels_url) %>% setDT() %>%
    .[grepl((tbl), name, ignore.case=TRUE),.(name, label)] %>%
    .[, name:=tolower(name)] %>% unique()

  # Get data
  data_url <- paste0("https://ctppdata.transportation.dev/api/data/",
                     dyear, get_arg, scale_arg, geo_arg,"&format=list")
  dt <- api_gofer(data_url) %>% setDT()
  melt_vars <- grep("^geoid$|name$", colnames(dt), value=TRUE)
  dt %<>% melt(id.vars=melt_vars, variable.factor=FALSE) %>%
    .[, `:=`(valtype=str_sub(str_extract(variable,"_(e|m)\\d+$"),2L,2L),
             value=str2num(value), table_id=str_sub(variable, 1L, 7L),
             variable_id=tolower(variable),
             geoid=str_replace(geoid,"[ABC]\\d+US",""))] %>%
    .[labels, variable:=label, on=.(variable=name)] %>%
    dcast(... ~ valtype, value.var="value")  %>%
    setnames(c("variable", "e", "m"), c("category", "estimate", "estimate_moe"))
  if(scale_ref$table_type==1){
    dt %<>% .[,c("work_geoid", "work_label"):=NA_character_] %>%
      setnames(c("geoid","name"),c("res_geoid", "res_label"))
  }else if(scale_ref$table_type==2){
    dt %<>% .[,c("res_geoid", "res_label"):=NA_character_] %>%
      setnames(c("geoid","name"),c("work_geoid", "work_label"))
  }else if(scale_ref$table_type==3){
    dt %<>% .[, `:=`(res_geoid=str_sub(geoid,3L,(2+scale_ref$res_len)),
                              work_geoid=str_sub(geoid,-(scale_ref$work_len)))] %>%
      .[, geoid:=NULL] %>%
      setnames(c("origin_name","destination_name"),
               c("res_label", "work_label"))
  }
  return(dt)
}

#' Retrieve CTPP data
#'
#' @param scale "county", "place", or "tract" for residence/workplace tables;
#' "county-county", "place-place", "place-county", "county-place", or "tract-tract" for O-D tables
#' @param table_code requested data table as string, e.g. "A302103"
#' @param dyear last of 5-year CTPP span, e.g. 2016 for ctpp1216 survey
#' @param geoids optional string vector of GEOID codes to limit the table
#' @param filepath optional network location of downloaded ftp CTPP files
#' @return data table
#'
#' @import data.table
#' @export
get_psrc_ctpp <- function(scale, table_code, dyear=2016, geoids=NULL, filepath=NULL){
  # Declare variables (to avoid package warnings)
  res_geoid <- work_geoid <- GEOID <- NULL
  psrc_counties <- c("033","035","053","061")
  scale_ref <- scale_code_lookup(scale, table_code)

  # Get data - API is default source; use filepath if API is not working
  if(!is.null(filepath)){
    dt <- fetch_ctpp_from_file(scale, table_code, dyear, filepath)
  }else{
    dt <- fetch_ctpp_from_api(scale, table_code, dyear, geoids)
  }

  # Filtering
  if(!rlang::is_empty(geoids)){
    dt %<>% .[(res_geoid %in% geoids)|(work_geoid %in% geoids)]
  }else{
    if(scale_ref$scale_id %in% c(5,25,45,50,51)){
      psrc_places <- suppressMessages(psrccensus::get_psrc_places(dyear)) %>% sf::st_drop_geometry() %>% unique()
      dt %<>% .[(res_geoid %in% psrc_places$GEOID)|(work_geoid %in% psrc_places$GEOID)]
    }else{
      pat <- paste0("^53", psrc_counties, collapse="|")
      dt %<>% .[(grepl(pat, res_geoid))|(grepl(pat, work_geoid))]
    }
  }
  # Order columns
  dt %<>% setcolorder(sort(setdiff(colnames(dt), c("category", "estimate", "estimate_moe")))) %>%
    setcolorder("table_id")
  return(dt)
}

#' Generic call for CTPP summary statistics
#'
#' Given specific form by related \code{\link{ctpp_stat}} functions.
#' @inheritParams ctpp_stat
#' @param stat_type for now, "sum" is only option
#'
#' @importFrom dplyr ungroup group_by filter across summarize if_all rename
#' @importFrom tidyselect all_of
#' @importFrom tidycensus moe_sum
#' @import data.table
psrc_ctpp_stat <- function(df, group_vars, stat_type="sum", incl_na=FALSE){
  table_id <- line_id <- category <- estimate <- estimate_moe <- NULL
  sum_estimate <- sum_moe <- NULL
  if(all(group_vars!="keep_existing")){df %<>% ungroup()}                                          # "keep_existing" is power-user option to maintain more complex groupings;
  if(all(!is.null(group_vars) & group_vars!="keep_existing")){                                     # -- otherwise the package ungroups before and afterward
    if(incl_na==FALSE){df %<>% filter(if_all(all_of(group_vars), ~ !is.na(.)))}                    # Allows users to exclude w/o removing observations from the data object itself
    df %<>% group_by(across(c(table_id, line_id, all_of(group_vars), category)))
  }
  if(stat_type=="sum"){
    rs <- suppressMessages(summarize(df, sum_estimate=sum(estimate, na.rm=TRUE),
            sum_moe=moe_sum(estimate_moe, estimate)) %>% ungroup()) %>%
      rename(estimate=sum_estimate, estimate_moe=sum_moe)
  }
  if(all(group_vars!="keep_existing")){df %<>% ungroup()}
  return(rs)
}

#' CTPP summary statistics
#'
#' @param df result of get_psrc_ctpp function
#' @param group_vars grouping variables, original or added
#' @param incl_na whether to include NA lines
#' @name ctpp_stat
#' @return A table with the variable names and labels, summary statistic and margin of error
NULL

#' @rdname ctpp_stat
#' @title Generate CTPP sums
#' @export
psrc_ctpp_sum <- function(df, group_vars=NULL, incl_na=TRUE){
  rs <- psrc_ctpp_stat(df=df, stat_type="sum", group_vars=group_vars, incl_na=incl_na)
  return(rs)
}

#' Add shares to CTPP table
#'
#' @param df dataframe, either result of get_psrc_ctpp or psrc_ctpp_stat
#' @return dataframe with share & share_moe
#'
#' @importFrom dplyr filter select select_if rename_with inner_join mutate
#' @importFrom rlang is_empty
#' @importFrom stringr str_replace
#' @importFrom tidycensus moe_prop
#' @import data.table
#' @export
ctpp_shares <- function(df){
  category <- estimate <- estimate_moe <- total <- total_moe <- totals <- NULL
  if(is_empty(grepl("^Total",df$category))){return(df)}else{
    totals <- filter(df, grepl("^Total", category)) %>% select_if(!grepl("^table_id$|^line_id$|^category$", colnames(.))) %>%
      rename_with(~str_replace(., "estimate", "total"), grep("^estimate$|^estimate_moe$", colnames(.), value=TRUE))
    rs <- suppressMessages(inner_join(df, totals, na_matches="never") %>%
      mutate(share=estimate/total,
             share_moe=moe_prop(estimate, total, estimate_moe, total_moe)) %>%
      select(-c(total, total_moe)))
    return(rs)
  }
}
