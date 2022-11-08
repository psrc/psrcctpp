#' @importFrom magrittr %<>% %>%
#' @author Michael Jensen
NULL

globalVariables(c(":=", "!!", ".", "enquos"))
psrc_counties <- c("033","035","053","061")

str2num <- function(x){as.numeric(stringr::str_replace_all(x,"(\\+/-)|,",""))}
`%not_in%` <- Negate(`%in%`)

#' Correspondence table for CTPP codes, table type, and scale
#'
#' @return correspondence table
#'
scale_code_lookup <- function(){
scale_refs <- data.frame(
  scale_id   =sprintf("%02i",c(2:3,5,11,22:23,25,31,42:43,45,50:51,54)),
  table_type =c(rep(1,4),rep(2,4),rep(3,6)),
  scale_label=c(rep(c("state","county","place","tract"),2),
                "state-state","county-county","place-place","place-county","county-place","tract-tract"),
  res_len    =c(2,5,7,11, rep(NA_integer_,4), 2,5,7,7,5,11),
  work_len   =c(rep(NA_integer_,4), 2,5,7,11, 2,5,7,5,7,11)) %>% setDT()
return(scale_refs)
}

#' Retrieve CTPP data
#'
#' @param dyear last of 5-year CTPP span, e.g. 2016 for ctpp1216 survey
#' @param data_table requested data table as string, e.g. "A302103"
#' @param scale "county", "place", or "tract" for residence/workplace tables; "county-county", "place-place", "place-county", "county-place", or "tract-tract" for O-D tables
#' @param geoids optional string vector of GEOID codes to limit the table
#' @return data table
#'
#' @importFrom stringr str_sub str_extract str_replace
#' @import data.table
#' @export
get_psrc_ctpp <- function(dyear=2016, data_table, scale, geoids=NULL){
  # Declare variables (to avoid package warnings)
  scale_refs <- scale_ref <- scale_filter <- scale_label <- dir <- val_lookup <- dt <- NULL
  rgeo <- wgeo <- geo_lookup <- res_geoid <- work_geoid <- res_label <- work_label <- NULL
  psrc_places <- targetfile <- pat <- NAME <- GEOID <- category <- NULL

  # Create geography prefix lookup per scale
  scale_ref <- scale_code_lookup() %>%
    .[scale_label==scale & str_sub(data_table,2L,2L)==table_type]
  scale_filter <- paste0("^C", scale_refs$scale_id, "\\w+",collapse="|")

  # Value & geography lookup table ETL
  dir <- dplyr::if_else(dyear==2010,"2006_2010/","2012_2016/") %>% paste0("X:/DSA/Census/CTPP/", .)
  val_lookup <- paste0(dir,"acs_ctpp_2012thru2016_table_shell.txt") %>%
    fread(showProgress=FALSE) %>% setkeyv(c("TBLID","LINENO"))
  # rgeo <- paste0(dir,"acs_ctpp_2012thru2016_res_geo.txt") %>% fread() %>% .[,5:6] %>%
  #   .[grepl(scale_filter, GEOID)] %>% .[, GEOID:=str_replace(GEOID, "^C\\w+US", "")]
  # wgeo <- paste0(dir,"acs_ctpp_2012thru2016_pow_geo.txt") %>% fread() %>% .[,5:6] %>%
  #   .[grepl(scale_filter, GEOID)] %>% .[, GEOID:=str_replace(GEOID, "^C\\w+US", "")]
  # geo_lookup <- rbind(rgeo, wgeo) %>% unique() %>% setkeyv(c("GEOID"))
  # rm(rgeo, wgeo)
  geo_lookup <- paste0(dir,"acs_ctpp_2012thru2016_all_geo.txt") %>%
    fread(colClasses=rep("character",2),showProgress=FALSE) %>% setkeyv(c("GEOID"))

  # Load primary datatable; filter by scale, [geoid]; attach value and geography labels
  targetfile <- paste0("WA_", as.character(dyear-4), "thru", dyear, "_") %>% paste0(dir, ., data_table, ".csv")
  dt <- fread(targetfile, showProgress=FALSE) %>%
    .[, c("EST", "MOE"):=lapply(.SD, str2num), .SDcols=c("EST", "MOE")] %>%
    .[grepl(paste0("^C",scale_ref$scale_id), GEOID)] %>% setkeyv(c("TBLID","LINENO")) %>%
    .[val_lookup, category:=LDESC, on=key(.)]
  dt[, (c("res_geoid","res_label","work_geoid","work_label")):=""]
  if(scale_ref$table_type %in% c(1,3)){
    dt %<>% .[, res_geoid:=str_sub(str_extract(GEOID,"US\\d+"),3L,(2+scale_ref$res_len))]
    dt[geo_lookup, res_label:=NAME, on=.(res_geoid=GEOID)]
    }
  if(scale_ref$table_type %in% c(2,3)){
    dt %<>% .[, work_geoid:=str_sub(str_extract(GEOID,"US\\d+"),-(scale_ref$work_len))]
    dt[geo_lookup, work_label:=NAME, on=.(work_geoid=GEOID)]
    }
  if(!rlang::is_empty(geoids)){
    dt %<>% .[(res_geoid %in% geoids)|(work_geoid %in% geoids)]
  }else{
    if(scale_ref$scale_id %in% c(5,25,45,50,51)){
      psrc_places <-  psrccensus::get_psrc_places(dyear) %>% sf::st_drop_geometry() %>% unique()
      dt %<>% .[(res_geoid %in% psrc_places$GEOID)|(work_geoid %in% psrc_places$GEOID)]
    }else{
      pat <- paste0("^53", psrc_counties, collapse="|")
      dt %<>% .[(grepl(pat, res_geoid))|(grepl(pat, work_geoid))]
    }
  }
  dt %<>% .[, c("GEOID","SOURCE"):=NULL] %>%
    setnames(c("TBLID","LINENO", "EST", "MOE"),c(c("table_id","line_id", "estimate", "moe"))) %>%
    setcolorder(c("table_id","line_id","res_geoid", "res_label", "work_geoid", "work_label", "category", "estimate", "moe"))
  return(dt)
}

#' Generic call for CTPP summary statistics
#'
#' Given specific form by related \code{\link{ctpp_stat}} functions.
#' @inheritParams ctpp_stat
#' @param stat_type for now, "sum" is only option
#'
#' @importFrom dplyr ungroup group_by filter across summarize if_all
#' @importFrom rlang is_empty
#' @importFrom tidyselect all_of
#' @importFrom tidycensus moe_sum
#' @import data.table
psrc_ctpp_stat <- function(df, group_vars, stat_type="sum", incl_na=FALSE){
  sum_estimate <- sum_moe <- NULL
  if(all(group_vars!="keep_existing")){df %<>% ungroup()}                                          # "keep_existing" is power-user option to maintain more complex groupings;
  if(all(!is.null(group_vars) & group_vars!="keep_existing")){                                     # -- otherwise the package ungroups before and afterward
    if(incl_na==FALSE){df %<>% filter(if_all(all_of(group_vars), ~ !is.na(.)))}                    # Allows users to exclude w/o removing observations from the data object itself
    df %<>% group_by(across(c(table_id, line_id, all_of(group_vars), category)))
  }
  if(stat_type=="sum"){
    rs <- suppressMessages(summarize(df, sum_estimate=sum(estimate, na.rm=TRUE),
                        sum_moe=tidycensus::moe_sum(moe, estimate)) %>% ungroup())
  }
  if(!is_empty(rs$category=="Total")){
    totals <- filter(rs, category=="Total") %>% select(-c(table_id, line_id, category)) %>%
      rename(total_estimate=sum_estimate, total_moe=sum_moe)
    rs %<>% inner_join(totals, by=group_vars, na_matches="never") %>%
      mutate(share=sum_estimate/total_estimate,
             share_moe=moe_prop(sum_estimate, total_estimate,sum_moe, total_moe)) %>%
      select(-c(total_estimate, total_moe))
    }
  if(all(group_vars!="keep_existing")){df %<>% ungroup()}
  return(rs)
}

#' CTPP summary statistics
#'
#' @param ctpp_data result of get_psrc_ctpp function
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
