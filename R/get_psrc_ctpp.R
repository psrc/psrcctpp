#' @importFrom magrittr %<>% %>%
#' @author Michael Jensen
NULL

globalVariables(c(":=", "!!", ".", "enquos"))
psrc_counties <- c("033","035","053","061")

`%not_in%` <- Negate(`%in%`)

#' List Census Places within the PSRC region
#'
#' @param year of census geography
#' @return string vector of Place FIPS codes
#'
#' @importFrom sf st_transform st_buffer st_join st_intersects st_drop_geometry
#' @importFrom dplyr filter select rename
#' @export
get_psrc_places <- function(year){
  NAME <- COUNTYFP <- GEOID <- geometry <- psrc_region <- place_lookup <- counties <-NULL
  psrc_region <- tigris::counties("53", cb=TRUE) %>%
    filter(COUNTYFP %in% psrc_counties) %>% dplyr::summarize() %>%
    st_transform(2285) # planar projection to allow intersect
  place_lookup <- tigris::places("53", year=year, cb=TRUE) %>%
    select(c(GEOID, NAME, geometry)) %>% st_transform(2285) %>% st_buffer(-1) %>% # To avoid any overlap
    st_join(psrc_region, join=st_intersects, left=FALSE) %>% st_drop_geometry() %>% unique()
}

#' Retrieve CTPP data
#'
#' @param dyear last of 5-year CTPP span, e.g. 2016 for ctpp1216 survey
#' @param data_table requested data table as string, e.g. "A302103"
#' @param scale "state", "county", "place", or "tract" for residence/workplace tables; "state-state", "county-county", "place-place", "place-county", "county-place", or "tract-tract" for O-D tables
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
  scale_refs <- data.frame(
    scale_id   =sprintf("%02i",c(2:3,5,11,22:23,25,31,42:43,45,50:51,54)),
    table_type =c(rep(1,4),rep(2,4),rep(3,6)),
    scale_label=c(rep(c("state","county","place","tract"),2),
                  "state-state","county-county","place-place","place-county","county-place","tract-tract"),
    res_len    =c(2,5,7,11, rep(NA_integer_,4), 2,5,7,7,5,11),
    work_len   =c(rep(NA_integer_,4), 2,5,7,11, 2,5,7,5,7,11)) %>% setDT()
  scale_ref <- scale_refs[scale_label==scale & str_sub(data_table,2L,2L)==table_type]
  scale_filter <- paste0("^C", scale_refs$scale_id, "\\w+",collapse="|")

  # Value & geography lookup table ETL
  dir <- dplyr::if_else(dyear==2010,"2006_2010/","2012_2016/") %>% paste0("X:/DSA/Census/CTPP/", .)
  val_lookup <- paste0(dir,"acs_ctpp_2012thru2016_table_shell.txt") %>% fread() %>% setkeyv(c("TBLID","LINENO"))
  # rgeo <- paste0(dir,"acs_ctpp_2012thru2016_res_geo.txt") %>% fread() %>% .[,5:6] %>%
  #   .[grepl(scale_filter, GEOID)] %>% .[, GEOID:=str_replace(GEOID, "^C\\w+US", "")]
  # wgeo <- paste0(dir,"acs_ctpp_2012thru2016_pow_geo.txt") %>% fread() %>% .[,5:6] %>%
  #   .[grepl(scale_filter, GEOID)] %>% .[, GEOID:=str_replace(GEOID, "^C\\w+US", "")]
  # geo_lookup <- rbind(rgeo, wgeo) %>% unique() %>% setkeyv(c("GEOID"))
  # rm(rgeo, wgeo)
  geo_lookup <- paste0(dir,"acs_ctpp_2012thru2016_all_geo.txt") %>% fread() %>% setkeyv(c("GEOID"))

  # Load primary datatable; filter by scale, [geoid]; attach value and geography labels
  targetfile <- paste0("WA_", as.character(dyear-4), "thru", dyear, "_") %>% paste0(dir, ., data_table, ".csv")
  dt <- fread(targetfile) %>% .[grepl(paste0("^C",scale_ref$scale_id), GEOID)] %>% setkeyv(c("TBLID","LINENO")) %>%
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
      psrc_places <-  get_psrc_places(dyear)
      dt %<>% .[(res_geoid %in% psrc_places$GEOID)|(work_geoid %in% psrc_places$GEOID)]
    }else{
      pat <- paste0("^53", psrc_counties, collapse="|")
      dt %<>% .[(grepl(pat, res_geoid))|(grepl(pat, work_geoid))]
    }
  }


  dt %<>% .[, c("GEOID","SOURCE"):=NULL] %>%
    setcolorder(c("TBLID","LINENO","res_geoid", "res_label", "work_geoid", "work_label", "category", "EST", "MOE"))
  return(dt)
}
