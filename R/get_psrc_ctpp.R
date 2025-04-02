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
#' @importFrom httr GET http_status http_type add_headers content
#' @importFrom purrr pluck
#' @importFrom jsonlite fromJSON
api_gofer <- function(url) {
  # Check if API key is set
  if (Sys.getenv("CTPP_API_KEY") == "") {
    stop("CTPP_API_KEY environment variable not set. Use Sys.setenv(CTPP_API_KEY='your_key')", call. = FALSE)
  }

  h <- c("x-api-key" = Sys.getenv("CTPP_API_KEY"), "accept" = "application/json")

  # Try-catch for network errors
  tryCatch({
    resp <- GET(url, add_headers(.headers = h))

    # Check HTTP status code
    if (http_status(resp)$category != "Success") {
      error_msg <- paste("API request failed with status:",
                         http_status(resp)$message,
                         "\nURL:", url)
      stop(error_msg, call. = FALSE)
    }

    # Check content type
    if (http_type(resp) != "application/json") {
      stop("API did not return JSON. Received: ", http_type(resp), call. = FALSE)
    }

    # Parse response
    result <- tryCatch({
      content <- content(resp, "text", encoding = "UTF-8")
      if (content == "" || is.null(content)) {
        stop("Empty response from API", call. = FALSE)
      }
      parsed <- fromJSON(content, simplifyDataFrame = TRUE)
      if (is.null(parsed$data)) {
        stop("API response missing 'data' field", call. = FALSE)
      }
      parsed %>% purrr::pluck("data")
    }, error = function(e) {
      stop("Failed to parse API response: ", e$message, call. = FALSE)
    })

    return(result)
  }, error = function(e) {
    if (grepl("Could not resolve host", e$message)) {
      stop("Network connection error: Unable to reach API server", call. = FALSE)
    } else if (grepl("Timeout", e$message)) {
      stop("Request timed out. The API server may be overloaded", call. = FALSE)
    } else {
      stop("API request failed: ", e$message, call. = FALSE)
    }
  })
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
ctpp_tblsearch <- function(prefix, regex, year){
  description <- universe <- name <- NULL # Declare for documentation purposes
  if(grepl("[ABC\\?][123\\?]", prefix) & !rlang::is_empty(regex)){
  url <- paste0("https://ctppdata.transportation.dev/api/groups?year=", year)
  result <- api_gofer(url) %>% setDT() %>%
    .[grepl(prefix, str_sub(name, 1L, 2L)) & grepl(regex, description, ignore.case=TRUE)]
  return(result)
  }else{message("Review your parameters; prefix is length 2, a letter (ABC) then a number (123)")}
}

#' Search CTPP variable codes
#'
#' @param table_code requested data table as string, e.g. "A302103"
#' @param year last of 5-year CTPP span, e.g. 2016 for ctpp1216 survey
#' @return data table
#'
#' @import data.table
#' @export
ctpp_varsearch <- function(table_code, year){
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
fetch_ctpp_from_file <- function(scale, table_code, dyear=2016, filepath="default"){
  scale_filter <- scale_label <- dir <- targetfile <- geoid <- ldesc <- name <- NULL  # Declare for documentation purposes
  #rgeo <- wgeo <- geo_lookup <- GEOID <- LDESC <- NULL
  res_geoid <- work_geoid <- res_label <- work_label <- table_type <- category <- NULL
  # Create geography prefix lookup per scale
  scale_ref <- scale_code_lookup(scale, table_code)
  # Value & geography lookup table ETL
  dir <- case_when(dyear==2010 ~"2006_2010/", dyear==2016 ~"2012_2016/", dyear==2021 ~"2017_2021/") %>%
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
fetch_ctpp_from_api <- function(scale, table_code, dyear, geoids) {
  name <- label <- valtype <- variable <- value <- geoid <- NULL

  tryCatch({
    # Validate scale and table code compatibility
    scale_ref <- tryCatch({
      scale_code_lookup(scale, table_code)
    }, error = function(e) {
      stop("Failed to match scale and table_code: ", e$message, call. = FALSE)
    })

    if (nrow(scale_ref) == 0) {
      stop("Invalid combination of scale '", scale, "' and table_code '",
           table_code, "'. Table type does not match scale.", call. = FALSE)
    }

    # Build API call URL - with validation
    tbl <- unique(mapply(str_sub, table_code, 1L, 7L))
    if (length(tbl) > 1) {
      stop("Requested variables must come from the same table.", call. = FALSE)
    }

    # Create get_arg with validation
    get_arg <- if (length(table_code) > 1) {
      # Multiple variables
      var_names <- c(tolower(table_code), str_replace(tolower(table_code), "e", "m"))
      paste0("?get=", paste0(var_names, collapse = "%2C"))
    } else {
      # Whole group
      paste0("?get=group%28", tolower(table_code), "%29")
    }

    # Create scale_arg with validation
    if (!grepl("-", scale)) {
      scale_arg <- paste0("&for=", scale)
    } else {
      parts <- str_split(scale, "-")[[1]]
      if (length(parts) != 2) {
        stop("Invalid scale format for flow table: ", scale,
             ". Should be origin-destination format.", call. = FALSE)
      }
      scale_arg <- paste0("&for=", parts[1], "&d-for=", parts[2])
    }

    # Create geo_arg with validation
    if (is.null(geoids)) {
      geo_arg <- if (grep("^\\w+\\b", scale, value = TRUE) %in% c("county", "tract")) {
        paste0("&in=county%3A", paste0(psrc_counties, collapse = "%2C"), "&in=state%3A53")
      } else {
        "&in=state%3A53"
      }

      if (grepl("-\\w+$", scale)) {
        dest_type <- grep("-\\w+$", scale, value = TRUE)
        if (dest_type %in% c("-county", "-tract")) {
          geo_arg %<>% paste0("&d-in=county%3A", paste0(psrc_counties, collapse = "%2C"),
                              "&d-in=state%3A53")
        } else if (dest_type == "-place") {
          geo_arg %<>% paste0("&d-in=state%3A53")
        }
      }
    } else {
      # Check if all geoids have the same length
      geo_lengths <- unique(nchar(geoids))
      if (length(geo_lengths) != 1) {
        stop("All geoids must have the same length. Found lengths: ",
             paste(geo_lengths, collapse = ", "), call. = FALSE)
      }

      geo_scale <- case_when(
        geo_lengths == 5  ~ "county",
        geo_lengths == 7  ~ "place",
        geo_lengths == 11 ~ "tract",
        TRUE ~ NA_character_
      )

      if (is.na(geo_scale)) {
        stop("Invalid geoid length: ", geo_lengths,
             ". Must be 5 (county), 7 (place), or 11 (tract).", call. = FALSE)
      }

      geo_arg <- paste0("&in=", geo_scale, "%3A", paste0(geoids, collapse = "%2C"))
    }

    # Retrieve labels
    labels_url <- paste0("https://ctppdata.transportation.dev/api/groups/",
                         tbl, "/variables?year=", dyear)

    labels <- tryCatch({
      result <- api_gofer(labels_url)
      if (is.null(result) || nrow(result) == 0) {
        stop("No variable labels found for table ", tbl, " and year ", dyear, call. = FALSE)
      }
      result
    }, error = function(e) {
      stop("Failed to fetch variable labels: ", e$message,
           "\nURL: ", labels_url, call. = FALSE)
    }) %>%
      setDT() %>%
      .[grepl((tbl), name, ignore.case = TRUE), .(name, label)] %>%
      .[, name := tolower(name)] %>%
      unique()

    if (nrow(labels) == 0) {
      stop("No matching variables found for table ", tbl, call. = FALSE)
    }

    # Get data
    data_url <- paste0("https://ctppdata.transportation.dev/api/data/",
                       dyear, get_arg, scale_arg, geo_arg, "&format=list")

    dt <- tryCatch({
      result <- api_gofer(data_url)
      if (is.null(result)) {
        stop("Empty data returned from API", call. = FALSE)
      }
      result
    }, error = function(e) {
      if (grepl("HTTP 400|Bad Request", e$message)) {
        if (grepl("geoid", e$message, ignore.case = TRUE)) {
          stop("Invalid geography specified. Check your geoids parameter.", call. = FALSE)
        } else {
          stop("API request error: ", e$message,
               "\nCheck your scale, table_code, and geoids parameters.", call. = FALSE)
        }
      } else if (grepl("HTTP 404", e$message)) {
        stop("Data not found. Table ", tbl, " may not exist for year ", dyear,
             " or scale ", scale, ".", call. = FALSE)
      } else {
        stop("Failed to fetch data: ", e$message,
             "\nURL: ", data_url, call. = FALSE)
      }
    }) %>% setDT()

    if (nrow(dt) == 0) {
      warning("Query returned zero rows. URL: ", data_url, call. = FALSE)
      # Return empty but properly structured data.table
      cols <- c("table_id", "category", "estimate", "estimate_moe",
                "res_geoid", "res_label", "work_geoid", "work_label")
      return(setnames(data.table(matrix(ncol = length(cols), nrow = 0)), cols))
    }

    # Validate required columns
    required_cols <- c("geoid")
    missing_cols <- setdiff(required_cols, names(dt))
    if (length(missing_cols) > 0) {
      stop("API response missing required columns: ",
           paste(missing_cols, collapse = ", "), call. = FALSE)
    }

    # Process data
    tryCatch({
      melt_vars <- grep("^geoid$|name$", colnames(dt), value = TRUE)
      dt %<>% melt(id.vars = melt_vars, variable.factor = FALSE) %>%
        .[, `:=`(
          valtype = str_sub(str_extract(variable, "_(e|m)\\d+$"), 2L, 2L),
          value = str2num(value),
          table_id = str_sub(variable, 1L, 7L),
          variable_id = tolower(str_replace(variable, "_m", "_e")),
          geoid = str_replace(geoid, "[ABC]\\d+US", "")
        )]

      # Join with labels
      dt <- dt[labels, variable := label, on = .(variable_id = name)]

      # Check if labels were joined successfully
      if (dt[, sum(is.na(variable))] > 0) {
        warning("Some variables could not be matched with labels", call. = FALSE)
      }

      # Cast data
      dt %<>% dcast(... ~ valtype, value.var = "value") %>%
        setnames(c("variable", "e", "m"), c("category", "estimate", "estimate_moe"))

      # Structure output based on table type
      if (scale_ref$table_type == 1) {
        dt %<>% .[, c("work_geoid", "work_label") := NA_character_] %>%
          setnames(c("geoid", "name"), c("res_geoid", "res_label"))
      } else if (scale_ref$table_type == 2) {
        dt %<>% .[, c("res_geoid", "res_label") := NA_character_] %>%
          setnames(c("geoid", "name"), c("work_geoid", "work_label"))
      } else if (scale_ref$table_type == 3) {
        # For flow tables
        if (any(is.na(scale_ref$res_len)) || any(is.na(scale_ref$work_len))) {
          stop("Missing length specifications for residence or workplace geographies", call. = FALSE)
        }

        dt %<>% .[, `:=`(
          res_geoid = str_sub(geoid, 3L, (2 + scale_ref$res_len)),
          work_geoid = str_sub(geoid, -(scale_ref$work_len))
        )] %>%
          .[, geoid := NULL]

        # Handle different column names for flow tables
        if ("origin_name" %in% names(dt) && "destination_name" %in% names(dt)) {
          dt %<>% setnames(c("origin_name", "destination_name"),
                           c("res_label", "work_label"))
        } else {
          warning("Expected columns 'origin_name' and 'destination_name' not found in flow table",
                  call. = FALSE)
        }
      }

      return(dt)
    }, error = function(e) {
      stop("Error processing API response: ", e$message, call. = FALSE)
    })
  }, error = function(e) {
    # Catch any other errors
    stop("Error in fetch_ctpp_from_api: ", e$message, call. = FALSE)
  })
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
#' @importFrom sf st_drop_geometry
#' @importFrom psrccensus get_psrc_places
#' @importFrom rlang is_empty
#' @export
get_psrc_ctpp <- function(scale, table_code, dyear = 2016, geoids = NULL, filepath = NULL) {
  # Declare variables (to avoid package warnings)
  res_geoid <- work_geoid <- GEOID <- NULL

  # Validate inputs
  if (missing(scale) || !is.character(scale) || length(scale) != 1) {
    stop("'scale' must be a single character string", call. = FALSE)
  }

  valid_scales <- c("county", "place", "tract",
                    "county-county", "place-place", "place-county",
                    "county-place", "tract-tract")
  if (!(scale %in% valid_scales)) {
    stop("Invalid 'scale'. Must be one of: ",
         paste(valid_scales, collapse = ", "), call. = FALSE)
  }

  if (missing(table_code) || !is.character(table_code)) {
    stop("'table_code' must be a character string", call. = FALSE)
  }

  if (!all(grepl("^[ABC][123]\\d{5}(_(e|m)\\d{1,2})?$", table_code))) {
    stop("'table_code' format should match pattern 'B1XXXXX'", call. = FALSE)
  }

  if (!is.numeric(dyear) || dyear < 2000 || dyear > 2030) {
    stop("'dyear' must be a valid year (e.g., 2016)", call. = FALSE)
  }

  if (!is.null(geoids) && !is.character(geoids)) {
    stop("'geoids' must be NULL or a character vector", call. = FALSE)
  }

  # Try to get scale reference to validate scale/table_code compatibility
  scale_ref <- tryCatch({
    scale_code_lookup(scale, table_code[1])
  }, error = function(e) {
    stop("Error in scale and table code combination: ", e$message, call. = FALSE)
  })

  if (nrow(scale_ref) == 0) {
    stop("Invalid combination of scale '", scale, "' and table_code '",
         table_code[1], "'. Table type does not match scale.", call. = FALSE)
  }

  # Fetch data
  tryCatch({
    if (!is.null(filepath)) {
      dt <- fetch_ctpp_from_file(scale, table_code, dyear, filepath)
    } else {
      dt <- fetch_ctpp_from_api(scale, table_code, dyear, geoids)
    }

    # Validate returned data
    if (is.character(dt) && length(dt) == 1) {
      stop(dt, call. = FALSE)  # Error message from helper function
    }

    if (nrow(dt) == 0) {
      warning("Query returned zero rows. Check your parameters.", call. = FALSE)
      return(dt)
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

  }, error = function(e) {
    if (grepl("HTTP 401|unauthorized|Unauthorized", e$message)) {
      stop("API authentication failed. Check your API key.", call. = FALSE)
    } else if (grepl("HTTP 404|Not Found", e$message)) {
      stop("Resource not found. Check table_code and year.", call. = FALSE)
    } else {
      stop("Failed to fetch CTPP data: ", e$message, call. = FALSE)
    }
  })
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
