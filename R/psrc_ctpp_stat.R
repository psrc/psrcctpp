#' @importFrom magrittr %<>% %>%
#' @author Michael Jensen
NULL

globalVariables(c(":=", "!!", ".", "enquos"))

psrc_ctpp_require_cols <- function(df, required_cols, fn_name){
  missing_cols <- setdiff(required_cols, colnames(df))
  if(length(missing_cols) > 0L){
    stop(
      sprintf(
        "%s() requires column(s): %s",
        fn_name,
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

psrc_ctpp_is_total_category <- function(category){
  grepl("^Total\\b", as.character(category))
}

psrc_ctpp_total_summary <- function(df, group_cols = character()){
  dt <- as.data.table(df)
  if(length(group_cols) > 0L){
    dt[, .(
      total_n = sum(psrc_ctpp_is_total_category(category), na.rm = TRUE),
      category_n = uniqueN(category[!is.na(category)])
    ), by = group_cols]
  }else{
    dt[, .(
      total_n = sum(psrc_ctpp_is_total_category(category), na.rm = TRUE),
      category_n = uniqueN(category[!is.na(category)])
    )]
  }
}

psrc_ctpp_warn_total_contract <- function(df, fn_name, group_cols = character()){
  if(nrow(df) == 0L || !"category" %in% colnames(df)){
    return(invisible(NULL))
  }

  total_summary <- psrc_ctpp_total_summary(df, group_cols)
  risky_groups <- total_summary[category_n > 1L]
  if(nrow(risky_groups) == 0L){
    return(invisible(NULL))
  }

  missing_total_n <- risky_groups[total_n == 0L, .N]
  duplicate_total_n <- risky_groups[total_n > 1L, .N]

  if(missing_total_n > 0L){
    warning(
      sprintf(
        "%s(): %d grouped result(s) contain multiple categories but no category value beginning with 'Total'. If you recode category, preserve a single total row per group.",
        fn_name,
        missing_total_n
      ),
      call. = FALSE
    )
  }

  if(duplicate_total_n > 0L){
    warning(
      sprintf(
        "%s(): %d grouped result(s) contain multiple category values beginning with 'Total'. Preserve exactly one total row per group before summarizing or computing shares.",
        fn_name,
        duplicate_total_n
      ),
      call. = FALSE
    )
  }

  invisible(NULL)
}

psrc_ctpp_warn_total_collapse <- function(df, fn_name, group_cols = "table_id"){
  if(nrow(df) == 0L || !"category" %in% colnames(df)){
    return(invisible(NULL))
  }

  collapsing_groups <- psrc_ctpp_total_summary(df, group_cols)[category_n > 1L & total_n == 1L]
  if(nrow(collapsing_groups) > 0L){
    warning(
      sprintf(
        "%s(): %d grouped result(s) will sum a 'Total' row together with component rows. This is usually not intended; use group_vars = 'category' to preserve category totals.",
        fn_name,
        nrow(collapsing_groups)
      ),
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Generic call for CTPP summary statistics
#'
#' Given specific form by related \code{\link{ctpp_stat}} functions.
#' @inheritParams ctpp_stat
#' @param stat_type summary statistic to compute. Supported values are
#'   \code{"sum"} and \code{"median"}.
#'
#' @importFrom dplyr ungroup group_by filter across summarize if_all rename
#' @importFrom tidyselect all_of
#' @importFrom tidycensus moe_sum
#' @import data.table
psrc_ctpp_stat <- function(df, group_vars, stat_type="sum", incl_na=FALSE){
  table_id <- category <- estimate <- estimate_moe <- NULL
  sum_estimate <- sum_moe <- median_estimate <- NULL
  keep_existing <- identical(group_vars, "keep_existing")
  if(!stat_type %in% c("sum", "median")){
    stop(
      "psrc_ctpp_stat(): stat_type must be one of 'sum' or 'median'.",
      call. = FALSE
    )
  }
  psrc_ctpp_require_cols(df, c("table_id", "category", "estimate", "estimate_moe"), "psrc_ctpp_stat")
  if(!is.null(group_vars) && !keep_existing){
    psrc_ctpp_require_cols(df, group_vars, "psrc_ctpp_stat")
  }
  if(!keep_existing){df %<>% ungroup()}                                                             # "keep_existing" is power-user option to maintain more complex groupings;
  if(stat_type=="sum"){
    psrc_ctpp_warn_total_contract(df, "psrc_ctpp_stat", unique(c("table_id", group_vars)))
    if(is.null(group_vars)){
      psrc_ctpp_warn_total_collapse(df, "psrc_ctpp_stat")
    }
  }
  if(!is.null(group_vars) && !keep_existing){                                                       # -- otherwise the package ungroups before and afterward
    if(incl_na==FALSE){df %<>% filter(if_all(all_of(group_vars), ~ !is.na(.)))}                    # Allows users to exclude w/o removing observations from the data object itself
    df %<>% group_by(across(c(table_id, all_of(group_vars), category)))
  }
  if(stat_type=="sum"){
    rs <- suppressMessages(summarize(df, sum_estimate=sum(estimate, na.rm=TRUE),
            sum_moe=moe_sum(estimate_moe, estimate)) %>% ungroup()) %>%
      rename(estimate=sum_estimate, estimate_moe=sum_moe)
  }else if(stat_type=="median"){
    rs <- suppressMessages(summarize(df,
            median_estimate=stats::median(estimate, na.rm=TRUE)) %>% ungroup()) %>%
      rename(estimate=median_estimate)
    rs$estimate_moe <- NA_real_
    metric_cols <- c("estimate", "estimate_moe")
    rs <- rs[, c(setdiff(colnames(rs), metric_cols), metric_cols)]
  }
  if(!keep_existing){df %<>% ungroup()}
  return(rs)
}

#' CTPP summary statistics
#'
#' @param df result of get_psrc_ctpp function or a derived table that still preserves
#' the package's standard columns, including \code{table_id}, \code{category},
#' \code{estimate}, and \code{estimate_moe}
#' @param group_vars grouping variables, original or added
#' @param incl_na whether to include NA lines
#' @name ctpp_stat
#' @return A table with the variable labels, summary statistic, and margin of error.
#' Sum summaries return a propagated margin of error. Median summaries preserve the
#' standard \code{estimate_moe} column and fill it with \code{NA_real_}, because a
#' comparable median MOE is not currently computed. If you recode \code{category},
#' preserve exactly one total row per group so downstream helpers such as
#' \code{ctpp_shares()} can still identify the denominator. Sum summaries with
#' \code{group_vars = NULL} warn when the input still contains a \code{"Total"}
#' row alongside component rows, because that full collapse will add the total to
#' its parts.
NULL

#' @rdname ctpp_stat
#' @title Generate CTPP sums
#' @export
psrc_ctpp_sum <- function(df, group_vars="category", incl_na=TRUE){
  rs <- psrc_ctpp_stat(df=df, stat_type="sum", group_vars=group_vars, incl_na=incl_na)
  return(rs)
}

#' @rdname ctpp_stat
#' @title Generate CTPP medians
#' @export
psrc_ctpp_median <- function(df, group_vars="category", incl_na=TRUE){
  rs <- psrc_ctpp_stat(df=df, stat_type="median", group_vars=group_vars, incl_na=incl_na)
  return(rs)
}

#' Add shares to CTPP table
#'
#' @param df dataframe, either result of get_psrc_ctpp or psrc_ctpp_stat, that retains
#' the standard \code{category}, \code{estimate}, and \code{estimate_moe} columns.
#' Category totals must remain identifiable with a value beginning with \code{"Total"}.
#' @return dataframe with share & share_moe. If \code{ctpp_shares()} cannot identify
#' exactly one total row per group, it warns and returns \code{df} unchanged.
#'
#' @importFrom dplyr filter select select_if rename_with inner_join mutate
#' @importFrom stringr str_replace
#' @importFrom tidycensus moe_prop
#' @import data.table
#' @export
ctpp_shares <- function(df){
  category <- estimate <- estimate_moe <- total <- total_moe <- totals <- NULL
  psrc_ctpp_require_cols(df, c("category", "estimate", "estimate_moe"), "ctpp_shares")

  share_group_cols <- setdiff(colnames(df), c("table_id", "line_id", "category", "estimate", "estimate_moe"))
  total_summary <- psrc_ctpp_total_summary(df, share_group_cols)
  risky_groups <- total_summary[category_n > 1L & total_n != 1L]
  if(nrow(risky_groups) > 0L){
    psrc_ctpp_warn_total_contract(df, "ctpp_shares", share_group_cols)
    return(df)
  }

  if(!any(psrc_ctpp_is_total_category(df$category), na.rm = TRUE)){
    warning(
      "ctpp_shares(): no category value beginning with 'Total' was found, so shares were not added.",
      call. = FALSE
    )
    return(df)
  }

  totals <- filter(df, psrc_ctpp_is_total_category(category)) %>%
    select_if(!grepl("^table_id$|^line_id$|^category$", colnames(.))) %>%
    rename_with(~str_replace(., "estimate", "total"), grep("^estimate$|^estimate_moe$", colnames(.), value=TRUE))
  join_vars <- intersect(colnames(df), colnames(totals))
  joined <- if(length(join_vars) == 0){
    dplyr::cross_join(df, totals)
  }else{
    suppressMessages(inner_join(df, totals, by=join_vars, na_matches="never"))
  }
  rs <- joined %>%
    mutate(share=estimate/total,
           share_moe=moe_prop(estimate, total, estimate_moe, total_moe)) %>%
    select(-c(total, total_moe))
  return(rs)
}
