sample_ctpp_table <- function(){
  data.frame(
    table_id = rep("B202105", 3),
    category = c("Total", "Bike/Ped", "Transit"),
    estimate = c(10, 4, 6),
    estimate_moe = c(1, 0.4, 0.6),
    custom_geo = rep("Downtown", 3),
    stringsAsFactors = FALSE
  )
}

test_that("psrc_ctpp_sum warns when grouped categories lose an identifiable total", {
  df <- sample_ctpp_table()
  df$category <- c("All Workers", "Bike/Ped", "Transit")

  expect_warning(
    psrc_ctpp_sum(df, group_vars = "custom_geo", incl_na = FALSE),
    "no category value beginning with 'Total'"
  )
})

test_that("psrc_ctpp_stat warns when a full sum collapses totals with components", {
  df <- sample_ctpp_table()

  expect_warning(
    psrc_ctpp_sum(df, group_vars = NULL),
    "will sum a 'Total' row together with component rows"
  )
})

test_that("psrc_ctpp_median returns grouped medians and NA MOE", {
  df <- rbind(
    sample_ctpp_table(),
    transform(
      sample_ctpp_table(),
      estimate = c(12, 6, 8),
      estimate_moe = c(1.2, 0.6, 0.8)
    )
  )
  df$summary_geo <- "Regional Core"

  expect_no_warning(rs <- psrc_ctpp_median(df, group_vars = "summary_geo", incl_na = FALSE))

  lookup <- stats::setNames(rs$estimate, rs$category)
  expect_equal(unname(lookup[c("Total", "Bike/Ped", "Transit")]), c(11, 5, 7))
  expect_true(all(is.na(rs$estimate_moe)))
  expect_identical(
    colnames(rs),
    c("table_id", "summary_geo", "category", "estimate", "estimate_moe")
  )
})

test_that("ctpp_shares errors when required columns are missing", {
  expect_error(
    ctpp_shares(data.frame(category = "Total", estimate = 10)),
    "estimate_moe"
  )
})

test_that("ctpp_shares warns and returns input unchanged when totals are missing", {
  df <- sample_ctpp_table()
  df$category <- c("All Workers", "Bike/Ped", "Transit")

  expect_warning(
    rs <- ctpp_shares(df),
    "no category value beginning with 'Total'"
  )
  expect_identical(rs, df)
})

test_that("ctpp_shares warns and returns input unchanged when totals are duplicated", {
  df <- sample_ctpp_table()
  df$category[2] <- "Total, Bike/Ped"

  expect_warning(
    rs <- ctpp_shares(df),
    "multiple category values beginning with 'Total'"
  )
  expect_identical(rs, df)
})

test_that("ctpp_shares computes shares when exactly one total is present", {
  df <- sample_ctpp_table()

  expect_no_warning(rs <- ctpp_shares(df))
  expect_true(all(c("share", "share_moe") %in% colnames(rs)))
  expect_equal(rs$share[rs$category == "Total"], 1)
  expect_equal(rs$share[rs$category == "Bike/Ped"], 0.4)
  expect_equal(rs$share[rs$category == "Transit"], 0.6)
})

test_that("scale_code_lookup accepts variable vectors from one table", {
  expect_no_warning(
    rs <- scale_code_lookup("county", paste0("B202101_e", 6:8))
  )

  expect_equal(nrow(rs), 1)
  expect_equal(rs$scale_id, 23)
  expect_equal(rs$table_type, 2)
})

test_that("get_psrc_ctpp file fallback filters a table by requested variables", {
  root <- tempfile("ctpp")
  dir.create(file.path(root, "2017_2021"), recursive = TRUE)

  shell <- data.table::data.table(
    TBLID = "B202101",
    LINENO = 6:8,
    LDESC = c("60 to 64 years", "65 to 74 years", "75 years and over")
  )
  data.table::fwrite(
    shell,
    file.path(root, "2017_2021", "acs_ctpp_2017thru2021_table_shell.txt"),
    sep = "|"
  )

  geo <- data.table::data.table(
    GEOID = "53033",
    NAME = "King County, Washington"
  )
  data.table::fwrite(
    geo,
    file.path(root, "2017_2021", "acs_ctpp_2017thru2021_all_geo.txt"),
    sep = "|"
  )

  dt <- data.table::data.table(
    TBLID = rep("B202101", 4),
    LINENO = c(6L, 7L, 8L, 9L),
    GEOID = c("C23US53033", "C23US53033", "C23US53033", "C23US53033"),
    EST = c("10", "20", "30", "40"),
    MOE = c("1", "2", "3", "4"),
    SOURCE = "x"
  )
  data.table::fwrite(
    dt,
    file.path(root, "2017_2021", "WA_2017thru2021_B202101.csv")
  )

  rs <- get_psrc_ctpp(
    "county",
    paste0("B202101_e", 6:8),
    2021,
    filepath = paste0(root, "/")
  )

  expect_equal(rs$category, c("60 to 64 years", "65 to 74 years", "75 years and over"))
  expect_equal(rs$estimate, c(10, 20, 30))
  expect_equal(rs$work_geoid, rep("53033", 3))
  expect_true(all(is.na(rs$res_geoid)))
})