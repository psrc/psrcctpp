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