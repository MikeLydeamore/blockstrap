library(dplyr)

test_that("Number of blocks is conserved", {
  original_blocks <- ToothGrowth |>
    group_by(supp, dose) |>
    count(supp, dose)

  sampled_blocks <- ToothGrowth |>
    group_by(supp, dose) |>
    slice_block(n = 2)

  check_frame <- sampled_blocks |>
    count(supp, dose) |>
    left_join(original_blocks, by = c("supp", "dose"))

  expect_true(all(check_frame$n.x == check_frame$n.y))
})
