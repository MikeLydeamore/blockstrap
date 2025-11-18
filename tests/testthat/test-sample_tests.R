library(dplyr)
library(palmerpenguins)

test_that("Number of blocks is conserved", {
  original_blocks <- penguins |>
    group_by(species, island) |>
    count(species, island)

  sampled_2 <- penguins |>
    group_by(species, island) |>
    slice_block(n = 2)

  check_frame <- sampled_2 |>
    count(species, island) |>
    left_join(original_blocks, by = c("species", "island"))

  expect_true(all(check_frame$n.x == check_frame$n.y))
})
