#' Sample complete groups from a data frame
#'
#' @param .data A data frame, possibly grouped
#' @param n Number of groups to sample
#' @param replace Should sampling be done with replacement?
#' @param weight_by Optional column to weight groups by (unquoted name)
#' @param ... Additional arguments passed to methods
#'
#' @return A data frame with sampled complete groups
#' @export
slice_block <- function(.data, ...) {
  UseMethod("slice_block")
}

#' @export
slice_block.grouped_df <- function(
  .data,
  n = 1,
  replace = FALSE,
  weight_by = NULL,
  ...
) {
  # Check if data is grouped
  groups <- dplyr::group_vars(.data)

  if (length(groups) == 0) {
    # No groups - return the entire data frame n times if replace = TRUE
    if (replace && n > 1) {
      return(dplyr::bind_rows(replicate(n, .data, simplify = FALSE)))
    } else {
      return(.data)
    }
  }

  # Get unique group keys
  group_keys <- dplyr::group_keys(.data)
  n_groups <- nrow(group_keys)

  # Sample group indices
  if (n > n_groups && !replace) {
    cli::cli_abort(c(
      "!" = "{.arg n} ({n}) is greater than the number of groups ({n_groups}).",
      "i" = "Set {.arg replace = TRUE} to sample with replacement."
    ))
  } else {
    # Handle weights if provided
    weight_by_expr <- rlang::enquo(weight_by)
    if (!rlang::quo_is_null(weight_by_expr)) {
      # Calculate weights per group
      weight_col <- rlang::eval_tidy(weight_by_expr, data = .data)
      weights <- .data |>
        dplyr::mutate(.weight = {{ weight_by }}) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
        dplyr::summarise(
          .weight_sum = sum(.weight, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::pull(.weight_sum)

      sampled_indices <- sample(
        seq_len(n_groups),
        size = n,
        replace = replace,
        prob = weights
      )
    } else {
      sampled_indices <- sample(seq_len(n_groups), size = n, replace = replace)
    }
  }

  # Get the sampled groups
  sampled_keys <- group_keys[sampled_indices, , drop = FALSE]

  # Filter data to keep only sampled groups
  result <- .data |>
    dplyr::semi_join(sampled_keys, by = groups) |>
    dplyr::ungroup()

  # If sampling with replacement, need to handle duplicates properly
  if (replace && any(duplicated(sampled_indices))) {
    # Build result by binding rows for each sampled index
    result_list <- lapply(sampled_indices, function(idx) {
      key <- group_keys[idx, , drop = FALSE]
      .data |>
        dplyr::semi_join(key, by = groups) |>
        dplyr::ungroup()
    })
    result <- dplyr::bind_rows(result_list)
  }

  result
}