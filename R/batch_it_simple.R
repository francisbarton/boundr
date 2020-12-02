batch_it_simple <- function(x, batch_size = 100) {
  if (!rlang::is_interactive()) {
    options(usethis.quiet = TRUE)
  }

  # ensure x is a reasonable vector
  if (is.list(x)) {
    usethis::ui_info("Converting list to single vector")
    x <- do.call("c", x)
  }

  if (!is.vector(x)) {
    usethis::ui_stop("This function only works with lists or vectors")
  }

  if (length(x) > 10e6) {
    usethis::ui_nope("Easy, tiger! That vector has more than a million items.
            Are you sure you want to continue?")
  }

  # ensure batch_size is an appropriate single positive number
  if (!length(batch_size) == 1 || !batch_size > 0) {
    usethis::ui_stop("The batch_size parameter must be a single positive value")
  }

  if ((batch_size > 0) && (batch_size < 1)) {
    batch_size <- ceiling(length(x) * batch_size)
  }

  if (batch_size > length(x)) {
    # usethis::ui_info("Batch size provided was greater than the length of the vector.")
    batch_size <- length(x)
  }

  # stolen from `?integer`
  is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol

  # ensure batch size is a single whole positive integer
  assertthat::assert_that(is.wholenumber(batch_size))
  assertthat::assert_that(batch_size > 0)

  # do the batching by creating a vector of factors of length(x)
  # then use this as the factor argument to split(x)
  rep(1:ceiling(length(x) / batch_size), each = batch_size) %>%
    utils::head(length(x)) %>%
    split(x = x, f = .)
}
