#' @importFrom rlang syms sym is_string
tidyr_call <- function(data = NULL,
                       targets_fix = NULL,
                       targets_pivot = NULL,
                       settings = NULL) {
  if (is.null(data)) {
    return("")
  }


  targets_fix <- dropNulls(targets_fix)
  targets_pivot <- dropNulls(targets_pivot)

  cols <- expr(-c(!!!syms(targets_fix)))
  if (settings$pivot_type == "longer") {
    settings <- syms(settings)
    call <- expr(
      pivot_longer(
        data = !!data,
        cols = !!cols,
        names_to = paste0(expr(!!settings$names_column)),
        values_to = paste0(expr(!!settings$values_column))
      )
    )
  } else {
    call <- "hello!"
  }

  call
}
