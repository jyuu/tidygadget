#' @importFrom rlang syms sym is_string expr
tidyr_call <- function(data = NULL,
                       targets_fix = NULL,
                       targets_pivot = NULL,
                       settings = NULL) {
  if (is.null(data)) {
    return("")
  }

  data <- sym(data)

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
    settings <- syms(settings)
    call <- expr(
      pivot_wider(
        data = !!data,
        names_from = paste0(expr(!!settings$names_column)),
        values_from = paste0(expr(!!settings$values_column))
      )
    )
  }
  call
}
