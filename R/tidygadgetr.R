#' Addin to aid with visualizing the result of pivoting with `tidyr`.
#'
#' @param data A `data.frame` passed by the user, or one selected from
#'   the global environment.
#'
#' @return Code to reproduce the pivoted dataframe.
#'
#' @export
#'
#' @importFrom shiny dialogViewer runGadget reactiveValues
tidygadgetr <- function(data = NULL) {

  res_data <- get_data(data, name = deparse(substitute(data)))

  rv <- reactiveValues(
    data = res_data$esquisse_data,
    name = res_data$esquisse_data_name
  )

  runGadget(
    app = tidygadgetUI(id = "tidygadget", container = NULL, insert_code = TRUE),
    server = function(input, output, session) {
      callModule(
        module = tidygadgetServer,
        id = "tidygadget",
        data = rv
      )
    },
    viewer = dialogViewer("Tidy gadget", width = 700, height = 800)
  )
}
