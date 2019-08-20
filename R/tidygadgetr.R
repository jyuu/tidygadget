tidygadgetr <- function(data = NULL,
                      viewer = getOption(x = "esquisse.viewer", default = "dialog")) {


  res_data <- get_data(data, name = deparse(substitute(data)))

  if (!is.null(res_data$esquisse_data)) {
    res_data$esquisse_data <- dropListColumns(res_data$esquisse_data)
  }

  rv <- reactiveValues(
    data = res_data$esquisse_data,
    name = res_data$esquisse_data_name
  )

  if (viewer == "browser") {
    inviewer <- browserViewer(browser = getOption("browser"))
  } else if (viewer == "pane") {
    inviewer <- paneViewer(minHeight = "maximize")
  } else {
    inviewer <- dialogViewer(
      "Tidy gadget",
      width = 700, height = 750
    )
  }

  runGadget(
    app = tidygadgetUI(id = "tidygadget", container = NULL, insert_code = TRUE),
    server = function(input, output, session) {
      callModule(
        module = tidygadgetServer,
        id = "tidygadget",
        data = rv
      )
    },
    viewer = inviewer
  )
}
