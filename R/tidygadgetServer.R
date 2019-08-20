tidygadgetServer <- function(input, output, session, data = NULL, dataModule = c("GlobalEnv", "ImportFile"), sizeDataModule = "m") {

  ggplotCall <- reactiveValues(code = "")

  observeEvent(data$data, {
    dataChart$data <- data$data
    dataChart$name <- data$name
  }, ignoreInit = FALSE)

  dataChart <- callModule(
    module = chooseDataServer,
    id = "choose-data",
    data = isolate(data$data),
    name = isolate(data$name),
    launchOnStart = is.null(isolate(data$data)),
    dataModule = dataModule, size = sizeDataModule
  )
  observeEvent(dataChart$data, {
    # special case: geom_sf
    if (inherits(dataChart$data, what = "sf")) {
      geom_possible$x <- c("sf", geom_possible$x)
    }
    var_choices <- setdiff(names(dataChart$data), attr(dataChart$data, "sf_column"))
    updateDragulaInput(
      session = session,
      inputId = "dragvars", status = NULL,
      choiceValues = var_choices,
      choiceNames = badgeType(
        col_name = var_choices,
        col_type = col_type(dataChart$data[, var_choices])
      ),
      badge = FALSE
    )
  })

  # geom_possible <- reactiveValues(x = "auto")
  # geom_controls <- reactiveValues(x = "auto")
  # observeEvent(list(input$dragvars$target, input$geom), {
  #   geoms <- potential_geoms(
  #     data = dataChart$data,
  #     mapping = build_aes(
  #       data = dataChart$data,
  #       x = input$dragvars$target$xvar,
  #       y = input$dragvars$target$yvar
  #     )
  #   )
  #   geom_possible$x <- c("auto", geoms)
  #
  #   geom_controls$x <- select_geom_controls(input$geom, geoms)
  #
  #   if (!is.null(input$dragvars$target$fill) | !is.null(input$dragvars$target$color)) {
  #     geom_controls$palette <- TRUE
  #   } else {
  #     geom_controls$palette <- FALSE
  #   }
  # })

  observeEvent(input$close, shiny::stopApp())

  output_module <- reactiveValues(code_plot = NULL, code_filters = NULL, data = NULL)

  return(output_module)
}
