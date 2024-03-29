#' @importFrom shinyWidgets prettyRadioButtons
#' @importFrom htmltools tags tagList
#' @importFrom shiny NS textInput
pivotUI <- function(id) {
  ns <- NS(id)

  tagList(
    prettyRadioButtons(
      inputId = ns("pivot_type"),
      label = "Choose a type of pivot desired: ",
      choices = c("longer", "wider"),
      inline = TRUE,
      status = "info",
      fill = TRUE
    ),

    textInput(
      inputId = ns("names_col"),
      label = "Names to: ",
      width = "100%"
    ),

    textInput(
      inputId = ns("values_col"),
      label = "Values to: ",
      width = "100%"
    )
  )
}

#' @importFrom shiny reactiveValues observe updateTextInput
pivotServer <- function(input, output, session) {

  settings <- reactiveValues()

  observe({
    ns <- session$ns

    if (input$pivot_type == "longer") {
      names_call <- "Names to: "
      values_call <- "Values to: "
    } else {
      names_call <- "Names from: "
      values_call <- "Values from: "
    }

    updateTextInput(
      session = session,
      inputId = "names_col",
      label = names_call
    )

    updateTextInput(
      session = session,
      inputId = "values_col",
      label = values_call
    )

    settings$pivot_type <- input$pivot_type
    settings$names_column <- input$names_col
    settings$values_column <- input$values_col
  })

  return(settings)
}
