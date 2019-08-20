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
      value = "",
      width = "100%"
    ),

    textInput(
      inputId = ns("values_col"),
      label = "Values to: ",
      value = "",
      width = "100%"
    )
  )
}

pivotServer <- function(input, output, session) {

  observeEvent(input$pivot_type, {
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
  })

  settings <- reactiveValues({
    type = input$pivot_type
    names_column = input$names_col
    values_column = input$values_col
  })

  return(settings)
}
