
#' @importFrom htmltools tagList tags HTML
#' @importFrom shiny NS icon
#' @importFrom shinyWidgets pickerInput
selectVarsUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyUtils(),
    tags$script(
      sprintf("Shiny.onInputChange('%s', %f);", ns("launchSelectVars"), as.numeric(Sys.time()))
    ),
    tags$style(HTML(paste(
      paste0("#", ns("col_chooser-container")),
      ".bootstrap-select .dropdown-menu li a span.text {width: 96%;}"
    ))),
    tags$div(
      id = ns("col_chooser-container"),
      tags$label(
        class = "control-label",
        style = "width: 100%;",
        "Select variables to keep:"
      ),
      pickerInput(
        inputId = ns("col_chooser"),
        label = NULL,
        choices = "No data", multiple = TRUE, width = "100%",
        selected = NULL,
        options = list(
          `actions-box` = TRUE, `multiple-separator` = " ",
          `selected-text-format`= "count > 0",
          `count-selected-text` = "{0} variables chosen (from a total of {1})"
        )
      )
    ),
    tags$em("Legend :"),
    HTML(paste(
      doRenderTags(
        badgeType(col_name = c("discrete", "continuous", "time", "id"),
                  col_type = c("discrete", "continuous", "time", "id"))
      ),
      collapse = ", "
    ))
  )
}


#' @importFrom htmltools doRenderTags
#' @importFrom shiny reactiveValuesToList observeEvent reactiveValues
#' @importFrom shinyWidgets updatePickerInput
selectVarsServer <- function(input, output, session, data = list()) {

  ns <- session$ns

  observeEvent(input$launchSelectVars, {
    toggleInput(inputId = ns("col_chooser"), enable = FALSE)
  })

  observeEvent(reactiveValuesToList(data), {
    if (!is.null(data$data) && is.data.frame(data$data)) {
      toggleInput(inputId = ns("col_chooser"), enable = TRUE)
    } else {
      toggleInput(inputId = ns("col_chooser"), enable = FALSE)
    }
  }, ignoreNULL = FALSE)

  observeEvent(reactiveValuesToList(data), {
    if (!is.null(data$data) && is.data.frame(data$data)) {
      res_col_type <- unlist(lapply(data$data, col_type))
      updatePickerInput(
        session = session,
        inputId = "col_chooser",
        choices =  names(res_col_type),
        selected = names(res_col_type)[unname(res_col_type) != "id"],
        choicesOpt = list(
          # content = unlist(lapply(
          #   X = badgeType(col_name = names(res_col_type), col_type = unname(res_col_type)),
          #   FUN = doRenderTags
          # ))
          content = paste0(
            unlist(lapply(
              X = badgeType(col_name = names(res_col_type), col_type = unname(res_col_type)),
              FUN = doRenderTags
            )),
            "<span style='float: right; margin-right: 15px; white-space: pre;'>",
            "Class: ",
            "<span style='display: inline-block; width: 65px; text-align: right;'>",
            unlist(lapply(data$data, function(x) class(x)[1]), use.names = FALSE),
            "</span>",
            " |   ",
            "Missing values: ",
            "<span style='display: inline-block; width: 30px; text-align: right;'>",
            unlist(lapply(data$data, function(x) sum(is.na(x))), use.names = FALSE),
            "</span>",
            "</span>"
          )
        )
      )
    }
  }, ignoreNULL = FALSE)

  res <- reactiveValues(selected_vars = NULL)

  observeEvent(input$col_chooser, {
    res$selected_vars <- input$col_chooser
  }, ignoreNULL = FALSE)

  return(res)
}

