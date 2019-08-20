#' Tidygadget Shiny module.
#'
#' @param id Module id.
#' @param container Defaults to using the `esquisse` container as the
#'   addin display.
#' @param insert_code Logical. Displays a button to insert `tidyr` code
#'   in the current user script. Defaults to `TRUE`.
#'
#' @return Reactive values with 2 slots containing the code to generate
#'   the pivot table result and the resulting dataframe.
#'
#' @export
#'
#' @importFrom htmltools tags tagList singleton
#' @importFrom shiny icon actionButton NS textOutput
#' @importFrom miniUI miniPage miniTabstripPanel miniTabPanel miniContentPanel
#' @importFrom shinyWidgets prettyToggle
tidygadgetUI <- function(id,
                        container = esquisseContainer(),
                        insert_code = FALSE) {
  ns <- NS(id)

  box_title <- tags$div(
    class="gadget-title dreamrs-title-box",
    tags$h1("Pivot helper", class = "dreamrs-title"),
    tags$div(
      class = "pull-right",
      miniTitleBarButton(inputId = ns("close"), label = "Close")
    ),

      tags$div(
        class = "pull-left",
        chooseDataUI(id = ns("choose-data"), class = "btn-primary")
      )

  )

  addin <- miniPage(

    # style sheet
    singleton(x = tagList(
      tags$link(rel="stylesheet", type="text/css", href="esquisse/styles.css"),
      tags$script(src = "esquisse/clipboard/clipboard.min.js")
    )),

    # header
    box_title,

    miniTabstripPanel(

      # pivot settings
      miniTabPanel("Pivot Settings", icon = icon("arrows"),
        miniContentPanel(
          dragulaInput(
            inputId = ns("dragvars"),
            sourceLabel = "Variables",
            targetsLabels = c("Fixed Variables", "Pivot Variables"),
            targetsIds = c("fvar", "pvar"),
            choices = "",
            badge = FALSE,
            width = "100%",
            height = "50%",
            replace = FALSE
         ),
         pivotUI(ns("pivot_test"))
       )
      ),

      # viewer
      miniTabPanel("View", icon = icon("eye"),
        miniContentPanel(
          DT::dataTableOutput(ns("viewer"))
        )
      ),

      # export code
      miniTabPanel("Export", icon = icon("laptop-code"),
        miniContentPanel(
          # nothing yet
        )
      )
    ) # end of miniTabstripPanel
  )

  if (is.function(container)) {
    addin <- container(addin)
  }

  return(addin)
}
