tidygadgetUI <- function(id,
                        container = esquisseContainer(),
                        choose_data = TRUE,
                        insert_code = FALSE) {
  ns <- NS(id)

  box_title <- tags$div(
    class="gadget-title dreamrs-title-box",
    tags$h1("Pivot helper", class = "dreamrs-title"),
    tags$div(
      class = "pull-right",
      miniTitleBarButton(inputId = ns("close"), label = "Close")
    ),
    if (isTRUE(choose_data)) {
      tags$div(
        class = "pull-left",
        chooseDataUI(id = ns("choose-data"), class = "btn-primary")
      )
    }
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
            targetsLabels = c("X", "Y"),
            targetsIds = c("xvar", "yvar"),
            choices = "",
            badge = FALSE,
            width = "100%",
            height = "50%",
            replace = TRUE
         )
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
          textOutput("code")
        )
      )
    ) # end of miniTabstripPanel
  )

  if (is.function(container)) {
    addin <- container(addin)
  }

  return(addin)
}
