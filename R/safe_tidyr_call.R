
# safe_tidyr_call <- function(expr,
#                             data = NULL,
#                             session = shiny::getDefaultReactiveDomain()) {
#
#   show_condition_message <- function(e, type, session) {
#     if (!is.null(session)) {
#       shiny::showNotification(
#         ui = paste(
#           tools::toTitleCase(type),
#           conditionMessage(e),
#           sep = " : "
#         ),
#         type = type,
#         session = session
#       )
#     }
#   }
#
#   withCallingHandlers(
#     expr = tryCatch(
#       expr = {
#         call <- rlang::eval_tidy(expr = expr, data = data)
#         call
#       },
#       error = function(e) {
#         show_condition_message(e, "error", session)
#         list(call = NULL, data = NULL)
#       }
#     ),
#     warning = function(w) {
#       show_condition_message(w, "warning", session)
#       list(call = NULL, data = NULL)
#     }
#   )
# }

safe_tidyr_call <- function(expr, data = NULL) {
  if(!is.null(data)) {
    output <- rlang::eval_tidy(expr = expr, data = data)
  } else {
    output <- mtcars
  }
  return(output)
}
