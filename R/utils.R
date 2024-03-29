

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

`%empty%` <- function(a, b) {
  if (a != "") a else b
}

`%|e|%` <- function(a, b) {
  if (!is.null(a) && a != "") a else b
}

`%nin%` <- Negate(`%in%`)





# utilities borrowed from shiny
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}
nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0 || x == ""
}
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE = logical(1))]
}


# quickly clean a string
#' @importFrom stringi stri_trans_general stri_replace_all_regex
clean_string <- function(str) {
  str <- stri_trans_general(str = str, id = "Latin-ASCII")
  str <- stri_replace_all_regex(str = str, pattern = "[^a-zA-Z0-9_]+", replacement = "_")
  return(str)
}



#' Retrieve a data.frame by name from an environment
#'
#' @param df character, name of the object
#' @param env an environment
#'
#' @return the object
#' @noRd
#'
#' @importFrom utils data
#'
get_df <- function(df, env = globalenv()) {
  if (df %in% ls(name = env)) {
    get(x = df, envir = env)
  } else if (df %in% data(package = "tidyr", envir = environment())$results[, "Item"]) {
    get(utils::data(list = df, package = "tidyr", envir = environment()))
  } else {
    NULL
  }
}



#' Search for object with specific class in an environment
#'
#' @param what a class to look for
#' @param env An environment
#'
#' @return Character vector of the names of objects, NULL if none
#' @noRd
#'
#' @examples
#'
#' # NULL if no data.frame
#' search_obj("data.frame")
#'
#' library(ggplot2)
#' data("mpg")
#' search_obj("data.frame")
#'
#'
#' gg <- ggplot()
#' search_obj("ggplot")
#'
search_obj <- function(what = "data.frame", env = globalenv()) {
  all <- ls(name = env)
  objs <- lapply(
    X = all,
    FUN = function(x) {
      if (inherits(get(x, envir = env), what = what)) {
        x
      } else {
        NULL
      }
    }
  )
  objs <- unlist(objs)
  if (length(objs) == 1 && objs == "") {
    NULL
  } else {
    objs
  }
}


#' Create badge according to data type
#'
#' It uses conventions defined in the package, variable type are retrieve with \code{\link{col_type}}.
#'
#' @param col_name Variable's name
#' @param col_type Variable's type : 'discrete', 'time', 'continuous', 'id'
#'
#' @noRd
badgeType <- function(col_name, col_type) {
  stopifnot(length(col_name) == length(col_type))
  res <- lapply(
    X = seq_along(col_name),
    FUN = function(i) {
      col_name_i <- col_name[i]
      col_type_i <- col_type[i]
      if (col_type_i == "discrete") {
        tags$span(class='label label-discrete badge-dad', col_name_i)
      } else if (col_type_i == "time") {
        tags$span(class='label label-datetime badge-dad', col_name_i)
      } else if (col_type_i == "continuous") {
        tags$span(class='label label-continue badge-dad', col_name_i)
      } else if (col_type_i == "id") {
        tags$span(class='label label-default badge-dad', col_name_i)
      }
    }
  )
  res
}





#' Try to guess type of a vector
#'
#' @param x a vector
#'
#' @noRd
col_type <- function(x, no_id = FALSE) {
  if (is.null(x))
    return(NULL)

  if (is.data.frame(x) && inherits(x, what = "sf")) {
    x <- x[, setdiff(names(x), attr(x, "sf_column")), drop = FALSE]
  }

  if (is.data.frame(x)) {
    return(unlist(lapply(x, col_type), use.names = FALSE))
  } else {
    if (inherits(x, c("logical", "character", "factor", "AsIs"))) {
      n <- length(x)
      u <- length(unique(x))
      if (u/n < 0.99 | u <= 30 | no_id) {
        return("discrete")
      } else {
        return("id")
      }
    }

    if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
      return("time")
    }

    if (inherits(x, c("numeric", "integer", "double"))) {
      return("continuous")
    }
  }

  NULL
}



# utils for geom icons
geomIcons <- function() {
  geoms <- c(
    "auto", "line", "area", "bar", "histogram",
    "point", "boxplot", "violin", "density",
    "tile", "sf"
  )
  href <- "esquisse/geomIcon/gg-%s.png"
  geomsChoices <- lapply(
    X = geoms,
    FUN = function(x) {
      list(inputId = x, img = sprintf(fmt = href, x), label = capitalize(x))
    }
  )

  geomsChoicesNames <- lapply(
    X = geomsChoices,
    FUN = function(x) {
      list(
        style = "width: 90px;",
        tags$img(src = x$img, width = 56, height = 56),
        tags$br(), x$label
      )
    }
  )
  geomsChoicesValues <- unlist(lapply(geomsChoices, `[[`, "label"), use.names = FALSE)
  geomsChoicesValues <- tolower(geomsChoicesValues)

  list(names = geomsChoicesNames, values = geomsChoicesValues)
}


capitalize <- function(x) {
  lo <- substring(text = x, first = 2)
  up <- substring(text = x, first = 1, last = 1)
  lo <- tolower(lo)
  up <- toupper(up)
  lo <- gsub(pattern = "_", replacement = " ", x = lo)
  paste0(up, lo)
}





# colors ------------------------------------------------------------------

#' Convert a color in character into hex format
#'
#' @param col name of a color, e.g. 'steelblue'
#'
#' @return a hex code
#' @noRd
#'
#' @importFrom grDevices rgb col2rgb
#'
col2Hex <- function(col) {
  mat <- grDevices::col2rgb(col, alpha = TRUE)
  grDevices::rgb(mat[1, ]/255, mat[2, ]/255, mat[3,]/255)
}


get_brewer_pal <- function(name) {
  bpi <- RColorBrewer::brewer.pal.info
  maxn <- bpi[rownames(bpi) %in% name, ]
  maxn <- maxn$maxcolors
  brewer.pal(n = maxn, name = name)
}


linear_gradient <- function(cols) {
  x <- round(seq(from = 0, to = 100, length.out = length(cols)+1))
  ind <- c(1, rep(seq_along(x)[-c(1, length(x))], each = 2), length(x))
  m <- matrix(data = paste0(x[ind], "%"), ncol = 2, byrow = TRUE)
  res <- lapply(
    X = seq_len(nrow(m)),
    FUN = function(i) {
      paste(paste(cols[i], m[i, 1]), paste(cols[i], m[i, 2]), sep = ", ")
    }
  )
  res <- unlist(res)
  res <- paste(res, collapse = ", ")
  paste0("linear-gradient(to right, ", res, ");")
}









