
#' Title Shiny App for PLM2024 methods
#'
#' @return Shiny app window
#' @export

plm_shiny <- function(...) {
  appDir <- system.file("19_myapp",
                        package = "plm2024")


  if (appDir == "") {
    stop("Could not find example directory.
         Try re-installing `plm2024`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

