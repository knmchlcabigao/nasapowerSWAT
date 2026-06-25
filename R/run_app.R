#' Launch the NASA POWER Daily Data Downloader for SWAT
#'
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "nasapowerSWAT")
  if (app_dir == "") {
    stop("Could not find app directory. Try reinstalling the package.")
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
