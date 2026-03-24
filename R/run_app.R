#' Launch the wheather comparison Shiny app
#'
#' @param ... Arguments passed to [shiny::runApp()]
#' @export
run_app <- function(...) {
  app_dir <- system.file("shiny", package = "wheather")
  if (app_dir == "") {
    cli::cli_abort("Could not find Shiny app. Try reinstalling the {.pkg wheather} package.")
  }
  shiny::runApp(app_dir, ...)
}
