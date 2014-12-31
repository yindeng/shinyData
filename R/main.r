#' Run shinyData
#'
#' This will open your default browser and run shinyData locally on your computer.
#'
#' @export
shinyData <- function() {
  shiny::runApp(file.path(system.file("shinyDataApp", package = "shinyData")))
}

# #' Print hello
# #'
# #' This function prints hello
# #'
# #' @param fname First name
# #' @param lname Last name
# #' @export
# #' @examples
# #' hello(fname="Tomas",lname="Greif")
# #' hello(fname="Your",lname="Name")
#
# hello <- function(fname, lname) {
#   cat(paste("Hello",fname,lname,"!"))
# }
