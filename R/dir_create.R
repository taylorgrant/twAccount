#' Create new directory
#'
#' Simple convenience function to check for folder and create if not found.
#'
#' @param x file path
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' dir_create("~/Desktop/New_Folder")
#' }
dir_create <- function(x) {
  ifelse(!dir.exists(x), dir.create(x), FALSE)
}
