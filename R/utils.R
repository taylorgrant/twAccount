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

#' Decode short links
#'
#' Simple convenience function to check for folder and create if not found.
#'
#' @param tbl timeline extracted with `rtweet`
#'
#' @return tibble of urls
#' @export
#'
#' @examples
#' \dontrun{
#' decode_links(tbl)
#' }
decode_links <- function(tbl) {
  # function
  cat(crayon::cyan("Decoding links used on Twitter...\n"))

  decode_url <- purrr::possibly(function(shortlink){httr::GET(shortlink)$url},
                         otherwise = NA)
  urls <- tbl %>%
    dplyr::select(urls_expanded_url) %>%
    tidyr::unnest(urls_expanded_url) %>%
    dplyr::filter(!is.na(urls_expanded_url)) %>%
    dplyr::filter(!grepl("pbs\\.twimg\\.com|twitter\\.com|t\\.co", urls_expanded_url))

  shorts <- urls %>%
    dplyr::filter(!stringr::str_detect(urls_expanded_url, ".com")) %>%
    dplyr::pull()
  longs <- urls %>%
    dplyr::filter(stringr::str_detect(urls_expanded_url, ".com"))

  su1 <- shorts[1:round((length(shorts)/2))]
  su2 <- shorts[(round((length(shorts)/2))+1):length(shorts)]
  cores <- parallel::detectCores(logical = TRUE) - 2
  decode1 <- parallel::mclapply(su1, decode_url, mc.cores = cores)
  decode2 <- parallel::mclapply(su2, decode_url, mc.cores = cores)

  links <- tibble::tibble(urls = unlist(c(longs, decode1, decode2)))
}
