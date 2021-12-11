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
  cat(crayon::cyan("Expanding short links used on Twitter...\n"))

  # decode_url <- purrr::possibly(function(shortlink){httr::GET(shortlink)$url},
  #                        otherwise = NA)
  urls <- tbl %>%
    dplyr::select(urls_expanded_url) %>%
    tidyr::unnest(urls_expanded_url) %>%
    dplyr::filter(!is.na(urls_expanded_url)) %>%
    dplyr::filter(!grepl("pbs\\.twimg\\.com|twitter\\.com|t\\.co", urls_expanded_url)) %>%
    dplyr::group_by(url = urls_expanded_url) %>%
    dplyr::summarise(n = n())

  shorts <- urls %>%
    dplyr::filter(!stringr::str_detect(url, ".com")) %>%
    dplyr::pull(url)

  su1 <- shorts[1:round((length(shorts)/2))]
  su2 <- shorts[(round((length(shorts)/2))+1):length(shorts)]
  cores <- parallel::detectCores(logical = TRUE) - 2
  exp1 <- parallel::mclapply(su1, longurl::expand_urls, mc.cores = cores) %>%
    purrr::map(~ dplyr::select(., -status_code)) %>% purrr::reduce(dplyr::bind_rows)
  exp2 <- parallel::mclapply(su2, longurl::expand_urls, mc.cores = cores) %>%
    purrr::map(~ dplyr::select(., -status_code)) %>% purrr::reduce(dplyr::bind_rows)
  exp <- bind_rows(exp1,exp2)

  links <- urls %>%
    dplyr::left_join(exp, by = c("url" = "orig_url")) %>%
    dplyr::mutate(expanded_url = dplyr::case_when(stringr::str_detect(url, ".com") ~ url,
                                                  TRUE ~ expanded_url))
}
