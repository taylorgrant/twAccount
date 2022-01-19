#' Clean Twitter timeline and get images
#'
#' Take a tibble from `rtweet::get_timeline()`, extract urls, order, and
#' read in images
#'
#' @param dir directory to save files into (predetermined)
#' @param tbl twitter timeline in tibble format
#'
#' @return tibble with image data
#' @export
#'
#' @examples
#' \dontrun{
#' pics <- get_pics(d2, tl)
#' }
get_pics <- function(dir, tbl) {

  # test urls (this is slow)
  valid_url <- function(url_in,t=2){
    con <- url(url_in)
    check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
    suppressWarnings(try(close.connection(con),silent=T))
    ifelse(is.null(check),TRUE,FALSE)
  }

  cat(crayon::blue(paste0("\nChecking urls first...\n")))
  tmp <- tbl %>%
    dplyr::filter(!is.na(media_url)) %>%
    dplyr::select(status_id, ext_media_url, created_at) %>%
    dplyr::mutate(year = lubridate::year(created_at)) %>%
    dplyr::arrange(year, created_at) %>%
    tidyr::unnest(ext_media_url) %>%
    dplyr::mutate(exist = sapply(ext_media_url, valid_url)) %>%
    dplyr::filter(exist == "TRUE") %>%
    dplyr::group_by(status_id, year) %>%
    summarise(ext_media_url = list(ext_media_url)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(index = dplyr::row_number(),
                  index = stringr::str_pad(index, 2, pad = "0"),
                  img_ct = lengths(ext_media_url))

  # create folders for everything
  folder_names <- c( "local_cols", "final_cols", unique(tmp$year))
  purrr::map(file.path(dir, folder_names), dir_create)

  purrr::pwalk(list(tmp$year, tmp$index, tmp$img_ct, tmp$ext_media_url), extract_images)
  return(tmp)
}
