#' Cleaning Tweets for Topic Modeling
#'
#' Simple convenience function that cleans tweets. Removes urls, mentions,
#' and punctuation.
#'
#' @param tbl tibble or data.frame
#'
#' @return tibble with 2 columns - status id and cleaned text
#' @export
#' @importFrom graphics text
#' @examples
#' \dontrun{
#' out <- tm_clean_tweets(tbl)
#' }
tm_clean_tweets <- function(tbl) {
  tbl %>%
    dplyr::filter(is.na(reply_to_screen_name) & is_retweet == "FALSE") %>%
    dplyr::select(status_id, text) %>%
    dplyr::mutate(text = tolower(text),
                  text = qdapRegex::rm_url(text),
                  text = gsub('@\\S+', '', text), ## Remove Mentions
                  text = stringr::str_trim(gsub('[[:punct:]]', '', text)))
}
