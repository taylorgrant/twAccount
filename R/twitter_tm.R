#' Get Twitter Timeline, Mentions and Topic Models
#'
#' Total convenience function to extract Twitter timeline of specified handle,
#' 18,000 tweets mentioning specific search query, and then uses biterm topic model
#' algorithm to model TM of both the timeline and mentions.
#'
#' This is the only function that needs to be run. Everything else works in the background.
#'
#' Relies heavily on `rtweet` `BTM` and `LDAvis` packages.
#'
#' @param handle Twitter handle without `@` sign
#' @param search_query boolean search query
#' @param topic_ct how many topics to model
#' @param term_ct how many top salient terms to return for each topic
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' twitter_tm("BMWUSA", search_query = "bmw", topic_ct = 40, term_ct = 30)
#' }
twitter_tm <- function(handle, search_query = NULL, topic_ct, term_ct) {

  # get twitter data and images#
  cat(crayon::green("Extracting relevant data from Twitter...\n"))
  get_twitter(handle, search_query)

  # build topic models
  if (is.null(search_query)) {
    cat(crayon::green('Short text topic modeling...\n'))
    run_btm(handle, "timeline", n_topics = 40, n_terms = 30)
  } else {
    cat(crayon::green('Short text topic modeling...\n'))
    run_btm(handle, "timeline", n_topics = 40, n_terms = 30)
    run_btm(handle, "mentions", n_topics = 40, n_terms = 30)
  }

}
