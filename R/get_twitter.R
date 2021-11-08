#' Get Twitter Timeline
#'
#' Extract 3,200 tweets from a timeline, extract all pictures and save timeline
#' asa .rds file to new folder. Note that this creates a new directory in the current
#' working directory. "~/working_directory/account_analysis/handle/Sys.Date"
#'
#' If search_query is added, also pulls 18,000 recent tweets matching the query.
#' Mentions are also saved into the .rds file.
#'
#' Heavily reliant on the `rtweet` package. Read the package tutorial for how to
#' set up Twitter API authorization.
#'
#' @param handle twitter handle without `@`
#' @param search_query boolean search query (read `rtweet::search_tweets()` docs for proper formatting)
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' get_twitter("BMWUSA", search_query = "bmw")
#' }
get_twitter <- function(handle, search_query = NULL) {

  # hard coding directory but building out proper folder structure
  aa <- file.path(here::here(), "account_analysis")
  d <- file.path(here::here(), "account_analysis", fs::path_sanitize(handle))
  d2 <<- file.path(d, as.character(Sys.Date())) # put this into the environment
  dir_create <- function(x) ifelse(!dir.exists(x), dir.create(x), FALSE)
  dir_create(aa)
  dir_create(d)
  dir_create(d2)

  # user info
  user <- rtweet::lookup_users(handle)

  # handle timeline
  tl <- rtweet::get_timeline(
    handle,
    n = 3200,
    check=FALSE)
  attr(tl$created_at, 'tzone') = 'PST8PDT'
  tl <- dplyr::mutate(tl, tsday = as.Date(created_at))
  tl <- dplyr::mutate(tl, dow = lubridate::wday(created_at, label=TRUE, abbr=FALSE))
  tl <- dplyr::mutate(tl, hr = lubridate::hour(created_at))

  # picture collage from handle timeline
  cat(crayon::blue(paste0("Saving pictures from ", handle,"'s timeline...\n")))
  pics <- get_pics(d2, tl)
  cat(crayon::blue("Making some collages...\n"))
  as.character(unique(pics$year)) %>%
    purrr::walk(make_collage)

  # decoding short links
  cat(crayon::blue("Decoding short links...\n"))
  links <- decode_links(tl)
  ## save if no mention search
  if (is.null(search_query)) {
    cat(crayon::blue("Putting things away...\n"))
    saveRDS(
      object = list(
        user = user,
        timeline = tl,
        links = links
      ),
      file = file.path(d2, paste0(handle,"_twitter_info.rds"))
    )

  } else
    # grab mentions via twitter search
    cat(crayon::blue(paste0("Searching for tweets mentioning '",search_query,"'\n")))
  mentions <- rtweet::search_tweets(q = search_query, n = 18000, type = "recent",
                                    include_rts=FALSE, lang = "en")

  # save including mentions
  saveRDS(
    object = list(
      user = user,
      timeline = tl,
      mentions = mentions,
      links = links
    ),
    file = file.path(d2, paste0(handle,"_twitter_info.rds"))
  )
}
