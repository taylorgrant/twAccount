#' Run Biterm Topic Models
#'
#' Uses the `BTM` package to run noun-based topic models in collections of short texts.
#' Uses `udpipe` to tokenize and keep only nouns, proper nouns. The BTM output is then processed using
#' functions and calculations from the `LDAvis` package.
#'
#' The json file is included in the output if you want to visualize with `LDAvis`
#' or plots can be made with the x_phi output.
#'
#' @param handle twitter handle without the `@` sign
#' @param data_source either timeline or mentions
#' @param n_topics how many topics to model
#' @param n_terms how many of the most salient terms to include for each topic
#'
#' @return .rds file - x_phi is scaled x,y coordinates for topics, term_info is the top n terms;
#' both are based on lambda - 0.6; json is the json object from `LDAvis` package
#' @export
#' @importFrom stats predict
#' @examples
#' \dontrun{
#' run_btm("BMWUSA", data_source = 'timeline', n_topics = 40, n_terms = 30)
#' }
run_btm <- function(handle, data_source = c("timeline", "mentions"), n_topics, n_terms) {

  # load udpipe model for POS tagging
  aa <- file.path(here::here(), "account_analysis")
  ud_model <- udpipe::udpipe_download_model(language = "english",
                                            model_dir = aa)
  f <- file.path(d2, glue::glue("{handle}_twitter_info.rds"))

  if (data_source == "timeline") {
    tl <- readRDS(f)$timeline
    try(if(is.null(tl)) stop("Timeline is missing..."))
    mm <- tm_clean_tweets(tl)
  } else {
    mentions <- readRDS(f)$mentions
    try(if(is.null(mentions)) stop("Mentions are missing..."))
    mm <- tm_clean_tweets(mentions)
  }
  cat(crayon::red(paste0("Tokenizing the ",data_source,"...\n")))
  pos_tagged <- udpipe::udpipe_annotate(ud_model, x = mm$text,
                                        doc_id = mm$status_id)
  pos_tagged <- as.data.frame(pos_tagged)
  # subsetting to parts of speech (noun model works best)
  x <- subset(pos_tagged, xpos %in% c("NN", "NNP", "NNS"))
  # running the BTM model (hard coded at 40 topics right now)
  k <- n_topics

  cat(crayon::red(paste0("Identifying ",k," topics within the ",data_source,"...\n")))
  tm1 <- BTM::BTM(x, k = k, beta = 0.01, iter = 1000, trace = 100, detailed = TRUE)

  # calculated and extract parameters
  phi <- t(tm1$phi)
  docsize <- table(x$doc_id)
  scores  <- stats::predict(tm1, x)
  scores  <- scores[names(docsize), ]
  theta <- scores
  doc.length <- as.integer(docsize)
  vocab <- tm1$vocabulary$token
  term.frequency = tm1$vocabulary$freq
  ## JSON file for LDAvis (d3 and JS)
  json <- createJSON(
    phi = t(tm1$phi),
    theta = scores,
    doc.length = as.integer(docsize),
    vocab = tm1$vocabulary$token,
    term.frequency = tm1$vocabulary$freq)
  cat(crayon::red("Extracting PCA coords and top terms...\n"))
  # frequency to reorder topics
  topic.frequency <- colSums(theta * doc.length)
  topic.proportion <- topic.frequency/sum(topic.frequency)
  # re-order the K topics in order of decreasing proportion:
  o <- order(topic.proportion, decreasing = TRUE)
  phi <- phi[o, ]
  theta <- theta[, o]
  topic.frequency <- topic.frequency[o]
  topic.proportion <- topic.proportion[o]
  # get the topic coordinates using
  x_phi <- jsPCA(phi)
  x_phi <- x_phi %>% dplyr::mutate(id = dplyr::row_number())
  # add topic proportion for sizing of each
  x_phi <- x_phi %>%
    dplyr::mutate(frac = topic.proportion,
                  Topic = paste0("Topic ", id))

  # gathering top terms
  # token counts for each term-topic combination (widths of red bars)
  term.topic.frequency <- phi * topic.frequency
  term.frequency <- colSums(term.topic.frequency)
  # stopifnot(all(term.frequency > 0))
  # marginal distribution over terms (width of blue bars)
  term.proportion <- term.frequency/sum(term.frequency)
  phi <- t(phi)
  topic.given.term <- phi/rowSums(phi)  # (W x K)
  kernel <- topic.given.term * log(sweep(topic.given.term, MARGIN=2,
                                         topic.proportion, `/`))
  distinctiveness <- rowSums(kernel)
  saliency <- term.proportion * distinctiveness
  # Order the terms for the "default" view by decreasing saliency:
  R <- n_terms
  K <- n_topics # argument passed through
  Rs <- rev(seq_len(R))
  topic_seq <- rep(seq_len(K), each = R)
  category <- paste0("Topic ", topic_seq)
  lift <- phi/term.proportion
  find_relevance <- function(i) {
    relevance <- i*log(phi) + (1 - i)*log(lift)
    idx <- apply(relevance, 2,
                 function(x) order(x, decreasing = TRUE)[seq_len(R)])
    # for matrices, we pick out elements by their row/column index
    indices <- cbind(c(idx), topic_seq)
    data.frame(Term = vocab[idx], Topic = category,
               logprob = round(log(phi[indices]), 4),
               loglift = round(log(lift[indices]), 4),
               stringsAsFactors = FALSE)
  }
  lambda.step <- .1
  lambda.seq <- seq(0, 1, by=lambda.step)
  # tinfo <- lapply(as.list(lambda.seq), find_relevance) # use this if stepping it out
  tinfo <- lapply(0.6, find_relevance) # note that we're hard coding this
  tinfo <- unique(do.call("rbind", tinfo))
  tinfo$Total <- term.frequency[match(tinfo$Term, vocab)]
  rownames(term.topic.frequency) <- paste0("Topic ", seq_len(K))
  colnames(term.topic.frequency) <- vocab
  tinfo$Freq <- term.topic.frequency[as.matrix(tinfo[c("Topic", "Term")])]

  saveRDS(
    object = list(
      x_phi = x_phi,
      term_info = tinfo,
      json = json
    ),
    file = file.path(d2, glue::glue("{handle}_tm_{data_source}.rds"))
  )
}
