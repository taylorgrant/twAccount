#' Make a collage of images from Twitter timeline
#'
#' Create a 12xN size collage. Takes photos and appends them into
#' column, then stitches columns together into collage. Collages are split out
#' by the year images were posted. To ensure proper sizing, blank cells are
#' created to fill out any NA or missing images.
#'
#' @param year vector of years to compile
#'
#' @return
#' @export
#' @importFrom stats lag
#'
#' @examples
#' \dontrun{
#' years %>% purrr::walk(make_collage)
#' }
make_collage <- function(year) {

  file_count <- length(dir(file.path(d2,year)))

  # if (file_count > 120) {
  #   no_rows <- 12
  # } else {
  #   no_rows <- 10
  # }
  no_cols <- 12
  no_rows <- ceiling(file_count/no_cols)

  # get number of files that need to be blanks
  max_count <- no_rows*no_cols
  if (file_count != max_count) {
    blankfiles <- paste0("img",(file_count+1):max_count,"-",year,".jpg")
    bftmp <- tidyr::crossing(blankfiles, year)
    purrr::walk2(bftmp$blankfiles, bftmp$year, make_blanks)
  }
  # count total files
  files <- dir(file.path(d2,year), full.names = TRUE)

  purrr::walk(0:(no_cols-1),
              make_column,
              files = files,
              no_rows = no_rows,
              year = year)

  # put together
  folder_loc <- file.path(d2, "local_cols/")
  colfiles <- dir(folder_loc, pattern = year)
  # subset collages
  parse_cols <- function(a, b, c) {
    magick::image_read(file.path(folder_loc, colfiles)[b:a]) %>%
      magick::image_scale("1000x1000") %>%
      magick::image_append(stack = FALSE) %>%
      magick::image_write(paste0(folder_loc, year, "collage_",c,".jpg"))
  }
  nn <- tibble::tibble(a = seq(1, length(colfiles))) %>%
    dplyr::mutate(c = floor(a/10)) %>%
    dplyr::group_by(c) %>%
    dplyr::slice(c(1,n())) %>%
    dplyr::mutate(b = lag(a)) %>%
    dplyr::filter(!is.na(b))
  purrr::pwalk(list(nn$a, nn$b, nn$c), parse_cols)
  # build into final collage
  magick::image_append(magick::image_read(dir(file.path(folder_loc),
                                              pattern = paste0(year, "collage"),
                                              full.names = TRUE)),
                       stack = FALSE) %>%
    magick::image_write(paste0(folder_loc, year, '_final_collage.jpg'))
}

#' build the image columns to be collaged
#'
#' @param i number of columns
#' @param files vector of image files that were downloaded
#' @param no_rows how many rows in each column
#' @param year year images posted
#'
make_column <- function(i, files, no_rows, year){

  ff <- tibble::tibble(f = files[(i*no_rows+1):((i+1)*no_rows)]) %>%
    dplyr::filter(!is.na(f)) %>%
    dplyr::pull()

  magick::image_read(ff) %>%
    magick::image_append(stack = TRUE) %>%
    magick::image_write(
      file.path(d2, 'local_cols', paste0("cols", i, "_", year, ".jpg"))
    )
}

#' if images are missing or column count is off, build blanks
#' @param bf file name, automatically created
#' @param year year for file save
#'
make_blanks <- function(bf, year) {
  magick::image_blank(width=1020, height=583, color = "white") %>%
    magick::image_border('white', '10x10') %>%
    magick::image_write(file.path(d2, year, bf))
}
