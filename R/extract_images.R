#' Extract and save images
#'
#' Work through a data.frame or tibble of Twitter post data to extract and save images in
#' specific folders. If multiple images were posted in a grid, this extracts each and
#' puts them into a grid format. If 2 images, a 1x1, if more than 2 it automatically
#' gets put into a 2x2 tile using `magick::image_montage()`.
#' We use a helper function to read and scale images so that we can montage without
#' needing to map, per \url{https://stackoverflow.com/questions/52833932/pass-a-list-of-image-objects-to-a-r-magick-function-instead-of-a-vector}
#'
#' @param year Year of the Twitter post
#' @param index Image number; each tweet/image is numbered
#' @param img_ct How many images were used in the Tweet
#' @param img url of the image(s)
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' tmp <- tibble(status_id = 1144617178710388737,
#' ext_media_url = "http://pbs.twimg.com/media/D-J_QYPXoAAWw2d.jpg",
#' creaated_at = 2019-06-28 07:42:56, year =2019, index = 01, img_ct = 1)
#' purrr::pwalk(list(tmp$year, tmp$index, tmp$img_ct, tmp$ext_media_url), extract_images)
#' }
extract_images <- function(year, index, img_ct, img) {

  if (img_ct == 2) {
    magick::image_append(image_read_fn(img), stack = FALSE) %>%
      magick::image_scale("1000") %>%
      magick::image_convert("jpg") %>%
      magick::image_write(file.path(d2, year,
                                    paste0('img',index,"-",year,".jpg")))

  } else if (img_ct > 2) {
    magick::image_montage(image_read_fn(img),
                          tile = '2x2',
                          geometry = 'x1000') %>%
      magick::image_scale("1000") %>%
      magick::image_convert("jpg") %>%
      magick::image_write(file.path(d2, year,
                                    paste0('img',index,"-",year,".jpg")))
  } else
    image_read_fn(img) %>%
    magick::image_convert("jpg") %>%
    magick::image_write(file.path(d2, year,
                                  paste0('img',index,"-",year,".jpg")))
}
