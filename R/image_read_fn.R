#' Read and scale images
#'
#' Read in an image, scale it, and set white border. Just streamlines the `magick::image_read()` function.
#' @param img url or location of image to read in
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' image_read_fn("https://jeroen.github.io/images/frink.png")
#' }
image_read_fn <- function(img) {
  # safe error
  safe_image_read <- purrr::possibly(magick::image_read, otherwise = NULL)
  safe_image_read(img) %>%
    magick::image_scale("1000") %>%
    magick::image_border('white', '10x10')
}
