image_read_fn <- function(img) {
  magick::image_read(img) %>%
    magick::image_scale("1000") %>%
    magick::image_border('white', '10x10')
}
