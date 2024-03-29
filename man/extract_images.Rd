% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_images.R
\name{extract_images}
\alias{extract_images}
\title{Extract and save images}
\usage{
extract_images(year, index, img_ct, img)
}
\arguments{
\item{year}{Year of the Twitter post}

\item{index}{Image number; each tweet/image is numbered}

\item{img_ct}{How many images were used in the Tweet}

\item{img}{url of the image(s)}
}
\value{

}
\description{
Work through an \code{rtweet} timeline object to extract and save images in
specific folders. If multiple images were posted in a grid, this extracts each and
puts them into a grid format. If 2 images, a 1x1, if more than 2 it automatically
gets put into a 2x2 tile using \code{magick::image_montage()}.
A helper function reads and scales images so that we can montage without
needing to map, per \url{https://stackoverflow.com/questions/52833932/pass-a-list-of-image-objects-to-a-r-magick-function-instead-of-a-vector}
}
\examples{
\dontrun{
tmp <- tibble(status_id = 1144617178710388737,
ext_media_url = "http://pbs.twimg.com/media/D-J_QYPXoAAWw2d.jpg",
creaated_at = 2019-06-28 07:42:56, year =2019, index = 01, img_ct = 1)
purrr::pwalk(list(tmp$year, tmp$index, tmp$img_ct, tmp$ext_media_url), extract_images)
}
}
