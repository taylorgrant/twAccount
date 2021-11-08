
<!-- README.md is generated from README.Rmd. Please edit that file -->

# twAccount

Extract a brand’s Twitter timeline, visualize the brand identity through
the imagery posted and topic model both the timeline and mentions of
that brand. Topic modeling relies on the `BTM` package, which is
designed for short texts as opposed to standard LDA topic modeling.

## Installation

You can install the development version of twAccount from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("taylorgrant/twAccount")
```

## Example

The package provides an overall wrapper function `twitter_tm()` that
will (1) extract a brand’s Twitter timeline, (2) recent, public tweets
matching a boolean search, and (3) all images that a brand has included
in its publicly available tweets.

Once the data is extracted, a model is run to estimate topic models
based upon the Biterm Topic Models algorithm in the `BTM` package.
Currently, the model is specified to run based on the number of topics
selected by the user, rather than running through a series of topic
sizes and maximizing the log-likelihood (maybe later).

The BTM topics are then placed into 2d space using the Jensen-Shannon
Divergence methodology and then scaling with PCA. For example, the

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
