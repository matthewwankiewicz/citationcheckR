
<!-- README.md is generated from README.Rmd. Please edit that file -->

# citationcheckR

<!-- badges: start -->

<!-- badges: end -->

The goal of citationcheckR is to make citing multiple package a little
bit easier. Instead of having to copy and paste multiple citations, you
can simply use citationcheckR to create multiple citations at once.

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("matthewwankiewicz/citationcheckR")
```

## After using the Functions

Once you have created your citations and have them stored in another
file, you will need to make changes to the YAML in your R Markdown file
by adding two lines of text. The first line should say `bibliography:
references.bib` or the name for your citation file, this will tell R
where your citations are located.

The other line you can include is `nocite: '@*'`. This will
automatically add the citations found in your citation file to your R
Markdown document. If you plan to cite each package in the body of your
paper you don’t need to include this line.

Example of the YAML header to include citations:

``` r
title:

author:

bibliography: references.bib

nocite: '@*'
```
