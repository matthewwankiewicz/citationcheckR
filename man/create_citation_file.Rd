% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_citation_file.R
\name{create_citation_file}
\alias{create_citation_file}
\title{Create Citation File}
\usage{
create_citation_file(
  packages = NULL,
  format = "bibtex",
  filename = "references.bib",
  include_r = TRUE
)
}
\arguments{
\item{packages}{A list of packages that you have used or plan to use in your document.}

\item{format}{The type of citations you want to return.}

\item{filename}{A file containing the citations of the packages used. Called 'references.bib'
by default.}

\item{include_r}{If set to TRUE, will create a citation for R.}
}
\value{
A new file containing the citations of packages.
}
\description{
This function is an extension of the `create_citation` file. `create_citation_file` creates a
new file which contains your citations. The file name can be changed but by default it will be
called 'references.bib'.
}
\examples{
create_citation_file(packages = list("tidyverse", "DoSStoolkit"),
 format = "bibtex")

create_citation_file(c("tidyverse", "Lahman"), include_r = TRUE)

create_citation_file(c("DoSStoolkit"), format = "text")

}
