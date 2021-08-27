#' Create Citation File
#'
#' This function is an extension of the `create_citation` file. `create_citation_file` creates a
#' new file which contains your citations. The file name can be changed but by default it will be
#' called 'references.bib'.
#'
#' @param packages A list of packages that you have used or plan to use in your document.
#' @param format The type of citations you want to return.
#' @param filename A file containing the citations of the packages used. Called 'references.bib'
#' by default.
#' @param include_r If set to TRUE, will create a citation for R.
#'
#' @return A new file containing the citations of packages.
#' @export
#'
#' @examples create_citation_file(packages = list("tidyverse", "DoSStoolkit"),
#'  format = "bibtex")
#'
#' create_citation_file(c("tidyverse", "Lahman"), include_r = TRUE)
#'
#' create_citation_file(c("DoSStoolkit"), format = "text")
#'
create_citation_file <- function(packages = NULL, format = "bibtex",
                                 filename = "references.bib",
                                 include_r = TRUE){
  suppressWarnings( # need this or it gets annoying
    if(missing(packages)){
      packages <- c(packages, .packages())
      if(format == "bibtex"){
        bib <- file(filename)
        citations <- list()
        for(reference in packages){
          citations <- append(citations, citation(reference))
        }
        if(include_r == TRUE){
          citations <- unique(citations)
          cites <- as.character(toBibtex(citations))
          writeLines(cites, bib)
          close(bib)
        }
        else if(include_r == FALSE){
          citations_noR <- citations[!(citations %in% citation("base"))]
          citations_noR <- unique(citations_noR)
          cites <- as.character(toBibtex(citations_noR))
          writeLines(cites, bib)
          close(bib)
        }
      }
      else if(format == "text"){
        citations <- list()
        for(reference in packages){
          citations <- append(citations, citation(reference))
        }
        if(include_r == TRUE){
          citations <- unique(citations)
          cites <- citations$textVersion
          invisible(lapply(cites, write, filename, append=TRUE, ncolumns=length(cites)))
        }
        else if(include_r == FALSE){
          citations_noR <- citations[!(citations %in% citation("base"))]
          citations_noR <- unique(citations_noR)
          cites <- citations_noR$textVersion
          invisible(lapply(cites, write, filename, append=TRUE, ncolumns=length(cites)))
        }
      }
    }
    else{
      if(format == "bibtex"){
        citations <- c()
        bib <- file(filename)
        for(reference in packages){
          citations <- append(citations, citation(reference))
        }
        if(include_r == FALSE){
          citations <- unique(citations)
          cites <- as.character(toBibtex(citations))
          writeLines(cites, bib)
          close(bib)
        }
        else if(include_r == TRUE){
          citations <- c(citations, citation("base"))
          citations <- unique(citations)
          cites <- as.character(toBibtex(citations))
          writeLines(cites, bib)
          close(bib)
        }
      }
      else if(format == "text"){
        citations <- c()
        bib <- file(filename)
        for(reference in packages){
          citations <- append(citations, citation(reference))
        }
        if(include_r == FALSE){
          citations <- unique(citations)
          cites <- citations$textVersion
          invisible(lapply(cites, write, filename, append=TRUE, ncolumns=length(cites)))
        }
        else if(include_r == TRUE){
          citations <- c(citations, citation("base"))
          citations <- unique(citations)
          cites <- citations$textVersion
          invisible(lapply(cites, write, filename, append=TRUE, ncolumns=length(cites)))
        }
      }}
  )
}
