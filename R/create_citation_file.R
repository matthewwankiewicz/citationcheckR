#' Create Citation File
#'
#' This function is an extension of the `create_citation` file. `create_citation_file` creates a
#' new file which contains your citations. The file name can be changed but by default it will be
#' called 'references.bib'.
#'
#' @param packages A list of packages that you have used or plan to use in your document.
#' @param format The type of citations you want to return.
#' @param filename A file containing the citations of the packages used. Called 'references.bib' by default.
#'
#' @return A new file containing the citations of packages.
#' @export
#'
#' @examples create_citation_file(packages = list("tidyverse", "DoSStoolkit"),
#'  format = "bibtex")
#'
#' create_citation_file(c("tidyverse", "Lahman"))
#'
#' create_citation_file(c("DoSStoolkit"), format = "text")
#'
create_citation_file <- function(packages, format = "bibtex", filename = "references.bib"){
  suppressWarnings(if("base" %in% packages){
    if(format == "bibtex"){
      bib <- file(filename)
      citations <- list()
      for(reference in packages){
        citations <- append(citations, citation(reference))
      }
      cites <- as.character(toBibtex(citations))
      writeLines(cites, bib)
      close(bib)
    }
    if(format == "text"){
      citations <- list()
      for(reference in packages){
        citations <- append(citations, citation(reference))
      }
      cites <- citations$textVersion
      invisible(lapply(cites, write, filename, append=TRUE, ncolumns=length(cites)))
    }
  }
  else{
    packages <- append(packages, "base")
    if(format == "bibtex"){
      bib <- file(filename)
      citations <- list()
      for(reference in packages){
        citations <- append(citations, citation(reference))
      }
      cites <- as.character(toBibtex(citations))
      writeLines(cites, bib)
      close(bib)
    }
    if(format == "text"){
      citations <- list()
      for(reference in packages){
        citations <- append(citations, citation(reference))
      }
      cites <- citations$textVersion
      invisible(lapply(cites, write, filename, append = TRUE, ncolumns = length(cites)))
    }
  })
}
