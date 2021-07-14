#' create_citations
#'
#' This function creates citations for multiple R packages at once. By default, this function will create
#' a citation for R, along with the other functions you include. When creating the citations, you
#' can choose to either receive text citations or have them formatted in BibTeX. The function will return
#' citations in the console pane in your RStudio window.
#'
#' @param packages A list of packages that you have used or plan to use in your document.
#' @param format The type of citations you want to return. Returns bibtex by default, use `format = text` to return text citations.
#' @param include_r If set to TRUE, will create a citation for R.
#'
#' @return A list of citations for the packages included in a document.
#' @export
#'
#' @examples create_citations(packages = list("tidyverse", "DoSStoolkit"),
#'  format = "bibtex")
#'
#' create_citations(list("Lahman", "ggplot2", "forcats"),
#' "text")
#'
#' create_citations(list("tidyverse"))
#'
create_citations <- function(packages, format = "bibtex",
                             include_r = TRUE){
  suppressWarnings(if(include_r == FALSE){
    if(format == "bibtex"){
      citations <- list()
      for(reference in packages){
        citations <- append(citations, citation(reference))
      }
      print(toBibtex(citations))
    }
    if(format == "text"){
      citations <- list()
      for(reference in packages){
        citations <- append(citations, citation(reference))
      }
      print(citations)
    }
  }
  else{
    packages <- append(packages, "base")
    if(format == "bibtex"){
      citations <- list()
      for(reference in packages){
        citations <- append(citations, citation(reference))
      }
      print(toBibtex(citations))
    }
    if(format == "text"){
      citations <- list()
      for(reference in packages){
        citations <- append(citations, citation(reference))
      }
      print(citations)
    }
  })
}
