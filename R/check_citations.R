#' Check Citations
#'
#' Checks if the text citation given matches the citation given by the citation function in R.
#' If the citations are different, the function will return FALSE, if they are the same it will
#' return TRUE.
#'
#' @param citation A citation for an R package.
#' @param package An R package name.
#'
#' @return TRUE if entries are the same, FALSE if there are differences
#' @export
#'
#' @examples check_citations(citation, "tidyverse", format = "text")
#'
#'
check_citations <- function(citation, package, format = "bibtex"){
  if(format == "bibtex"){
    return(citation == toBibtex(citation(package)))
  }
  if(format == "text"){
    return(citation == citation(package)$textVersion)
  }
}

