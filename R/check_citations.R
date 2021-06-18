#' Check Citations
#'
#' @param citation A citation for an R package.
#' @param package An R package name.
#'
#' @return TRUE if entries are the same, FALSE if there are differences
#' @export
#'
#' @examples check_citations(citation, "Lahman", format = "text")
#'
#'
check_citations <- function(citation, package, format = "bibtex"){
  if(format == "text"){
    return(citation == citation(package)$textVersion)
  }
  if(format == "bibtex"){
    return(citation == toBibtex(citation(package)))
  }
}
