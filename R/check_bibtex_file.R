#' Check Bibtex File
#'
#' This function goes through a file containing BibTeX citations and compares the citations to
#' a vector of citations given.
#'
#' NOTE: The order of the citations vector must be the same as the order
#' of the citations in the file. For example, if the citations vector contains 2 packages and is
#' ordered as c(PackageA, PackageB), PackageA must be the first citation to appear in the citation
#' file.
#'
#' @param file File containing BibTeX citations.
#' @param citations A vector of packages, in the order they appear in your citation file.
#'
#' @return Any issues with the citation given when compared to the package's citation.
#' @export
#'
#' @examples
#'
#' check_bibtex_file("references.bib", c("tidyverse", "Lahman"))
#'
#'
#' check_bibtex_file("references.bib", "knitr")
#'



find_refs <- function(file){
  lines <- readLines(file)
  lines_vec <- c()
  start = 1
  for(i in 1:length(lines)){
    if(lines[i] == "}"){
      end = i
      x <- paste(lines[start:end], collapse = "\n")
      start = i+2
      lines_vec <- c(lines_vec, x)
    }
  }
  return(lines_vec)
}


check_bibtex_file <- function(file, citations){
  file_cites <- find_refs(file)
  for(i in 1:length(citations)){
    if(identical((grep("{", citations[i], fixed = T)), integer(0)) == TRUE){
      stop("Non-bibTeX citation used. Please use BibTeX citations only.")
    }}
  for(i in 1:length(file_cites)){
    print(paste("Checking for", citations[i], "package in", "file"))
    check_bibtex_citations(file_cites[i], citations[i])
  }
}
