#' Check Bibtex File
#'
#' @param file File containing BibTeX citations.
#' @param citations A vector of citations, in the order they appear in your citation file.
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
