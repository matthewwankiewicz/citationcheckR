#' Check Citation File
#'
#' @param file A Bibtex file containing the package citations.
#' @param packages A list of packages used in the project.
#'
#' @return Whether or not the listed packages are found in the given file.
#' @export
#'
#' @examples check_citation_file("references.bib", c("tidyverse", "Lahman"))
#'
#' check_citation_file("ref.bib", "ggplot2")
#'
check_citation_file <- function(file, packages){
  bibtib <- bib2df(file)
  create_citation_file(packages, filename = "tempfile.bib")
  packagestib <- bib2df("tempfile.bib")
  file.remove("tempfile.bib")
  results <- c()
  for(cite in 1:nrow(bibtib)){
    for(row in 1:nrow(packagestib)){
      check <- identical(packagestib[row,], bibtib[cite,])
      located <- ifelse(
        check == TRUE,
        paste(packagestib[row, 23], "found in", file),
        paste(packagestib[row, 23], "not found in", file)
      )
    }
    results <- c(results, located)
  }
  print(results)
}
