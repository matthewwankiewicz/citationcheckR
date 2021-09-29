#' Check Citation File
#'
#' This function looks at your BibTeX file that contains citations. It takes in a file name
#' and a list of packages. The function will look through the citation file and check if the
#' packages listed are found in the citation file. Packages that are both listed in the function
#' and found in the citation file will be printed in the console.
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
  bibtib <- bib2df::bib2df(file)
  create_citation_file(packages, filename = "tempfile.bib",
                       include_r = FALSE)
  packagestib <- bib2df::bib2df("tempfile.bib")
  file.remove("tempfile.bib")
  results <- c()
  for(row in 1:nrow(packagestib)){
    filepack <- packagestib[row, colSums(is.na(packagestib)) < nrow(packagestib)]
    filepack <- filepack[1, colSums(is.na(filepack)) < nrow(filepack)]
    for(row1 in 1:nrow(bibtib)){
      funcpack <- bibtib[row1, colSums(is.na(bibtib)) < nrow(bibtib)]
      funcpack <- funcpack[1, colSums(is.na(funcpack)) < nrow(funcpack)]
      if(identical(funcpack, filepack) == TRUE){
        cat(paste0(bibtib[row1, 23], " found in: ", file))
      }
    }
  }
}

