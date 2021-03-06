#' Check BibTeX Citations
#'
#' Compares citations to R's version of the citation. If any issues are found, R will return
#' which entry the issue was found  in and what the correct entry is.
#'
#' @param citation A BibTeX citation of an R package.
#' @param package An R package to be compared to the citation.
#'
#' @return Any issues with the citation given when compared to the package's citation.
#' @export
#'
#' @examples
#'
#' check_bibtex_citations(citation, "tidyverse")
#'
#' check_bibtex_citation(citation, "dplyr")
#'
#'
#'
check_bibtex_citations <- function(citation, package){
  if(identical((grep("{", citation, fixed = T)), integer(0)) == TRUE){
    stop("Non-BibTeX citation used. Please use BibTeX citations only.")
  }
  bib <- file("temp1.bib")
  writeLines(citation, bib)
  close(bib)
  citetib <- bib2df::bib2df("temp1.bib")
  citetib <- citetib[ , colSums(is.na(citetib)) < nrow(citetib)]
  create_citation_file(package, filename = "temp2.bib",
                       include_r = FALSE)
  packagestib <- bib2df::bib2df("temp2.bib")
  packagestib <- packagestib[ , colSums(is.na(packagestib)) < nrow(packagestib)]
  file.remove("temp2.bib")
  file.remove("temp1.bib")
  if("BIBTEXKEY" %in% colnames(citetib) == TRUE){
    citetib <- dplyr::select(citetib, -BIBTEXKEY)
  }
  if(identical(citetib, packagestib) == TRUE){
    cat("No citation issues found!")
  }
  else{
    # cycle through package dataframe
    issues <- c()
    for(col in colnames(packagestib)){
      if(col %in% colnames(citetib)){
        if(identical(dplyr::select(packagestib, all_of(col)),
                     dplyr::select(citetib, all_of(col)))){
        }
        else{
          issue <- paste("Issue with entry: ", col, ". The ", col, " in ", package,
                         " is: ", dplyr::select(packagestib, all_of(col)), sep = "")
          issues <- c(issues, issue)
        }
      }
      else{
        issue <- paste("Issue with entry: ", col, ". This part of the citation is missing. In ",
                       package, " the ", col, " is: ", dplyr::select(packagestib, all_of(col)), sep = "")
        issues <- c(issues, issue)
      }
    }
    print(issues)
  }
}
