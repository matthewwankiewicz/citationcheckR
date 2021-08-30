#' Find Refs
#'
#' This is a helper function for the `check_bibtex_file` function.
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
#'
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
