#' My Function
#'
#' @description
#' Here are more details about this function (this part is optional)
#'
#' @param input This is a value that the user must provide. Here is a description of what it should be
#'
#' @return A vector of numbers.
#' @export
#'
#' @examples
#' my_function(5)
#'
my_function <- function(input) {
  data <- c(1:input)

  return(data)
}


find_skip <- function(file) {
  min(grep(srx1200_dataHeader1, readr::read_lines(file))-1)
}


tidy_empties <- function(DF){
  DF[DF=="NaN"] <- ""
  DF[is.na(DF)] <- ""
  DF[DF=="0"] <- ""
  return(DF)
}
