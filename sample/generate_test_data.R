#' create_test_data
#'
#' Generates random data for testing
#'
#' @param numcols
#' @param numrows
#'
#' @return
#' @export
create_test_data <- function(numcols = 20, numrows = 100) {
  mvdata = matrix(runif(numrows*numcols), ncol = numcols, byrow = TRUE)
  colnames(mvdata) = paste("sample", 1:numcols, sep = "")
  rownames(mvdata) = paste("var", 1:numrows, sep = "")
  return(mvdata)
}

#' create_test_metadata
#'
#' @param numcols
#'
#' @return
#' @export
create_test_metadata <- function(numcols = 20) {
  metadata = data.frame(f = factor(runif(numcols) < 0.5),
                        g = factor(runif(numcols) < 0.5),
                        h = runif(numcols))
  rownames(metadata) = paste("sample", 1:numcols, sep = "")
  return(metadata)
}
