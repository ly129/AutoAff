#' Automatic generation of author list with customized styles.
#' 
#' \code{AffStyle} facilitates the generation of long author list in different styles based on the requirements of research journals.
#' 
#' @param X A user-supplied data frame with columns containing the authors' affiliations.
#' @param first Name of the variable containing the authors' first names. See 'Examples'.
#' @param middle Name of the variable containing the authors' middle names. See 'Examples'.
#' @param last Name of the variable containing the authors' last names. See 'Examples'.
#' @param name Name of the variable containing the authors' full names. See 'Examples'.
#' @param degree Name of the variable containing the authors' title (PhD, MS, etc..). See 'Examples'.
#' @param index \code{index} acquired from \code{AutoAff} function. See 'Value' in \link{AutoAff}.
#' @param template A template indicating the style of output. See 'Details' and 'Examples'.
#' 
#' @details Currently supporting two styles but growing.
#' 
#' \code{template} represents a style specified by the user.
#' 
#' f: first name
#' 
#' m: middle name
#' 
#' l: last name
#' 
#' n: full name
#' 
#' d: degree
#' 
#' i: index (acquired by \code{AutoAff})
#' 
#' ^foo^: if 'foo' needs to be superscripted
#' 
#' Any other characters can be used based on the user's need.
#' 
#' @details Styles that include superscripted indices require the use of R Markdown. The output for these styles are R Markdown syntax (text between ^^ will be superscipts after knitting.)
#' 
#' @return The function returns a text string that is either ready to be pasted into your manuscript (without superscripts) or need to be copied to R Markdown and knitted (with superscripts).
#' 
#' @examples 
#' result <- AutoAff(authors, c("aff1", "aff2", "aff3", "aff4", "aff5", "aff6"))
#' 
#' template <- "f m l, d^i^;"
#' AffStyle(X = authors, first = "First", middle = "Middle", last = "Last", degree = "Degree", index = result$index, template = template)
#' 
#' @export
#' 

AffStyle <- function(X, first, last, middle, name, degree, index, template) {
  col <- strsplit(template, split = "")[[1]]
  n <- dim(X)[1]
  p <- length(col)
  
  mat <- matrix(character(n*p), nrow = n)
  colnames(mat) <- col
  mat <- as.data.frame(mat)
  
  for (i in 1:p) {
    if (col[i] == "f") mat[, i] <- X[, first]
    if (col[i] == "m") mat[, i] <- X[, middle]
    if (col[i] == "l") mat[, i] <- X[, last]
    if (col[i] == "n") mat[, i] <- X[, name]
    if (col[i] == "d") mat[, i] <- X[, degree]
    if (col[i] == "i") mat[, i] <- index
    if (!col[i] %in% c("f", "m", "l", "n", "d", "i")) mat[, i] <- col[i]
  }
  
  apply(mat, MARGIN = 1, function(x) {cat(x, " ", sep = "")})
}