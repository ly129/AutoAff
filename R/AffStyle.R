#' Automatic generation of author list with different styles.
#' 
#' \code{AffStyle} facilitates the generation of long author list in different styles based on the requirements of research journals.
#' 
#' @param name Authors' names
#' @param degree Authors' title (PhD, MS, etc..)
#' @param index \code{index} acquired from \code{AutoAff} function. See 'Value' in \link{AutoAff}.
#' @param style Style of output. See 'Details'.
#' 
#' @details Currently supporting two styles but growing.
#' 
#' plain: John Doe, MSc 1-3; Jane Doe, PhD 1,3-5; ..
#' 
#' superscript: John Doe, MSc^1-3^; Jane Doe, PhD^1,3-5^; ..
#' 
#' @details Styles that require superscripted indices require the use of R Markdown. The output for these styles are R Markdown syntax (text between ^^ will be superscipts after knitting.)
#' 
#' @return The function returns a text string that is either ready to be pasted into your manuscript (without superscripts) or need to be copied to R Markdown and knitted (with superscripts).
#' 
#' @examples 
#' result <- AutoAff(authors, c("aff1", "aff2", "aff3", "aff4", "aff5", "aff6"))
#' AffStyle(name = authors$Name, degree = authors$Degree, index = result$index, style = "plain")
#' 
#' @export
#' 
AffStyle <- function(name, degree, index, style) {
  name <- as.character(name)
  degree <- as.character(degree)
  index <- as.character(index)
  for (i in 1:length(name)) {
    if (style == "plain") {
      cat(name[i], ", ", degree[i]," ", index[i], "; ", sep = "" )
    } 
    if (style == "superscript") {
      cat(name[i], ", ", degree[i], "^", index[i], "^; ", sep = "" )
    }
  }
}