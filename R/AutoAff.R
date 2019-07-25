# Main program to sort affiliations according to order of appearance
# Currently set to <=3 affiliations per author
getIndex <- function(x, aff) {
  n <- length(x)
  indices <- NULL
  for (i in 1:n) {
    indices <- c(indices, which(aff == x[i]))
  }
  return(indices)
}

hyphen <- function(x) {
  n <- length(x)
  
  if (n == 0) p <- NA
  if (n == 1) p <- paste(x, sep = "")
  if (n == 2) {
    x <- sort(x)
    p <- paste(x[1], ",", x[2], sep = "")
  }
  if (n >= 3) {
    x <- sort(x)
    p <- paste(x[1], sep = "")
    prev.hyphen <- FALSE
    for (i in 2:(n-1)) {
      left.cts <- ((x[i] - x[i-1]) == 1)
      right.cts <- ((x[i+1] - x[i]) == 1)
      if (left.cts) {
        if (right.cts) {
          if (!prev.hyphen) {
            p <- paste(p, "-", sep = "")
            prev.hyphen <- TRUE
          }
        } else {
          if (prev.hyphen) {
            p <- paste(p, x[i], sep = "")
            prev.hyphen <- FALSE
          } else {
            p <- paste(p, ",", x[i], sep = "")
            prev.hyphen <- FALSE
          }
        }
        
      } else {
        p <- paste(p, ",", x[i], sep = "")
        prev.hyphen <- FALSE
      }
    }
    
    if ((x[n] - x[n-1]) == 1) {
      if (prev.hyphen) {
        p <- paste(p, x[n], sep = "")
      } else {
        p <- paste(p, ",", x[n], sep = "")
      }
    } else {
      p <- paste(p, ",", x[n], sep = "")
    }
  }
  
  return(p)
}

#' Automatic generation of affiliation lists.
#' 
#' \code{AutoAff} facilitates the management for long author lists and their affiliations. The outputs include a text string of R Markdown syntax that can be knitted to create superscripted indices.
#' @param X A user-supplied data frame with columns containing the authors' affiliations.
#' @param affiliation Names of the variables containing affiliations. A character vector, see 'Examples'.
#'
#' @details Empty cells (for those authors with fewer affiliations) in \code{X} have to be either \code{NA} or \code{""}.
#' 
#' @details Copy and paste \code{rmd} in \code{result} into R Markdown and knit to Microsoft Word to get affiliation lists with superscripted indices. See 'Examples'.
#' 
#' @return This function returns a list with six elements:
#' \item{rmd}{Indexed affiliations in R Markdown syntax, see 'Details'.}
#' \item{affiliations}{Affiliations by order of appearance}
#' \item{aff.count}{Number of different affiliations}
#' \item{aut.count}{Number of authors}
#' \item{index}{Indices of each author's affililation(s)}
#' \item{missing}{Number of authors with no affiliations}
#' 
#' @examples
#' result <- AutoAff(authors, c("aff1", "aff2", "aff3", "aff4", "aff5", "aff6"))
#' result
#' 
#' @export

AutoAff <- function(X, affiliation) {
  # Number of affiliations per author
  n.per <- length(affiliation)
  mat <- as.matrix(X[, affiliation])
  n.aut <- dim(mat)[1]
  for (i in 1:n.aut) {
    for (j in 1:n.per) {
      if (mat[i, j] == "") {mat[i, j] <- NA}
    }
  }
  aff <- na.exclude(unique(as.character(as.vector(t(mat)))))
  n.aff <- length(aff)
  aff <- aff[1:n.aff]
  index <- character(n.aut)
  per <- integer(n.per)
  
  ll <- split(mat, f = seq(nrow(mat)))
  index.list <- lapply(ll, FUN = getIndex, aff = aff)
  index <- unlist(lapply(index.list, hyphen))
  ms <- sum(is.na(index))
  
  rmd <- character(1)
  for (i in 1:n.aff) {
    rmd <- paste(rmd, "^", i,"^", aff[i], "; " , sep = "")
  }
  
  return(list(rmd = rmd, affilliations = aff, aff.count = n.aff, aut.count = n.aut, index = index, missing = ms))
}