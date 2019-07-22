# Main program to sort affiliations according to order of appearance
# Currently set to <=3 affiliations per author
AutoAff <- function(X, aff1, aff2, aff3) {
  mat <- as.matrix(X[, c(aff1, aff2, aff3)])
  aff <- na.exclude(unique(as.character(as.vector(t(mat)))))
  n.aff <- length(aff)
  aff <- aff[1:n.aff]
  
  mat <- cbind(mat, rep(NA, dim(X)[1]))
  for (i in 1:dim(X)[1]) {
    aa <- which(aff == mat[i, 1])
    bb <- which(aff == mat[i, 2])
    cc <- which(aff == mat[i, 3])
    if (length(aa) == 0) {
      mat[i, 1] <- mat[i, 2] <- mat[i, 3] <- mat[i, 4] <- ""
    } else {
      if (length(bb) != 0){
        if (length(cc) == 0) {
          mat[i, 1] <- aff[min(c(aa, bb))]
          mat[i, 2] <- aff[max(c(aa, bb))]
          mat[i, 4] <- paste(min(c(aa, bb)), ",", max(c(aa, bb)), sep = "")
        }
        if (length(cc) != 0) {
          order <- sort(c(aa, bb, cc))
          mat[i, 1] <- aff[order[1]]
          mat[i, 2] <- aff[order[2]]
          mat[i, 3] <- aff[order[3]]
          if (order[2] - order[1] == 1 & order[3] - order[2] == 1) {
            mat[i, 4] <- paste(order[1], "-", order[3], sep = "")
          } else {
            mat[i, 4] <- paste(order[1],",", order[2], ",", order[3], sep = "")
          }
        }
      } else {
        mat[i, 4] <- paste(aa)
      }
    }
  }
  
  # data[, "a"] <- mat[, 1]
  # data[, "b"] <- mat[, 2]
  # data[, "c"] <- mat[, 3]
  # data[, "names"] <- data$LAST.NAME
  index <- mat[, 4]
  
  for (i in 1:n.aff) {
    cat(i," ", aff[i], "; " , sep = "")
  }
  
  return(list(affilliations = aff, count = n.aff, index = index))
}

# change the inputs and the cat() commands according to your need
# ?cat
# If superscripted indices are needed
# set latex = TRUE and copy-paste the output to R markdown and knit to MS Word.
aff.style <- function(first, last, degree, index, latex = FALSE) {
  first <- as.character(first)
  last <- as.character(last)
  degree <- as.character(degree)
  index <- as.character(index)
  for (i in 1:length(first)) {
    if (!latex) {
      cat(first[i], " ", last[i], ", ", degree[i], index[i], "; ", sep = "" )
    } else {
      cat(first[i], " ", last[i], ", ", degree[i], "^", index[i], "^; ", sep = "" )
    }
  }
}

# Load data, na.strings = c("", NA) set empty entries to NA
data <- read.csv("Book1.csv", na.strings = c("", NA)) # Give full path name

# Run main program AutoAff to get the affiliation list
result <- AutoAff(data, "AFFILIATION.1", "AFFILIATION.2", "AFFILIATION.3")
# Add a column of indices to original dataset
data$index <- result$index

# Get author list with indicies
aff.style(first = data$FIRST.NAME,last = data$LAST.NAME, degree = data$DEGREES, index = data$index, latex = T)

# Get affiliation list with superscripted indicies
# Knit in R Markdown
for (i in 1:result$count) {
  cat("^", i, "^ ", result$affilliations[i], "\n", sep = "")
}
