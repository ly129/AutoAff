## code to prepare `authors` dataset goes here
set.seed(1)

aff <- character(26)
for (i in 1:13) {
  aff[i] <- paste("University of", LETTERS[i])
}
for (i in 14:26) {
  aff[i] <- paste(LETTERS[i], "Institute")
}

n <- 140
p <- 6
authors <- matrix(character(n*p), nrow = n)
for (i in 1:7) {
  for (j in 1:20) {
    authors[20*(i-1) + j, ] <- c(sample(aff, size = 7-i, replace = FALSE), rep("", i-1))
  }
}
authors <- authors[sample(1:n), ]

authors <- as.data.frame(authors)
names <- sapply(1:140, FUN = function(x) {paste("Author", x)})
authors$names <- names
authors$first <- sapply(1:140, FUN = function(x) {paste("First", x)})
authors$middle <- sapply(1:140, FUN = function(x) {paste("Middle", x)})
authors$last <- sapply(1:140, FUN = function(x) {paste("Last", x)})
authors <- authors[, c(7,8,9,10,1,2,3,4,5,6)]
names(authors) <- c("Name", "First", "Middle", "Last", "aff1", "aff2", "aff3", "aff4", "aff5", "aff6")
authors$Degree <- sample(c("BA", "MSc", "PhD"), size = n, replace = TRUE)

# library(AutoAff)
# result <- AutoAff(authors, c("aff1", "aff2", "aff3", "aff4", "aff5", "aff6"))

usethis::use_data(authors, overwrite = TRUE)

