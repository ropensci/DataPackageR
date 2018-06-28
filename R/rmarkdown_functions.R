read_file <- function(path) {
  n <- file.info(path)$size
  readChar(path, n, TRUE)
}
