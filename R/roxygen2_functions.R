comments <- function(refs) {
  srcfile <- attr(refs[[1]], "srcfile")
  com <- vector("list", length(refs))
  for (i in seq_along(refs)) {
    if (i == 1) {
      first_byte <- 1
      first_line <- 1
    } else {
      first_byte <- refs[[i - 1]][4] + 1
      first_line <- refs[[i - 1]][3]
    }
    last_line <- refs[[i]][3]
    last_byte <- refs[[i]][4]
    lloc <- c(first_line, first_byte, last_line, last_byte)
    com[[i]] <- srcref(srcfile, lloc)
  }
  com
}
