read_file = function (path, binary = FALSE) 
{
  n <- file.info(path)$size
  if (binary) {
    readBin(path, raw(), n)
  }
  else {
    readChar(path, n, TRUE)
  }
}