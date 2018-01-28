## Some helper functions for tests

## create a path to a pdf file, remove a file if present
testfile = function(x) {
  result = file.path("figures", paste0(x, ".pdf"))
  if (file.exists(result)) {
    unlink(result)
  }
  result
}


## convert a string into a vector of characters
tochars = function(x) {
  unlist(strsplit(x, ""))
}



## write some text to a temporary file
## removes existing file, writes, returns the filename
tofile = function(x, filename=file.path("data", "style.tmp.Rcss")) {
  if (file.exists(filename)) {
    unlink(filename)
  }
  write(x, filename)
  filename
}

