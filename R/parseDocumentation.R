.doc_parse <- function(all_r_files) {
    sources <- lapply(all_r_files, function(x) parse(x, keep.source = TRUE))
    docs <- lapply(sources, function(x) try(comments(getSrcref(x))))
    docs <- lapply(docs, function(x) lapply(x, as.character))
    indx <- lapply(
      lapply(docs,
             function(x){
               lapply(x,
                      function(y){
                        sum(grepl("#'", y))
                      })
             }),
      function(x){
               unlist(x, use.names = FALSE) > 0
      })
    docs <- lapply(seq_along(docs), function(j){
      docs[[j]][indx[[j]]]
      })
    # Extract @name
    doc_names <- lapply(docs, function(x){
      unlist(lapply(x, function(y){
        gsub(".+@name (\\D+)", "\\1", {
            y[grepl("@name", y)]
        })}
        ), use.names = FALSE)
      })
    docs <- unlist(docs, recursive = FALSE)
    docnames <- unlist(doc_names, recursive = FALSE)
    names(docs) <- gsub(" ", "", docnames)
    docs
}
