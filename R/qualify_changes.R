.qualify_changes <- function(new, old) {
  # don't need DataVersion here
  new[["DataVersion"]] <- NULL
  old[["DataVersion"]] <- NULL
  new <- unlist(new)
  old <- unlist(old)
  added <- setdiff(names(new), names(old))
  deleted <- setdiff(names(old), names(new))
  common <- intersect(names(new), names(old))
  #test for equality
  changed <- common[new[common] != old[common]]
  list(added = added,
       deleted = deleted,
       changed = changed)
}
