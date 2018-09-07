.qualify_changes <- function(new, old) {
  new_names <- setdiff(names(new),"DataVersion")
  old_names <- setdiff(names(old),"DataVersion")
  added <- NULL
  deleted <- NULL
  changed <- NULL
  added <- setdiff(new_names,old_names)
  deleted <- setdiff(old_names,new_names)
  common <- intersect(new_names,old_names)
  #test for equality
  changed <- purrr::keep(
    purrr::map2(new[common], old[common], `!=`),
    .p = function(x) {
      x == FALSE
    }
  )
  changed <- names(changed)
  list(added = added,
       deleted = deleted,
       changed = changed)
}