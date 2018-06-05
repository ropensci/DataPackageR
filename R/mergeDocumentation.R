.mergeDocumentation = function(old, new) {
    merged = list()
    oldnames = names(old)
    newnames = names(new)
    for (i in oldnames) {
        if (i %in% newnames) {
            merged[[i]] = new[[i]]
        } else {
            merged[[i]] = old[[i]]
        }
    }
    for (i in newnames) {
        if (!i %in% oldnames) {
            merged[[i]] = new[[i]]
        }
    }
    return(merged)
}
