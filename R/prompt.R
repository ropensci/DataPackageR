.prompt_user_for_change_description <-
  function(interact = getOption(
               "DataPackageR_interact",
               interactive()
             )) {
    if (interactive()&interact) {
      cat(cli::col_cyan("Enter a text description of the changes for the NEWS.md file.\n")) #nocov
    }else{
      if (getOption('DataPackageR_verbose', TRUE)){
        cat(cli::col_cyan("Non-interactive NEWS.md file update.\n"))
      }
    }
    change_description <-
      ifelse(
        interactive() & interact,
        readline(prompt = "+ "),
        "Package built in non-interactive mode"
      )
    return(change_description)
  }

.update_news_md <- function(version = "Version Not Provided",
                            interact = getOption(
                              "DataPackageR_interact",
                              interactive()
                            )) {
  news_file <- .newsfile()
  change_description <-
    .prompt_user_for_change_description(interact = interact)
  news_con <- file(news_file, open = "r+")
  news_file_data <- readLines(news_con)
  writeLines(
    text = paste0("DataVersion: ", version),
    con = news_con,
    sep = "\n"
  )
  writeLines("=======================",
    con = news_con,
    sep = "\n"
  )
  writeLines(c(change_description, ""),
    con = news_con,
    sep = "\n"
  )
  writeLines(news_file_data,
    con = news_con,
    sep = "\n"
  )
  flush(news_con)
  close(news_con)
}

.newsfile <- function() {
  newsfile <- file.path(usethis::proj_get(), "NEWS.md")
  if (!file.exists(newsfile)) {
    .multilog_trace("NEWS.md file not found, creating!")
    file.create(newsfile)
  }
  return(newsfile)
}

.update_news_changed_objects <- function(objectlist) {
  news_file <- .newsfile()
  news_con <- file(news_file, open = "r+")
  news_file_data <- readLines(news_con)
  header_1 <- grep("DataVersion", news_file_data)[1]
  # header_2 <- grep("DataVersion", news_file_data)[2]
  ul_1 <- grep("=====", news_file_data)[1]
  # ul_2 <- grep("=====", news_file_data)[2]
  stopifnot(header_1 == ul_1 - 1)
  # stopifnot(header_2 == ul_2 - 1)
  header <- news_file_data[header_1:ul_1]
  news_file_data <- news_file_data[-c(header_1:ul_1)]
  #write header
  writeLines(
    text = header,
    con = news_con,
    sep = "\n"
  )
  #write changes
  added <- objectlist[["added"]]
  deleted <- objectlist[["deleted"]]
  changed <- objectlist[["changed"]]

  .write_changes <- function(string, news_con, what = NULL) {
    if (length(string) != 0) {
      if (getOption('DataPackageR_verbose', TRUE)){
        cat(cli::col_cyan(paste0("* ",what,": ",string,"\n")), sep = "")
      }
       writeLines(text = paste0("* ",what,": ", string),
                  con = news_con,
                  sep = "\n")
    }
  }
  .write_changes(added, news_con, "Added")
  .write_changes(deleted, news_con, "Deleted")
  .write_changes(changed, news_con, "Changed")

  #write the rest of the data
  writeLines(news_file_data,
             con = news_con,
             sep = "\n"
  )
  flush(news_con)
  close(news_con)
}
