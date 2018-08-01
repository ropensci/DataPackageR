.prompt_user_for_change_description <- function(){
  cat(crayon::cyan("Enter a text description of the changes for the NEWS file.\n")) #nolint
  change_description <- ifelse(interactive(),readline(prompt = "+ "),
                               "Package built in non-interactive mode")
  return(change_description)
}

.update_news_md <- function(version = "Version Not Provided") {
  news_file <- .newsfile()
    change_description <- .prompt_user_for_change_description()
  news_con <- file(news_file, open = 'r+')  
  news_file_data <- readLines(news_con)
  writeLines(text  = paste0("DataVersion: ",version),
             con = news_con, 
             sep = "\n")
  writeLines("=======================",
             con = news_con,
             sep = "\n")
  writeLines(c(change_description,""), 
             con = news_con, 
             sep = "\n")
  writeLines(news_file_data, 
             con = news_con,
             sep = "\n")
  flush(news_con)
  close(news_con)
}

.newsfile <- function() {
  newsfile <- file.path(usethis::proj_get(),"NEWS.md")
  if (!file.exists(newsfile)) {
    flog.info("NEWS.md file not found, creating!")
    con <- file(newsfile, open = 'w+', blocking = FALSE)
  } else{
    flog.info("NEWS.md file found, updating!")
    con <- file(newsfile, open = 'r+', blocking = FALSE)
  }
  close(con)
  return(newsfile)
}
