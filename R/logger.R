select_console_appender <- function(){
  if (getOption('DataPackageR_verbose', TRUE)){
    appender.console()
  } else {
    # quiet console appender
    function(line) { }
  }
}

.multilog_setup <- function(LOGFILE = NULL) {
  flog.logger(
    name = "console",
    appender = select_console_appender(),
    threshold = INFO
  )
}
