.multilog_warn <- function(msg) {
  flog.warn(msg, name = "console")
}
.multilog_fatal <- function(msg) {
  flog.fatal(msg, name = "console")
}
.multilog_error <- function(msg) {
  flog.error(msg, name = "console")
}

.multilog_thresold <- function(console = INFO, logfile = TRACE) {
  flog.threshold(console, name = "console")
}

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
