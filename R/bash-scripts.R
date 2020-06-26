#' Render job scripts from template files
#'
#' This function uses a whisker (aka Mustache) template file to creates one or
#' more job queue scripts.  It also optionally can create a bash script that
#' launches all of the jobs.
#'
#' @param template    File path to the script template
#' @param processes   How many processes queued jobs are sharing.  This makes a
#'   `threads` variable available during template rendering.
#' @param queueScript If not FALSE than the name of the queue script file.
#' @param scriptPath  Path to the folder where rendered scripts should be saved.
#' @param otherPaths  Other directories that need to be created before the jobs
#'   are run.
#' @param prequeue    Commands to run before the jobs are launched.
#' @param postqueue   Commands to run after all jobs have finished.
#' @param ...         A variable number of other arguments that are available as
#'   variable bindings.  Values not of length 1 should all be of the same
#'   length.  Must contain at least `file` entry, listing the file names for
#'   rendered scripts.
#'
#' @export
#'
renderJobScripts <- function( template
                            , processes   = 16
                            , queueScript = paste( "queue-"
                                                 , basename(template)
                                                 , sep = ""
                                                 )
                            , scriptPath = "rendered-scripts"
                            , otherPaths = "logs"
                            , prequeue   = ""
                            , postqueue  = 'wait\necho "All jobs have finished!"'
                            , ...
                            ) {
  
  data$threads <- max(1, floor(processes / length(list(...)$file)))
  
  tmpl <- paste(readLines(template), sep = "\n")
  
  scripts <- mapply( function(...) {
                       data <- list(...)
                       s    <- whisker::whisker.render(tmpl, data = data)
                       cat(s, file = file.path(scriptPath, data$file))
                      
                       data$file
                     }
                    , ...
                    )
  
  if (queueScript != FALSE) {
    cat( "#!/bin/sh\n"
       , paste("mkdir", otherPaths)
       , prequeue
       , paste( paste0("nohup ", file.path(scriptPath, scripts)," &> ", "logs/", scripts, ".log", " & ", '\n_pid=$!\necho "$_pid" >> jobs.pid')
              , collapse = "\n"
              )
       , postqueue
       , file = file.path(scriptPath, queueScript)
       , sep  = "\n"
       )
  }
}