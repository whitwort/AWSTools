#' Generate a set of shell scripts to to launch individual jobs
#'
#' This function uses a whisker (aka Mustache) template file to create one or
#' more job scripts.
#'
#' @param template    Path to the script whisker template.
#' @param processes   How many processes queued jobs are sharing  This makes a
#'   `threads` variable available during template rendering, which doles out an
#'   even number of threads to each job based on the total process count.
#' @param scriptPath  Path to the folder where rendered scripts should be saved.
#' @param ...         A variable number of other arguments that are available as
#'   variable bindings.  Values not of length 1 should all be of the same length
#'   (the number of job scripts being created).  Must contain at least a `file`
#'   argument which lists the output file names for rendered scripts.
#'
#' @return Returns a character vector containing the names of the job script
#'   files created.
#'
#' @export
#' 
makeJobScripts <- function( template
                          , processes   = 16
                          , scriptPath  = "rendered-scripts"
                          , ...
                          ) {
  
  data$threads <- max(1, floor(processes / length(list(...)$file)))
  
  tmpl <- paste(readLines(template), sep = "\n")
  
  mapply( function(...) {
            data <- list(...)
            s    <- whisker::whisker.render(tmpl, data = data)
            cat(s, file = file.path(scriptPath, data$file))
          
            data$file
          }
        , ...
        )

}


#' Generate a queue script to launch a batch of jobs
#'
#' The queue script launches jobs as background processes, records process ids
#' in a `jobs.pid` file, and by default `waits` for completion of all of the
#' jobs and then shuts down the instance.  It also redirects both the stdout and
#' stderr streams into log files located in `logs/`.
#'
#' @param scripts     File names of job scripts to add to the queue.  Should be
#'   located in `scriptPath`.
#' @param queueScript The name of the queue script file to be created.
#' @param scriptPath  Path to the folder where jobs scripts should be saved and
#'   queue script will be created.
#' @param otherPaths  Any directories that need to be created before the jobs
#'   are run.
#' @param prequeue    Commands to run before the jobs are launched.
#' @param postqueue   Commands to run after all jobs have finished.  By default
#'   it shutsdown the instance when all of the jobs are done (if the user
#'   running the queue script has sudo priveledges).  Set to an empty string to
#'   override this behavior.
#'
#' @export
#' 
makeQueueScript <- function( scripts
                           , queueScript = "queue.sh"
                           , scriptPath  = "rendered-scripts"
                           , otherPaths  = "logs"
                           , prequeue    = ""
                           , postqueue   = 'wait; sudo shutdown now'
                           ) {
  
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