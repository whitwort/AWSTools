#' Generate a set of shell scripts to to launch individual jobs
#'
#' This function uses a whisker (aka Mustache) template file to create one or
#' more job scripts.
#'
#' @param template Path to the script whisker template.
#' @param processes How many processes launched jobs are sharing  This makes a
#'   `threads` variable available during template rendering, which doles out an
#'   even number of threads to each job based on the total process count.
#' @param scriptPath Path to the folder where rendered scripts should be saved.
#' @param ... A variable number of other arguments that are available as
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


#' Generate a script to launch a batch of jobs
#'
#' The launch script starts jobs as background processes, records process ids in
#' a `jobs.pid` file, and by default `waits` for completion of all of the jobs
#' and then shuts down the instance.  It also redirects both the stdout and
#' stderr streams into log files.
#'
#' @param scripts File names of job scripts to add to launch.  Should be located
#'   in `scriptPath`.
#' @param launchScript The name of the launch script file to be created.
#' @param scriptPath Path to the folder where jobs scripts should be saved and
#'   launch script will be created.
#' @param logPath Directory where logs recording stdout and stderr streams for
#'   each job should be saved.
#' @param prelaunch Commands to run before the jobs are launched.
#' @param postlaunch Commands to run after all jobs have finished.  By default
#'   it shutsdown the instance when all of the jobs are done (assuming the user
#'   running the launch script has sudoer priveledges).  Set to an empty string
#'   to override this behavior.
#'
#' @export
#' 
makeLaunchScript <- function( scripts
                            , launchScript = "launch.sh"
                            , scriptPath   = "rendered-scripts"
                            , logPath      = "logs"
                            , prelaunch    = ""
                            , postlaunch   = 'wait; sudo shutdown now'
                            ) {
  
  cat( "#!/bin/sh\n"
     , paste("mkdir", logPath)
     , prelaunch
     , paste( paste0("nohup ", file.path(scriptPath, scripts)," &> ", logPath, "/", scripts, ".log", " & ", '\n_pid=$!\necho "$_pid" >> jobs.pid')
            , collapse = "\n"
            )
     , postlaunch
     , file = file.path(scriptPath, launchScript)
     , sep  = "\n"
     )
  
}