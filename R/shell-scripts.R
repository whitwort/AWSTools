
#' Generate a set of shell scripts to launch individual jobs
#'
#' This function uses a whisker (aka Mustache) template file to create one or
#' more job scripts.
#'
#' @param template Path to the script whisker template.
#' @param processes The number of processes launched jobs are sharing  This
#'   makes a `threads` variable available during template rendering, which doles
#'   out an even number of threads to each job based on the total process count.
#' @param jobScripts The file names for the job scripts to create through
#'   templating.  Should match the length of the longest variable argument.
#' @param scriptPath Path to the local folder where rendered scripts should be
#'   saved.
#' @param ... A variable number of other arguments that are available as
#'   variable bindings when the template is rendered.  Values not of length 1
#'   should all be of the same length (the number of job scripts being created).
#'
#' @return A List with one named element (\code{jobs}) containing a vector of
#'   job script file names.  Passed to \code{\link{makeLaunchScript}}
#'
#' @export
#' 
makeJobScripts <- function( template
                          , jobScripts
                          , processes   = 1
                          , scriptPath  = "rendered-scripts"
                          , ...
                          ) {
  
  if (!file.exists(scriptPath)) { dir.create(scriptPath) }
  
  scriptPath <- normalizePath(scriptPath)
  threads    <- max(1, floor(processes / length(jobScripts)))
  tmpl       <- paste(readLines(template), sep = "\n")
  
  renderScripts <- function(...) {
    data <- list(...)
    s    <- whisker::whisker.render(tmpl, data = data)
    cat(s, file = file.path(scriptPath, data$file))
  }
  
  do.call( mapply
         , args = c( list( FUN     = renderScripts
                         , file    = jobScripts
                         , threads = threads
                         )
                   , list(...)
                   )
         )

  list(jobs = jobScripts, localPath = scriptPath)
}


#' Generate a script to launch a batch of jobs
#'
#' The launch script starts jobs as background processes, records process ids in
#' a `jobs.pid` file, and by default `waits` for completion of all of the jobs
#' and then shuts down the instance.  It also redirects both the stdout and
#' stderr streams into log files.
#'
#' @param scripts List with job script file names returned by
#'   \code{\link{makeJobScripts}}.
#' @param launchScript The name of the launch script file to be created.
#' @param workingPath Path to the working directory on remote EC2 instances
#'   where scripts should be copied and executed.
#' @param logPath Folder where logs recording stdout and stderr streams for each
#'   job should be saved.  This is created as a subdirectory of
#'   \code{workingPath}.
#' @param prelaunch Commands to run before the jobs are launched.
#' @param postlaunch Commands to run after all jobs have finished.  The default
#'   shutsdown the instance after all of the jobs have finished running.  Set
#'   to an empty string to override this behavior.
#'
#' @return Named list with job and launch script file names.  Passed to
#'   \code{\link{launchJobs}}.
#' @export
#' 
makeLaunchScript <- function( scripts
                            , launchScript = "launch.sh"
                            , workingPath  = "~"
                            , logs         = "logs"
                            , prelaunch    = ""
                            , postlaunch   = 'wait; sudo shutdown now'
                            ) {
  
  scripts$launch     <- launchScript
  scripts$remotePath <- workingPath
  scripts$logPath    <- file.path(workingPath, logs, fsep = "/")
  
  cat( "#!/bin/sh\n"
     , paste("mkdir", scripts$logPath)
     , prelaunch
     , paste( paste0( "nohup "
                    , file.path(scripts$remotePath, scripts$jobs, fsep = "/")
                    ," &> "
                    , file.path(scripts$logPath, paste0(scripts$jobs, ".log"), fsep = "/")
                    , ' &\n _pid=$!\necho "$_pid" >> jobs.pid'
                    )
            , collapse = "\n"
            )
     , postlaunch
     , file = file.path(scripts$localPath, launchScript)
     , sep  = "\n"
     )
  
  scripts
  
}