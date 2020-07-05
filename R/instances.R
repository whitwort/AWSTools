
#' Launch an EC2 instance
#'
#' This wrapper function launches an EC2 instance from an AMI, monitors instance
#' initialization and startup, tests ssh connectivity, and handles formatting
#' and mounting any additional block devices at the specified locations.  It
#' returns an \code{instance} object which is used by other functions in this
#' package.
#'
#' This function is designed to be as simple as possible and does not support
#' the vast majority of configuration options available when launching EC2
#' instances.  If it doesn't meet your needs see the \code{paws} package for a
#' full featured EC2 SDK.  You can use the \code{paws::ec2()$run_instances}
#' function to launch your instance and then pass the `InstanceID` to the
#' \code{\link{getInstanceDescription}} function to generate an \code{instance}
#' object to use with the other functions in this package.
#'
#' @param imageId The image-id for the AMI used to launch the instance.
#' @param keyName The key-pair on your AWS account to use.  This pair should be
#'   associated with one of the default public keys on the current account that
#'   \code{ssh} will try to use when connecting to your instance.
#' @param securityGroupIds The EC2 security group id(s) to use.
#' @param instanceType The type of instance to launch.
#' @param shutdownBehavior "stop" or "terminate" indicating what the instance
#'   should do when a system command initiates a shutdown.  \bold{NOTE:} the
#'   default here (terminate) is different than the normal API default (stop).
#'   Stopping an instance pauses it while preserving all attached volumes; it
#'   can be resumed.  Terminating an instance destroys it and releases any
#'   attached ephemeral storage; it cannot be resumed.  The design of this
#'   package assumes you are using attached block devices as "scratch" storage
#'   during data analysis runs, thus the default is to terminate instances on
#'   shutdown.
#' @param tags Tags to apply to your instance and its resources. This should
#'   either be NULL or a normal R list with named elements; names will be tag
#'   Keys and elements their Values.
#' @param blockDevices Additional block devices to create and mount. This should
#'   either be NULL or an object returned by the \code{\link{blockDevices}}
#'   function.
#' @param throttle The amount of time to wait (in seconds) before repeating
#'   requests to the AWS API.  The account-wide limit is 10,000 r/s, so this
#'   default is extremely conservative.
#' @param username The administrator username to use when logging in over ssh.
#'   The user must have sudo privileges. Default is set for Amazon Linux AMIs.
#' @param sshTimeout How many times to re-test connecting over ssh after
#'   recieving a timeout or connection refused error.  This function waits the
#'   duraction of 'throttle' between attempts.
#'
#' @return An \code{instance} object that can be used with other functions in
#'   this package.
#' @export
#' 
launchInstance <- function( imageId
                          , keyName
                          , securityGroupIds
                          , instanceType       = "t2.micro"
                          , shutdownBehavior   = "terminate"
                          , tags               = NULL
                          , blockDevices       = NULL
                          , throttle           = 1
                          , username           = "ec2-user"
                          , sshTimeout         = 10
                          ) {
  
  # format TagSpecifications from tags
  tagSpecifications <- if (!is.null(tags)) {
    formattedTags <- lapply( names(tags)
                           , function(key) {
                               list(Key = key, Value = tags[[key]])
                             }
                           )
    list(
      list( ResourceType = "instance"
          , Tags         = formattedTags
          )
    )
  } else { NULL }
  
  # format BlockDeviceMappings from blockDevices
  blockDeviceMappings <- if (!is.null(blockDevices)) {
    lapply(blockDevices, function(l) { l$mapping })
  } else { NULL }
  
  # create a paws::EC2 service
  ec2 <- paws::ec2()
  
  # check that AWS API access to EC2 is configured
  cat("Checking AWS API access for EC2...")
  tryCatch( ec2$describe_instances
          , error = function(e) {
              cat( crayon::red(" failed.\n")
                 , "There was an error trying to use the `paws` package to connect to the AWS API.  This is probably because you have not setup your AWS configuration and credentials.  Please see the 'getting-started' vignette for instructions. Error message:"
                 )
              stop(e)
            }
          )
  cat(crayon::green(" done.\n"))
  
  # use paws to launch the instance; this blocks until the instance is created.
  cat("Launching the EC2 instance...")
  resp <- ec2$run_instances( ImageId                           = imageId
                           , KeyName                           = keyName
                           , SecurityGroupIds                  = as.list(securityGroupIds)
                           , InstanceType                      = instanceType
                           , InstanceInitiatedShutdownBehavior = shutdownBehavior
                           , BlockDeviceMappings               = blockDeviceMappings
                           , TagSpecifications                 = tagSpecifications
                           , MaxCount                          = 1
                           , MinCount                          = 1
                           )
  cat(crayon::green(" done.\n"))
  
  # get the instanceId from the response data
  instanceId <- resp$Instances[[1]]$InstanceId
  cat("The instance id is:", crayon::cyan(instanceId), "\n")
  
  instance <- getInstanceDescription(instanceId, throttle, username)
  
  ## NB: the AWS API describe_instance_status function reports on whether or not
  ## the System and Interface are reachable.  However these checks are very slow
  ## to update and SSH appears to be up and running long before these checks
  ## *actually* pass.  Therefore the currently implementation is to just try
  ## repeated connections with ssh after the state turns to "running" rather
  ## than wait for those tests to pass.  This may be a bad idea.
  
  # check ssh access
  host <- paste0(username, "@", instance$ip)
  Sys.sleep(5)
  cat("Waiting for an ssh connection to:", crayon::cyan(host), "...\n  ")
  sshTimeouts <- 0
  while (!trySSH(host)) {
    if (sshTimeouts == sshTimeout) {
      cat( crayon::red(" failed.\n\n")
         , "Your instance is running but an error occured when trying to connect to it over ssh; be sure to terminate it using 'terminateInstance', the web console or AWS CLI!  It may be that we just need to wait longer for it to get up and running; try increasing the value of 'sshTimeout'.  Also check to make sure that 'sshd' is installed and configured on your AMI and that the firewall is setup corretly through your security group.\n\nYou might try debugging the problem by running ssh in verbose mode with this terminal command:\n", crayon::cyan("ssh -v", host)
         )
      return(instance)
    }
    
    Sys.sleep(throttle)
    sshTimeouts <- sshTimeouts + 1
  }
  
  cat(crayon::green("  done.\n"))
  
  # setup block device mounts
  if (!is.null(blockDevices)) {
    
    cat("Connecting to ssh to format and mount block devices...\n  ")
    session <- ssh::ssh_connect(host)
      lapply( blockDevices
            , function(l) {
                mount <- l$mount
                
                cat( "  Formatting"
                   , crayon::cyan(mount$deviceName)
                   , "as"
                   , crayon::cyan(mount$fsType)
                   , "..."
                   )
                ssh::ssh_exec_wait( session
                                  , paste("sudo mkfs -t", mount$fsType, mount$deviceName)
                                  , std_out = function(...) {}
                                  , std_err = checkSUDOError
                                  )
                cat(crayon::green("  done.\n"))
                
                cat( "  Mounting"
                   , crayon::cyan(mount$deviceName)
                   , "at"
                   , crayon::cyan(mount$mountDir)
                   , "..."
                   )
                ssh::ssh_exec_wait( session
                                  , paste("sudo mkdir", mount$mountDir)
                                  )
                ssh::ssh_exec_wait( session
                                  , paste("sudo mount", mount$deviceName, mount$mountDir)
                                  )
                cat(crayon::green(" done.\n"))
                
                cat( "  Setting ownership of"
                   , crayon::cyan(mount$mountDir)
                   , "to"
                   , crayon::cyan(mount$owner), "..."
                   )
                ssh::ssh_exec_wait( session
                                  , paste("sudo chown", mount$owner, mount$mountDir)
                                  )
                cat(crayon::green(" done.\n"))
                
              }
            )
      
    cat(crayon::green("done.\n"))
    ssh::ssh_disconnect(session)
  }
  
  cat(crayon::green("\nSuccess!"), "Your instance is ready.\n")
  
  instance$name <- tags$Name
  instance
}

trySSH <- function(host) {
  tryCatch( { suppressMessages(session <- ssh::ssh_connect(host))
              ssh::ssh_disconnect(session)
              TRUE
            }
          , error = function(e) { 
              if (grepl("Timeout", e$message, ignore.case = TRUE) | grepl("refused", e$message, ignore.case = TRUE)) {
                return(FALSE)
              } else {
                cat( crayon::red(" failed.\n\n")
                   , "An error occured when trying to connect to your instance over ssh.  If your permissions were denied you may not have your local ssh keys setup corretly.  See the 'getting-started' vignette for instructions.  Try debugging the problem by running ssh in verbose mode:\n\n$ssh -v ", host, "\n\nError message:\n\n"
                   )
                stop(e)
              }
            }
           )
}

checkSUDOError <- function(e) {
  if (grepl("not in the sudoers file", e, fixed = TRUE)) {
    cat(crayon::red(" error."), "\n\nThe logged in user does not have sudo privileges, which is required for this operation.  If you're using Amazon Linux, you can fix this by adding this user to the 'wheel' group from a sudoer account (ec2-user by default).  Error message:\n\n")
    stop(e)
  } else if (grepl("no askpass", e, fixed = TRUE)) {
    cat(crayon::red(" error."), "\n\nThe logged in user is a sudoer but a password must be entered to run sudo. If you are using Amazon Linux, you can give this user the same NOPASSWORD permissions as the default ec2-user by adding a new line to '/etc/sudoers/d/90-cloud-init'.")
    stop(e)
  } else {
    cat(crayon::red(e), "\n\n")
  }
}

#' Gather descriptive information about a running instance
#'
#' This function checks to make sure a launched instance is 'running' then
#' returns an instance description used by the other functions in this package.
#' You won't normally need to call this function directly, but it is useful if
#' you want to launch instances yourself using the Web console, AWS CLI, or paws
#' SDK instead of using \code{\link{launchInstance}}.
#'
#' Note: this function only tests that the instance is running, it does not
#' verify that an ssh connection is corretly setup and available.  You may have
#' to wait awhile after starting your instance with external tools before you
#' can use the other functions in this package.
#'
#' @param instanceId The InstanceID returned or displayed through whatever
#'   mechanism used to launch the EC2 instance
#' @param throttle The amount of time to wait (in seconds) before repeating
#'   requests to the AWS API.  The account-wide limit is 10,000 r/s, so this
#'   default is extremely conservative.
#' @param username The administrator username to use when logging in over ssh.
#'   The user must have sudo privileges. Default is set for Amazon Linux AMIs.
#'
#' @return An \code{instance} object used by the other functions in this package
#' @export
#' 
getInstanceDescription <- function(instanceId, throttle = 1, username = "ec2-user") {

  # wait for the instance State to be running
  cat("Waiting for the instance state to be 'running'...")
  while(!isRunning(instanceId)) {
    Sys.sleep(throttle)
  }
  cat(crayon::green(" done.\n"))

  # Note: this implementation assumes all needed description fields are
  # populated by the time the state turns to "running".  So far so good.
  cat("Waiting for an instance description...")
  resp <- getDescription(instanceId)
  cat(crayon::green(" done.\n"))
  
  list( id   = instanceId
      , ip   = resp$Reservations[[1]]$Instances[[1]]$PublicIpAddress
      , host = paste0(username, "@", resp$Reservations[[1]]$Instances[[1]]$PublicIpAddress)
      )
}

isRunning <- function(instanceId) {
  tryCatch({
      ec2   <- paws::ec2()
      resp  <- ec2$describe_instances(InstanceIds = instanceId)
      state <- resp$Reservations[[1]]$Instances[[1]]$State$Name
      state == "running"
    }
  , error = function(e) {
      cat(crayon::red(" failed.\n\n"), connectionError(instanceId))
      stop(e)
    }
  )
  
}

getDescription <- function(instanceId) {
  tryCatch({
      ec2 <- paws::ec2()
      ec2$describe_instances(InstanceIds = instanceId)
    }
  , error = function(e) { 
      cat(crayon::red(" failed.\n\n"), connectionError(instanceId))
      stop(e)
    }
  )
}

connectionError <- function(instanceId) {
  cat("An error occured when trying to connect to your EC2 instance or check its status.  Please check the instance state using the Web console or AWS-cli as it may still be running; the InstanceId is: "
     , crayon::cyan$bold(instanceId)
     , ".\n\n Error message:"
     )
}

#' Describe one or more block devices (EBS) to create, format, and mount on an
#' EC2 instance
#'
#' The arguments to this function are vectorized, so you can use it to describe
#' one or more block devices to create and mount.  Pay particularly close
#' attention to the \code{deleteOnTermination} argument!  If set incorrectly you
#' may unexpectedly lose data or incur EBS charges.
#'
#' Note the design of this package assumes you are using attached block devices
#' for "scratch" drives during a data analysis run that are freshly provisioned
#' at the start and discarded at the end.  If this doesn't fit your use-case
#' you'll probably want to manually launch and configure your instances and
#' attached volumns.
#'
#' @param deviceName A character vector giving the names of one or more block
#'   devices to create on instance launch.  The length of \code{mountDir} must
#'   match the length of this vector.
#' @param volumeSize A numeric vector giving the size in GB of the volume(s) to
#'   create.
#' @param deleteOnTermination TRUE or FALSE to determine if the volume is
#'   deleted when the associated EC2 instance is terminated. \bold{WARNING #1}:
#'   Even if set to TRUE, the volume will not be deleted if the attached EC2
#'   instance is stopped instead of terminated.  \bold{WARNING #2}:  If you set
#'   this to FALSE than you will continue to incur EBS charges for data on this
#'   volume even after the instance has been terminated.
#' @param volumeType Type of EBS volume: "standard"|"io1"|"gp2"|"sc1"|"st1".
#' @param encrypted TRUE or FALSE whether or not the drive should be encrypted.
#' @param fsType The type of filesystem to use when formatting block devices.
#'   This can be any valid argument to 'mkfs -t' on your AMI.
#' @param mountDir The path on the instance file system where the device(s)
#'   should be mounted after being formatted.  The length of this vector must
#'   math the length of \code{deviceName}.
#' @param owner The user and group owners that should be set for the block
#'   device mount location. This string should be in the standard Linux format
#'   of "user:group".
#'
#' @return A list of block device mappings that can be used with
#'   \code{\link{launchInstance}}.
#' @export
#' 
blockDevices <- function( deviceName          = "/dev/xvdf"
                        , volumeSize          = 8
                        , deleteOnTermination = TRUE
                        , volumeType          = "gp2"
                        , encrypted           = FALSE
                        , fsType              = "ext4"
                        , mountDir            = "/data"
                        , owner               = "ec2-user:ec2-user"                
                        ) {
  
  if (length(deviceName) != length(mountDir)) { 
    stop("Length of deviceName and mountDir vectors do not match.") 
  }
  
  mapply( function( deviceName
                  , volumeSize
                  , deleteOnTermination
                  , volumeType
                  , encrypted
                  , fsType
                  , mountDir
                  , owner
                  ) { 
          list( mapping = list( DeviceName = deviceName
                              , Ebs = list( DeleteOnTermination = deleteOnTermination
                                          , VolumeSize          = volumeSize
                                          , VolumeType          = volumeType
                                          , Encrypted           = encrypted
                                          )
                              )
              , mount   = list( deviceName = deviceName
                              , fsType     = fsType
                              , mountDir   = mountDir
                              , owner      = owner
                              )
              )
          }
        , deviceName
        , volumeSize
        , deleteOnTermination
        , volumeType
        , encrypted
        , fsType
        , mountDir
        , owner
        , SIMPLIFY   = FALSE
        , USE.NAMES  = FALSE
        )
  
}

#' Start jobs by executing a launch script on an EC2 instance
#'
#' This function copies your individual job scripts and the launch script to an
#' EC2 instance, flags them as executable, and then runs them.
#'
#' @param instance An \code{instance} objected created with
#'   \code{\link{launchInstance}} or \code{\link{getInstanceDescription}}.
#' @param scripts A list with launch and job script file names and a script path
#'   created by \code{\link{makeLaunchScript}}
#' @param otherFiles NULL or a vector containing full paths to other files to be
#'   copied to the instance before the launch script is run.
#' @param permissions Specification for how the executable bit should be set
#'   after scripts are copied to the instance.  By default this marks the scrips
#'   as executable by both the owning user and group.
#'
#' @return The \code{instance} object.
#' @export
#' 
launchJobs <- function( instance
                      , scripts
                      , otherFiles   = NULL
                      , permissions  = "ug+x"
                      ) {

  localPaths  <- file.path(scripts$localPath, c(scripts$jobs, scripts$launch))
  remotePaths <- file.path(scripts$remotePath, c(scripts$jobs, scripts$launch), fsep = "/")

  cat("Opening ssh connection to instance...\n  ")
  session <- ssh::ssh_connect(instance$host)
  cat(crayon::green("  done.\n"))
  
  cat("Copying scripts to the instance...\n")
  ssh::scp_upload( session
                 , files   = localPaths
                 , to      = scripts$remotePath
                 , verbose = FALSE
                 )
  cat(crayon::green(" done.\n"))
  
  if (!is.null(otherFiles)) {
    cat("Copying other files to the instance...")
    ssh::scp_upload( session
                   , files   = otherFiles
                   , to      = scripts$remotePath
                   , verbose = FALSE
                   )
    cat(crayon::green(" done.\n"))  
  }
  
  cat("Making scripts executable...")
  ssh::ssh_exec_wait( session
                    , paste( "sudo chmod"
                           , permissions
                           , paste(remotePaths, collapse = " ")
                           , std_err = checkSUDOError
                           ) 
                    )
  cat(crayon::green(" done.\n"))
  
  cat("Running launch script...")
  ssh::ssh_exec_wait(session, paste("cd", scripts$remotePath))
  
  ssh::ssh_exec_wait( session
                    , paste0( "nohup ./"
                            , scripts$launch
                            , ' >launch.out 2>&1 &\n_pid=$!\necho "$_pid" >> launchers.pid'
                            )
                    )
  cat(crayon::green(" done.\n"))
    
  cat("You can check the status of your jobs using 'getStatus'.")
  
  instance$jobPaths <- if (is.null(instance$jobPaths)) {
    scripts$remotePath
  } else {
    unique(c(instance$jobPaths, scripts$remotePath))
  }
  
  ssh::ssh_disconnect(session)
  
  instance
  
}

#' Check whether or not an EC2 instance is running.
#'
#' @param instance An instance objected created with
#'   \code{\link{launchInstance}} or \code{\link{getInstanceDescription}}.
#'
#' @return TRUE or FALSE.  All conditions except the "running" state return
#'   FALSE (including AWS API and connection errors).
#' @export
#' 
checkAlive <- function(instance) {
  tryCatch( isRunning(instance$id)
          , error = function(e) { FALSE }
          )
}

#' Get the status of an EC2 instance.
#'
#' This function requires the \code{top} and \code{mpstat} programs on the
#' instance.
#'
#' @param instance An instance objected created with
#'   \code{\link{launchInstance}} or \code{\link{getInstanceDescription}}.
#' @param session Optionally provide an existing ssh session to the instance
#'   created with ssh::ssh_connect.
#'
#' @return A list containing two named elemements: \code{jobs} a data.frame
#'   giving the status of all jobs launched on the instance and \code{system} a
#'   list containing system summary information.
#' @export
#' 
getStatus <- function(instance, session = NULL) {
  
  if (is.null(session)) {
    cat("Opening ssh connection to instance...\n  ")
    session <- ssh::ssh_connect(instance$host)
    cat(crayon::green("  done.\n"))
  }
  
  # run top on the instance
  tresp    <- ssh::ssh_exec_internal(session, "top -bS -n 1")
  
  # parse top process table
  topS     <- gsub(pattern = "rs:main Q:R+", replacement = "rs:main_Q:R+", x = rawToChar(tresp$stdout)) # I've got no clue what's up with this.
  topLines <- strsplit(topS, split = "\n", fixed = TRUE)[[1]]
  
  topTable <- read.table( text = paste0( topLines[7:length(topLines)]
                                       , collapse = "\n"
                                       )
                        , header           = TRUE
                        , stringsAsFactors = FALSE
                        )
  
  # if jobs have been launched collect associated pids
  if (!is.null(instance$jobPaths)) {
    procTables <- lapply( instance$jobPaths
                          , function(path) {
                            jresp <- ssh::ssh_exec_internal( session
                                                           , paste0("cat ", path, "/jobs.pid")
                                                           )
                            lresp <- ssh::ssh_exec_internal( session
                                                           , paste0("cat ", path, "/launchers.pid")
                                                           ) 
                            
                            jobids    <- strsplit( rawToChar(jresp$stdout)
                                                 , split = "\n"
                                                 )[[1]]
                            launchids <- strsplit( rawToChar(lresp$stdout)
                                                 , split = "\n"
                                                 )[[1]]
                            
                            data.frame( pid  = as.numeric(c(jobids, launchids))
                                      , type = c( rep("job", times = length(jobids))
                                                , rep("launch", times = length(launchids))
                                                )
                                      , path = path
                                      , stringsAsFactors = FALSE
                                      )
                          }
    )
    
    procTable <- do.call(rbind, args = procTables)
    
    procTable$finished <- !(procTable$pid %in% topTable$PID)
    procTable <- dplyr::left_join(procTable, topTable, by = c("pid" = "PID"))
    
    colnames(procTable)[5:15] <- c("user", "priority", "nice", "virtual memory (KB)", "residual memory (KB)", "shared memory (KB)", "status", "% CPU", "% memory", "CPU time", "command")
    procTable$status <- c(D = "uninteruptable sleep", R = "running", S = "sleeping", T = "traced or stopped", Z = "zombie")[procTable$status]
  
  } else {
    procTable <- NA
  }
  
  # parse top summary lines
  topHead <- topLines[1:5]

  parseCPU <- function(t) {
    m <- regexpr(paste0("\\d+\\.\\d+\\s", t), topHead[3])
    substring(topHead[3], m, m + attr(m, 'match.length') - 4)
  }
  
  us <- parseCPU("us")
  sy <- parseCPU("sy")
  id <- parseCPU("id")
  
  parseMEM <- function(t) {
    m <- regexpr(paste0("\\d+\\s", t), topHead[4])
    substring(topHead[4], m, m + attr(m, 'match.length') - nchar(t) - 2)
  }
  
  total <- parseMEM("total")
  used  <- parseMEM("used")
  free  <- parseMEM("free")
  
  # run mpstat on the instance
  mresp <- ssh::ssh_exec_internal(session, "mpstat -P ALL 1 1")
  mpS   <- rawToChar(mresp$stdout)
  cpuS  <- strsplit(mpS, "\n\n")[[1]][2]
  cpu   <- read.table( text             = strsplit(cpuS, "\n")[[1]]
                     , header           = TRUE
                     , stringsAsFactors = FALSE
                     )
  colnames(cpu)[1] <- "time"
  cpu$time <- Sys.time()
  cpu$PM <- NULL
  
  list( jobs   = procTable
      , system = list( cpu.us    = as.numeric(us)
                     , cpu.sy    = as.numeric(sy)
                     , cpu.id    = as.numeric(id)
                     , mem.total = as.numeric(total)
                     , mem.used  = as.numeric(used)
                     , mem.free  = as.numeric(free)
                     , mpstat    = cpu 
                     )
      )
  
}

#' Wait for jobs on an instance to finish
#'
#' This function blocks the current thread of execution until all of the jobs on
#' the given instance have finished running.
#'
#' @param instance An instance objected created with
#'   \code{\link{launchInstance}} or \code{\link{getInstanceDescription}}.
#' @param wait The time (in seconds) to wait between checking on the state of jobs
#'   running on the instance.
#'
#' @return Invisibly returns the \code{instance} object.
#' @export
#'
waitForJobs <- function(instance, wait = 10) {
  
  cat("Opening ssh connection to instance...\n  ")
  session <- ssh::ssh_connect(instance$host)
  cat(crayon::green("  done.\n"))
  
  cat("Waiting for all jobs to finish...")
  status <- getStatus(instance, session)
  while (!all(status$jobs$finished)) {
    Sys.sleep(wait)
    status <- getStatus(instance, session)  
  }
  cat(crayon::green(" done.\n"))
  
  ssh::ssh_disconnect(session)
  
  invisible(instance)
  
}

#' Terminate an instance
#'
#' Terminates the instance. \bold{WARNING}: any block devices setup with
#' \code{deleteOnTermination = TRUE} will be deleted and ALL data LOST!  A
#' termianted instance cannot be restarted.
#'
#' @param instance An instance objected created with
#'   \code{\link{launchInstance}} or \code{\link{getInstanceDescription}}.
#'
#' @return Invisibly returns the \code{instance} object.
#' @export
#' 
terminateInstance <- function(instance) {
  ec2  <- paws::ec2()
  resp <- ec2$terminate_instances(InstanceIds = instance$id)
  cat("Instance state: ", crayon::cyan(resp$TerminatingInstances[[1]]$CurrentState$Name), "\n")
  
  invisible(instance)
}

#' Stop a running instance
#'
#' Stops the instance. \bold{WARNING}: ALL block devices associated with the
#' instance will continue to persist and incure storage charges even when the
#' instance is stopped.  Use \code{\link{terminateInstance}} to release
#' ephermeral block storage.  You can restart a stopped instance with
#' \code{\link\{startInstance}}.
#'
#' @param instance An instance objected created with
#'   \code{\link{launchInstance}} or \code{\link{getInstanceDescription}}.
#'
#' @return The Instance ID for the stopped instance.
#' @export
#'
stopInstance <- function(instance) {
  cat("Stopping instance...")
  ec2  <- paws::ec2()
  resp <- ec2$stop_instances(InstanceIds = instance$id)
  cat("Instance state: ", crayon::cyan(resp$TerminatingInstances[[1]]$CurrentState$Name), "\n")
  cat("Instance ID:", crayon::cyan(instance$id))
  instance$id
  
}

#' Starts a stopped instance
#'
#' Starts a stopped instance and waits for the instance to start running and for
#' ssh access to be available.
#'
#' @param instanceId The Instance ID for the stopped instance to start.
#' @param throttle The amount of time to wait (in seconds) before repeating
#'   requests to the AWS API.  The account-wide limit is 10,000 r/s, so this
#'   default is extremely conservative.
#' @param username The administrator username to use when logging in over ssh.
#'   The user must have sudo privileges. Default is set for Amazon Linux AMIs.
#' @param sshTimeout How many times to re-test connecting over ssh after
#'   recieving a timeout or connection refused error.  This function waits the
#'   duraction of 'throttle' between attempts.
#'
#' @return An \code{instance} object that can be used with the other functions
#'   in this package.
#' @export
#'
startInstance <- function( instanceId
                         , throttle   = 1
                         , username   = "ec2-user"
                         , sshTimeout = 10
                         ) {
  
  cat("Starting instance...")
  ec2  <- paws::ec2()
  ec2$start_instances(InstanceIds = instanceId)
  cat(crayon::green(" done.\n"))
  
  instance <- getInstanceDescription(instanceId, throttle, username)
  
  # check ssh access
  host <- paste0(username, "@", instance$ip)
  Sys.sleep(5)
  cat("Waiting for an ssh connection to:", crayon::cyan(host), "...\n  ")
  sshTimeouts <- 0
  while (!trySSH(host)) {
    if (sshTimeouts == sshTimeout) {
      cat( crayon::red(" failed.\n\n")
         , "Your instance is running but an error occured when trying to connect to it over ssh; be sure to terminate it using 'terminateInstance', the web console or AWS CLI!  It may be that we just need to wait longer for it to get up and running; try increasing the value of 'sshTimeout'.  Also check to make sure that 'sshd' is installed and configured on your AMI and that the firewall is setup corretly through your security group.\n\nYou might try debugging the problem by running ssh in verbose mode with this terminal command:\n", crayon::cyan("ssh -v", host)
         )
      return(instance)
    }
    
    Sys.sleep(throttle)
    sshTimeouts <- sshTimeouts + 1
  }
  
  cat(crayon::green("  done.\n"))
  cat(crayon::green("\nSuccess!"), "Your instance is ready.\n")
  
  instance
  
}
