#' Launch an EC2 instance
#'
#' This wrapper function launches an EC2 instance from an AMI image ID, monitors
#' instance initialization and startup, and handles mounting any additional
#' block devices at the specified locations.  It returns an \code{instance}
#' object which is used by other functions in this package
#'
#' This function is designed to be as simple as possible and does not support
#' the vast majority of configuration options available when launching EC2
#' instances.  If it doesn't meet your needs see the \code{paws} package for a
#' full featured EC2 SDK  You can use the \code{paws::ec2()$run_instances}
#' function to launch your instance and then pass the `InstanceID` to
#' \code{link{getInstanceDescription}} function to generate an \code{instance}
#' object to use with the other functions in this package.
#'
#' @param imageId The image-id for the AMI used to launch the instance.
#' @param keyName The key-pair on your AWS account to use.  This pair should be
#'   associated with one of the default public keys on the current account that
#'   \code{ssh} will try to use when connecting to your instance.
#' @param securityGroupIds The EC2 security group(s) Id(s) to use.
#' @param instanceType The type of instance to launch.
#' @param tags Tags to apply to your instance and its resources. This should
#'   either be NULL or a normal R list with named elements; names will be tag
#'   Keys and elements their Values.
#' @param blockDeviceMapping Additional block devices to create and mount. This
#'   should either be NULL or an object returned by the
#'   \code{\link{blockDeviceMapping}} function.
#' @param throttle The amount of time to wait (in seconds) before repeating
#'   requests to the AWS API.  The account-wide limit is 10,000 r/s, so this
#'   default is extremely conservative.
#' @param username The administrator username to use when logging in over ssh.
#'   Default is set for Amazon Linux AMIs.
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
                          , tags               = NULL
                          , blockDeviceMapping = NULL
                          , throttle           = 1
                          , username           = "ec2-user"
                          , sshTimeout         = 10
                          ) {
  
  # reformat tag specification
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
  
  # get the paws::EC2 service
  ec2  <- paws::ec2()
  
  # check that AWS API access is configured
  cat("Checking AWS API access to EC2...")
  tryCatch( ec2$describe_instances
          , error = function(e) {
              cat( crayon::red(" failed.\n")
                 , "There was an error trying to use the `paws` package to connect to the AWS API.  This is probably because you have not setup your AWS configuration and credentials.  Please see the 'configuration' vignette for instructions. Error message:"
                 )
              stop(e)
            }
          )
  cat(crayon::green(" done.\n"))
  
  # use paws to launch the instance; this blocks until the instance is created.
  cat("Launching EC2 instance...")
  resp <- ec2$run_instances( ImageId = imageId
                           , KeyName = keyName
                           , SecurityGroupIds = as.list(securityGroupIds)
                           , InstanceType = instanceType
                           , TagSpecifications = tagSpecifications
                           , MaxCount = 1
                           , MinCount = 1
                           )
  cat(crayon::green(" done.\n"))
  
  # get the instanceId from the response data
  instanceId <- resp$Instances[[1]]$InstanceId
  cat("The instance id is: ", crayon::cyan(instanceId), "\n")
  
  instance <- getInstanceDescription(instanceId, throttle)
  
  ## NB: the AWS API describe_instance_status function reports on whether or not
  ## the System and Interface are reachable.  However these checks are slow to
  ## update and SSH appears to be up and running long before these checks
  ## *actually* pass.  Therefore the currently implementation is to just try
  ## repeated connections with ssh after the state turns to "running" rather
  ## than wait for those tests to pass.  This may be a bad idea.
  
  # check ssh access
  host <- paste0(username, "@", instance$ip)
  Sys.sleep(5)
  cat("Checking ssh connection (", crayon::cyan(host), ")...\n")
  sshTimeouts <- 0
  while (!trySSH(host)) {
    if (sshTimeouts == sshTimeout) {
      cat( crayon::red(" failed.\n\n")
         , "Your instance is running but an error occured when trying to connect to it over ssh; be sure to terminate it using 'terminateInstance', the web console or AWS CLI!  It may be that we just need to wait longer for your server to get up and running; try increasing the value of 'sshTimeout'.  Also check to make sure that 'sshd' is installed and configured on your AMI and that the firewall is setup corretly through your security group.\n\nYou might have success debugging the problem by running ssh in verbose mode.  Try running this terminal command:\nssh -v ", host
         )
      return(instance)
    }
    
    Sys.sleep(throttle)
    sshTimeouts <- sshTimeouts + 1
  }
  
  cat(crayon::green("done.\n"))
  cat(crayon::green("\nSuccess!"), " Your instance is now ready to use.\n")
  
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
                   , "An error occured when trying to connect to your instance over ssh.  If your permissions were denied you may not have your local ssh keys setup corretly.  See the 'configuration' vignette for instructions.  Try debugging the problem by running ssh in verbose mode:\n\n$ssh -v ", host, "\n\nError message:\n\n"
                   )
                stop(e)
              }
            }
           )
}

#' Gather descriptive information about a running instance
#'
#' This function checks to make sure a launched instance is Running and both the
#' System and Instance reachability tests have passed, then returns an instance
#' description used by the other functions in this package.  This function is
#' useful if you want to launch instances yourself using the Web console, AWS
#' CLI, or paws SDK instead of using \code{\link{launchInstance}}.
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
#'
#' @return An \code{instance} object used by the other functions in this package
#' @export
#' 
getInstanceDescription <- function(instanceId, throttle = 1) {

  # wait for the instance State to be running
  cat("Waiting for the instance to be Running...")
  while(!isRunning(instanceId)) {
    Sys.sleep(throttle)
  }
  cat(crayon::green(" done.\n"))

  # The current setup assumes all needed description fields are populated by the
  # time the state turns to "running".  So far so good.
  cat("Getting instance description...")
  resp <- getDescription(instanceId)
  cat(crayon::green(" done.\n"))
  
  list( id = instanceId
      , ip = resp$Reservations[[1]]$Instances[[1]]$PublicIpAddress
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
  cat("An error occured when trying to connect to your EC2 instance or check its status.  Please check the instance state using the Web console or AWS-cli as it may still be running; the instance InstanceId is ", crayon::cyan$bold(instanceId), ". Error message:")
}

#' Describe one or more block devices (EBS) to create and mount on an EC2 instance.
#'
#' The arguments to this function are vectorized, so you can use it to describe
#' one or more block devices to create and mount.  Pay particular care to the
#' \code{deleteOnTermination} argument!  If set incorrectly you may unexpectedly
#' lose data or incur EBS charges.
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
#' @param mountDir The path on the instance file system where the device(s)
#'   should be mounted after the instance starts up.  The length of this vector
#'   must math the length of \code{deviceName}.
#'
#' @return A list of block device mappings that can be used with
#'   \code{\link{launchInstance}}.
#' @export
#' 
blockDeviceMapping <- function( deviceName          = "/dev/sdb"
                              , volumeSize          = 8
                              , deleteOnTermination = TRUE
                              , mountDir            = "/data"
                              ) {
  
  if (length(deviceName) != length(mountDir)) { 
    stop("Length of deviceName and mountDir vectors do not match.") 
  }
  
  mapply( function(deviceName, volumeSize, deleteOnTermination, mountDir) { 
            list( deviceName          = deviceName
                , volumeSize          = volumeSize
                , deleteOnTermination = deleteOnTermination
                , mountDir            = mountDir
                )
          }
        , deviceName
        , volumeSize
        , deleteOnTermination
        , mountDir
        )
  
}


#' Start jobs by executing a launch script on an EC2 instance.
#'
#' This function copies your individual job scripts and the launch script to an
#' EC2 instance, flags them as executable, and then runs them.
#'
#' @param instance An instance objected created with
#'   \code{\link{launchInstance}} or \code{\link{getInstanceDescription}}.
#' @param launchScript The name of the launch script file created by
#'   \code{\link{makeLaunchScript}}.
#' @param scriptPath Path to the folder where jobs scripts and the launch script
#'   were created with \code{\link{makeJobScripts}} and
#'   \code{\link{makeLaunchScript}}.
#'
#' @export
#' 
launchJobs <- function( instance
                      , launchScript = "launch.sh"
                      , scriptPath   = "rendered-scripts"
                      ) {
  
}

#' Check whether or not an EC2 instance is still running
#'
#' @param instance An instance objected created with
#'   \code{\link{launchInstance}} or \code{\link{getInstanceDescription}}.
#'
#' @return TRUE or FALSE.  All conditions except the "running" state return
#'   FALSE (including AWS API and connection errors).
#' @export
#' 
checkAlive <- function(instance) {
  tryCatch( isRunning(instance)
          , error = function(e) { FALSE }
          )
}

#' Get the status of jobs launched on an EC2 instance.
#'
#' @param instance An instance objected created with
#'   \code{\link{launchInstance}} or \code{\link{getInstanceDescription}}.
#'
#' @return
#' @export
#'
getStatus <- function(instance) {
  
}

#' Terminate instance
#'
#' Terminates the instance.  NOTE: any block devices setup with
#' \code{deleteOnTermination = TRUE} will be deleted and ALL data LOST!
#'
#' This package doesn't offer an API for stopping and resuming instances, only
#' terminating them.  If you want to manually stop and start instances, you can
#' get a new \code{instance} object to use with the other functions in this
#' package by passing the instanceId to \code{\link{getInstanceDescription}}
#' function.
#' 
#' @param instance An instance objected created with
#'   \code{\link{launchInstance}} or \code{\link{getInstanceDescription}}.
#'
#' @export
#' 
terminateInstance <- function(instance) {
  ec2  <- paws::ec2()
  resp <- ec2$terminate_instances(InstanceIds = instance$id)
  cat("Instance state: ", crayon::cyan(resp$TerminatingInstances[[1]]$CurrentState$Name), "\n")
}
