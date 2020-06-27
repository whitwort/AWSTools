#' Launch an EC2 instance
#'
#' This wrapper function launches an EC2 instance from an AMI image ID, monitors
#' instance initialization and startup, and handles mounting any additional
#' block devices at the specified locations.  It returns an \code{instance}
#' object which is used by other functions in this package to communicate with
#' the instance.
#'
#' This function is designed to be as simple as possible and does not support
#' the vast majority of configuration options available when launching EC2
#' instances.  If it doesn't meet your needs see the \code{paws} package for a
#' full featured EC2 API.  You can pass the return value from the
#' \code{paws.compute} \code{run_instances} function to the
#' \code{\link{parseLaunchResponse}} to get an \code{instance} object to use
#' with the other functions in this package.
#'
#' @param imageID The image-id for the AMI used to launch the instance.
#' @param keyName The key-pair on your AWS account to use.  This pair should be
#'   associated with one of the default public keys on the current account that
#'   \code{ssh} will try to use when connecting to your instance.
#' @param securityGroups The EC2 security group names to use.  This function
#'   launches your instance with the EC2-Classic scheme, not using VPC.
#' @param instanceType The type of instance to launch.
#' @param tags Tags to apply to your instance and its resources. This should
#'   either be NULL or a normal R list with named elements; names will be tag
#'   Keys and elements their Values.
#' @param blockDeviceMapping Additional block devices to create and mount. This
#'   should either be NULL or an object returned by the
#'   \code{\link{blockDeviceMapping}} function.
#'
#' @return An \code{instance} object that can be used with other functions in
#'   this package that need to communicate with your launched instance.
#' @export
#' 
launchInstance <- function( imageID
                          , keyName
                          , securityGroups
                          , instanceType       = "t2.micro"
                          , tags               = NULL
                          , blockDeviceMapping = NULL
                          ) {
  
}

#' Describe one or more block devices to create and mount on an EC2 instance.
#'
#' The arguments to this function are vectorized, so you can use it to describe
#' one or more block devices to create and mount.
#'
#' @param deviceName A character vector giving the names of one or more block
#'   devices to create on instance launch.  The length of \code{mountDir} must
#'   match the length of this vector.
#' @param volumeSize A numeric vector giving the size in GB of the volume(s) to
#'   create.
#' @param deleteOnTermination TRUE or FALSE to determine if the volume is
#'   deleted when the associated EC2 instance is terminated (NOTE: the volume
#'   will NOT be deleted if the EC2 instance is stopped instead of terminated).
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
#'   \code{\link{launchInstance}}.
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
#'   \code{\link{launchInstance}}.
#'
#' @return TRUE or FALSE
#' @export
#'
checkAlive <- function(instance) {
  
}

#' Get the status of jobs launched on an EC2 instance.
#'
#' @param instance 
#'
#' @return
#' @export
#'
#' @examples
getStatus <- function(instance) {
  
}

#' Parses an AWS run-instances response into an instance object
#'
#' This function converts the return value from the \code{paws.compute::ec2}
#' \code{run_instances} function into an \code{instance} object used by the
#' other functions in this package.  Normally you would never have to call this
#' function directly, but it is available if you need to create instances using
#' \code{paws.compute::ec2}.
#'
#' @param resp The response data structure returned by EC2 run-instances.
#'
#' @return An \code{instance} object that can be used with the other functions
#'   in this package.
#' @export
#'
parseLaunchResponse <- function(resp) {
  
}
