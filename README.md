# AWSTools

This package contains a set of R tools designed to ease and automate deploying data analysis projects to AWS EC2 instances.

## Installation & configuration

To install this package from GitHub first make sure you have `devtools` installed, then run:

```r
devtools::install_github('whitwort/AWSTools', build_vignettes = TRUE)
```

To use this package you will need to configure your AWS user credentials.  As an example, you can do this using the [aws-cli](https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-configure.html) `aws configure` command.  Please see the `paws` credentials documentation for more options (**note**: using a custom service config is not currently compatible with this package).

You will also need to make sure that the `ssh` key-pair you use to launch your EC2 instance is among the default keys that `ssh` will try when attempting to connect to the instance.

For more detailed configuration instructions see the [configuration](https://github.com/whitwort/AWSTools/blob/master/vignettes/configuration.Rmd) vignette.

## Limitations

The goal for this package is to provide a highly simplified workflow for straightforward projects.  See the [paws](https://github.com/paws-r/paws) package if you need a much more thorough AWS SDK for R.  

These tools don't handle resource management or load balancing between instances so they are only suitable for projects where resource needs are easy to determine ahead of time. See a project like [Slurm](https://github.com/SchedMD/slurm) if you need a full featured job scheduler to manage a compute cluster.

Finally, this toolset has been developed and tested on EC2 instances running [Amazon Linux 2](https://aws.amazon.com/amazon-linux-2/), but it *should* work with any standard Linux distribution.  It is not designed to work with Windows instances.

## License

Copyright © 2020 Gregg Whitworth and licensed under [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html).
