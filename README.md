# AWSTools

This package contains a set of R tools designed to ease and automate deploying data analysis projects to AWS EC2 instances.

# Limitations

The goal for this package is to provide a highly simplified workflow for straightforward projects.  See the [paws](https://github.com/paws-r/paws) package if you need a much more complete AWS SDK for R.  These tools don't handle resource management or load balancing between instances so it's only suitable for projects where resource needs are easy to determine ahead of time; see a project like [Slurm](https://github.com/SchedMD/slurm) if you need a fully featured job scheduler to manage a compute cluster.

# Dependencies

Many of the functions in this package make system calls and assume a standard Linux environment on the local machine but should work fine through a Bash on Windows distributions and *may* work on MacOS, depending on what tools are installed. 

Finally, these tools have been developed and tested on EC2 instances running [Amazon Linux 2](https://aws.amazon.com/amazon-linux-2/), but should work with any standard Linux distribution.

# Installation & Setup

To install from GitHub first make sure you have the `devtools` package installed, then run:

```r
devtools::install_github('whitwort/AWSTools')
```

To use the tools in this package you will need to configure your AWS user credentials files (or set environment variables).  As an example, you can do this using the [aws-cli](https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-configure.html) `aws configure` command.  You will also need to make sure that the `ssh` key-pair you use to launch your EC2 instance is among the default keys that `ssh` will try when attempting to connect to the instance.

# License

Copyright © 2020 Gregg Whitworth and licensed under [GPLv2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html).
