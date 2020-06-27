# AWSTools

This package contains a set of R tools designed to ease and automate deploying data analysis projects to AWS EC2 instances.

The goal here is to provide a highly simplified workflow for straightforward projects.  See the `paws` package for a nearly complete AWS SDK in R or something like [Slurm](https://github.com/SchedMD/slurm) for a full featured cluster job scheduler.

# Dependencies

Functions that make system calls assume a standard local Linux environment but should work fine through Bash on Windows and *may* work on MacOS, depending on what tools are installed.  This package also assumes that you have setup your AWS configuration and credentials files (for example using the [aws-cli](https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-configure.html) with: `aws configure`). Finally, these tools have only been tested with instances running AWS Linux 2.  YMMV if any of that isn't true.

# Installation

To install from GitHub first make sure you have the `devtools` package installed, then run:

```
devtools::install_github('whitwort/AWSTools')
```

# License

Copyright © 2020 Gregg Whitworth and licensed under [GPLv2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html).
