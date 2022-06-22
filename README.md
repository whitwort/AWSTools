# AWSTools

This package contains a set of R tools designed to ease and automate deploying data analysis projects to AWS EC2 instances.

## Getting started

To install this package from GitHub first make sure you have the `devtools` package installed, then run:

```r
devtools::install_github('whitwort/AWSTools', build_vignettes = TRUE)
```

Now take a look at the [Getting started tutorial](https://htmlpreview.github.io/?https://github.com/whitwort/AWSTools/blob/master/vignettes/getting-started.html), or open the vignette in R:

```r
library(AWSTools)
vignette('getting-started')
```

This tutorial will walk you through setting up access to the AWS API, ssh keys, and a sample workflow.

## Limitations

The goal for this package is to provide a highly simplified workflow for straightforward data analysis projects.  See the [paws](https://github.com/paws-r/paws) package if you need a much more thorough AWS SDK for R.  

These tools don't handle resource management or load balancing between instances so they are only suitable for projects where resource needs are easy to predict ahead of time. See a project like [Slurm](https://github.com/SchedMD/slurm) if you need a full featured job scheduler to manage a compute cluster.

Finally, this toolset has been developed and tested on EC2 instances running [Amazon Linux 2](https://aws.amazon.com/amazon-linux-2/), but it *should* work with any standard Linux distribution.

## License

Copyright © 2022 Gregg Whitworth and licensed under [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html).
