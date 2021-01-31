[![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0) 

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) 
[![Travis CI](https://travis-ci.com/bnasr/wiad.svg?branch=master)](https://travis-ci.com/bnasr/wiad) 
[![Coverage status](https://codecov.io/gh/bnasr/wiad/branch/master/graph/badge.svg)](https://codecov.io/gh/bnasr/wiad)

[![CRAN status](http://www.r-pkg.org/badges/version-last-release/wiad)](https://cran.r-project.org/package=wiad) 
[![Downloads](http://cranlogs.r-pkg.org/badges/wiad?color=brightgreen)](http://www.r-pkg.org/pkg/wiad) 
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/wiad?color=brightgreen)](http://www.r-pkg.org/pkg/wiad) 


# WIAD: Wood Image Analysis and Dataset

The Wood Image Analysis and Dataset is a dynamic interface to extract, store and share data from images of tree rings, such as annual ring width. 

WIAD presents a dynamic interface for a wide range of scientists in archeology, ecology, geology, climatology and chronology disciplines to digitise and analyze their tree ring imagery easily, back up results and share them with collaborators and the public.


# Installation

### Install from CRAN
The WIAD R package has been published on The Comprehensive R Archive Network (CRAN). The latest tested WIAD package can be installed from the <a href="https://cran.r-project.org/package=wiad">CRAN packages repository</a> by running the following command in an R environment:

```{r, echo=TRUE, eval=FALSE}

utils::install.packages('wiad', repos = "https://cran.us.r-project.org" )

```

### Install from GitHub
Alternatively, the latest beta release of WIAD can be directly downloaded and installed from the [GitHub repository](https://github.com/bnasr/wiad):

```{r, echo=TRUE, eval=FALSE}
# install devtools first
if(!require(devtools)) install.packages('devtools')

# installing the package from the GitHub repo
devtools::install_github('bnasr/wiad')

# loading the package
library(wiad)
```


# Usage
The interactive mode can be launched from an interactive R environment by the following command.

```{r, echo=TRUE, eval=FALSE}

library(wiad)
Launch()

```

or

```{r, echo=TRUE, eval=FALSE}

wiad::Launch()

```

or form the command line (e.g., shell in Linux, Terminal in macOS and Command Prompt in Windows machines) where an R engine is already installed by:

```{r, echo=TRUE, eval=FALSE}

Rscript -e "wiad::Launch(Interactive = TRUE)"

```

Calling the `Launch` function opens up the WIAD app in the system’s default web browser.
