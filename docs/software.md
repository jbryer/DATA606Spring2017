---
layout: page
title: Software
permalink: /software/
---

We will make use of [R](http://r-project.org), an open source statistics program and language. Be sure to [install R](http://cran.r-project.org/) and [RStudio](http://rstudio.com) on your own computers within the first few days of the class.

* R - [Windows](http://cran.r-project.org/bin/windows/base/) or [Mac](http://cran.r-project.org/bin/macosx/)
* RStudio - [Download Windows or Mac version from here](http://www.rstudio.com/products/rstudio/download/)
* LaTeX - [Windows use MiKTeX](http://miktex.org/) or [Mac use BasicTeX](http://www.tug.org/mactex/morepackages.html) (it is best to use Safari to download this file as Chrome or Firefox will often fail)

If using Windows, you also need to download and install these:

* [RTools](http://cran.r-project.org/bin/windows/Rtools/)
* [ActivePerl](http://www.activestate.com/activeperl/downloads/thank-you?dl=http://downloads.activestate.com/ActivePerl/releases/5.16.3.1603/ActivePerl-5.16.3.1603-MSWin32-x86-296746.msi)

Once everything is installed, execute the following command in RStudio to install the packages we will use for this class (you can copy-and-paste):

```
install.packages(c('openintro','OIdata','devtools','ggplot2','psych','reshape2',
				   'knitr','markdown','shiny'))
devtools::install_github("jbryer/DATA606")
```

## The `DATA606` R Package

Many of the course resouces are available in the `DATA606` R package. Here are some command to get started:

```
library('DATA606')          # Load the package
vignette(package='DATA606') # Lists vignettes in the DATA606 package
vignette('os3')             # Loads a PDF of the OpenIntro Statistics book
data(package='DATA606')     # Lists data available in the package
getLabs()                   # Returns a list of the available labs
viewLab('Lab0')             # Opens Lab0 in the default web browser
startLab('Lab0')            # Starts Lab0 (copies to getwd()), opens the Rmd file
shiny_demo()                # Lists available Shiny apps
```

