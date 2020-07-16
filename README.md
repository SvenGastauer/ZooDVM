# ZooDVM
Zooglider Zonar DVM

## Copyright and Licence  


    Copyright (C) 2020 Sven Gastauer, Mark D. Ohman.
    
    This file is part of ZooDVM.
    
    ZooDVM is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    ZooDVM is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with ZooScatR.  If not, see <http://www.gnu.org/licenses/>.
    This package is open for community development and we encourage users to extend the package as they need. We are not liable for any losses when using ZooDVM.
  

# Installation
ZooDVM can be instaled from binary or source files found [here](https://github.com/SvenGastauer/ZooDVM) and will be updated sporadically, sometimes, or frequenctly.

ZooDVM can be installed from GitHub. This requires the [devtools](https://cran.r-project.org/web/packages/devtools/index.html) package.

WIndows users: For devtools and other R tools to function correctly it is recommended to have the latest version of [Rtools](https://cran.r-project.org/bin/windows/Rtools/) installed along a recent verison of [R (>4.0)](https://cran.r-project.org/).

``` r
# The package can be installed from Github through devtools. Let's first check if devtools is available, otherwise install it and in any case load the library:
if('devtools' %in% installed.packages()){
  message(Sys.time(), ': devtools is already available, no further action required. The install version is ',packageVersion('devtools'))
}else{
  install.packages('devtools',dependencies = TRUE, INSTALL_opts = '--no-lock')
}
library(devtools)

#Install the package from github with vignettes

devtools::install_github('SvenGastauer/ZooDVM', build_vignettes = TRUE, dependencies = TRUE)
# if the above fails, try the alternative devtools syntax
# devtools::install_github('SvenGastauer/ZooDVM', build_opts = c("--no-resave-data", "--no-manual"))
```

## Dependencies and installation trouble shooting 

Please note that the below packages are unrelated to ZooDVM and the author has noinfluence on the development of these packages. 

### [Devtools](https://cran.r-project.org/web/packages/devtools/index.html)
If you get errors concerning devtools, try reinstalling devtools using:
``` r
 install.packages('devtools',dependencies = TRUE, INSTALL_opts = '--no-lock')
```
### [Rcpp](https://cran.r-project.org/web/packages/Rcpp/index.html) or other packages that need compiling
If you are getting error messages concerning Rcpp or any other packages that need compiling, make sure to install it using *dependencies = TRUE, INSTALL_opts = '--no-lock'* options:

``` r
 install.packages('Rcpp',dependencies = TRUE, INSTALL_opts = '--no-lock')
```

### [Rtools](https://cran.r-project.org/bin/windows/Rtools/) - Windows
At the time of writing a new version of Rtools and the way it is used has been implemented, which might cause some trouble.  
If you get a message such as:  
```WARNING: Rtools is required to build R packages, but is not currently installed. ```  
Install Rtools by downloading the required binary package from cran:  
* [Windows users](https://cran.r-project.org/bin/windows/Rtools/)  

### Rlang or other package that needs compiling fails  
First check if a folder with the package name is within the R library folder (if the standard paths are used these are in ../Documents/R/r version number/package name) Manually remove the package if present. Whilst in the package library structure, also have a look for any folder named 00xxx and remove it manually. Now try reinstalling the package using install.packages('package name'). If it fails again, repeat the previous steps and make sure no 00xxx folders are within the R library folder.  

## Getting help  
For ZooDVM specific questions make a feature request or post an issue on [GitHub](https://github.com/SvenGastauer/ZooDVM).    
For general R questions visit [Stack Overflow](https://stackoverflow.com/questions/tagged/r).  
If none of those options seem appropriate and you are getting really desperate, contact one the authors.  
