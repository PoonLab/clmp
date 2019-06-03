

## Requirements:

* [R software environment](https://cran.r-project.org/)
* R packages
  * [ape](http://ape-package.ird.fr/)
  * [Rcpp](https://cran.r-project.org/web/packages/Rcpp/index.html)
  * [ggtree](https://bioconductor.org/packages/release/bioc/html/ggtree.html)
* GNU tools
  * [autoconf]()
  * [flex](https://github.com/westes/flex)
* C libraries:
  * [GNU Scientific Library](https://www.gnu.org/software/gsl/)
  * [C-igraph](http://igraph.org/c/)



## Installing on Ubuntu

These instructions have been tested on Ubuntu versions 16.04 and 18.04.

1. Update and upgrade your system (optional):
    ```console
    $ sudo apt-get update
    $ sudo apt-get upgrade
    ```
    
2. If you do not already have R on your system, install it using the package manager:
    ```
    $ sudo apt-get install r-base
    $ sudo apt-get install r-base-dev
    ```
    
3. Install the required R packages:
    ```console
    $ R
    > install.packages("ape")
    > install.packages("Rcpp")
    ```
    
    If you are running R 3.6+, use the following commands to install Bioconductor and ggtree:
    ```R
    > if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager")
    > BiocManager::install()
    > BiocManager::install("ggtree")
    ```
    If you are running an older version of R, use the following commands:
    ```R
    > source("https://bioconductor.org/biocLite.R")
    > biocLite("ggtree")
    ```
    Finally, exit R to return to the command line:
    ```R
    > quit()
    Save workspace image? [y/n/c]: n
    ```
    
4. Install the required GNU tools and C libraries
    ```console
    $ sudo apt install libgsl-dev
    $ sudo apt install libigraph0v5
    $ sudo apt install libigraph0-dev
    $ sudo apt install flex
    $ sudo apt install autoconf
    ```

5. Download the clmp R package from GitHub:
   ```
   $ git clone https://github.com/PoonLab/clmp
   ```
   If you do not have git installed or would prefer to not clone the repository, you can download a ZIP archive of the package:
   ```
   $ wget https://github.com/PoonLab/clmp/archive/master.zip
   $ unzip master.zip
   $ cd clmp-master
   ```
   
6. Build and install the R package
   ```console
   $ autoconf
   $ ./configure
   $ R CMD INSTALL .
   * installing to library ‘/home/art/R/x86_64-pc-linux-gnu-library/3.6’
   * installing *source* package ‘clmp’ ...
   ** using staged installation
   configure: creating ./config.status
   config.status: creating src/Makevars
   ** libs
   make: Nothing to be done for 'all'.
   installing to /home/art/R/x86_64-pc-linux-gnu-library/3.6/00LOCK-clmp/00new/clmp/libs
   ** R
   ** byte-compile and prepare package for lazy loading
   ** help
   *** installing help indices
   ** building package indices
   ** testing if installed package can be loaded from temporary location
   ** checking absolute paths in shared objects and dynamic libraries
   ** testing if installed package can be loaded from final location
   ** testing if installed package keeps a record of temporary installation path
   * DONE (clmp)
   ```

## Requirements Installation Procedure (Mac):

* The commands for each step are to be written/copied line by line to the terminal.

1. Install the latest version of R from the appropriate [mirror](https://cran.r-project.org/mirrors.html).
2. Installing Xcode
    ```
    xcode-select --install
    ```
   Follow the generated prompts to the end of installation. To verify that Xcode was correctly installed check what version of Xcode was installed:
    ```
    xcodebuild -version
    ```
3. Installing Command Line Tools

   Go to http://developer.apple.com/downloads and sign in with your Apple ID (the same one you use for iTunes and app
   purchases). Search for "command line tools" (in the search field on the left), then click on version corresponding to the
   installed version of Xcode and click on the the .dmg link to download it. Run the .dmg and follow the generated prompts
   to the end of installation.
4. Installing Homebrew
    ```
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    ```
5. Installing R Packages
    ```
    R
    install.packages("ape")
    source("https://bioconductor.org/biocLite.R")
    biocLite("ggtree")
    quit()
    ```
4. Installing GNU tools and C libraries
    ```
    brew install gsl
    brew install igraph
    brew install flex
    ```
    
## clmp Installation Procedure (Ubuntu and Mac):

* Navigate to your preferred location in the filesystem and clone clmp from the GitHhub repository
    ```
    git clone https://github.com/PoonLab/clmp
    ```
* Compile and install the R package `clmp`
    ```
    autoconf
    ./configure
    R CMD INSTALL clmp
    ```

