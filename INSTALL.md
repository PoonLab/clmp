## Installation Requirements (Ubuntu and Mac):

* [R software environment](https://cran.r-project.org/)
* R packages
  * [ape](http://ape-package.ird.fr/)
  * [ggtree](https://bioconductor.org/packages/release/bioc/html/ggtree.html)
* C libraries:
  * [GNU Scientific Library](https://www.gnu.org/software/gsl/)
  * [C-igraph](http://igraph.org/c/)
  * [flex](https://github.com/westes/flex)


## Requirements Installation Procedure (Ubuntu):

* The commands for each step are to be written/copied line by line to the terminal.

1. Updating and Upgrading The System
    ```
    sudo apt-get update
    sudo apt-get upgrade
    ```
2. Installing R
    ```
    sudo apt-get install r-base
    sudo apt-get install r-base-dev
    ```
3. Installing R Packages
    ```
    R
    install.packages("ape")
    source("https://bioconductor.org/biocLite.R")
    biocLite("ggtree")
    quit() 
    ```
4. Installing GNU tools and C libraries
    ```
    sudo apt-get install libgsl-dev
    sudo apt-get install libigraph0v5
    sudo apt-get install libigraph0-dev
    sudo apt-get install flex
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
* Compile and install Kaphi
    ```
    R CMD INSTALL clmp
    ```

