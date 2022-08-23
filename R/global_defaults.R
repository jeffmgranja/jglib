jg_logo <- "
    _             _       _   ____  
   (_) __ _ _ __ | | ___ | |_|___ \\ 
   | |/ _` | '_ \\| |/ _ \\| __| __) |
   | | (_| | |_) | | (_) | |_ / __/ 
  _/ |\\__, | .__/|_|\\___/ \\__|_____|
 |__/ |___/|_|                      
"

jg_dependency <- c(
  "parallel",
  "jgplot2"
)

jg_github <- c(
  "zeehio/facetscales",
  "caleblareau/BuenColors"
)

.onAttach <- function(libname, pkgname){
  
  cat(jg_logo)
  packageStartupMessage("Logo created from patorjk.com")

  #Package Startup
  v <- packageVersion("jgplot2")
  packageStartupMessage("jgplot2 : Version ", v)
  packageStartupMessage("jgplot2 website : https://github.com/jeffmgranja/jgplot2")
  
  #Load Packages
  packageStartupMessage("Loading Required Packages...")
  pkgs <- jg_dependency
  for(i in seq_along(pkgs)){
    tryCatch({
      packageStartupMessage("\tLoading Package : ", pkgs[i], " v", packageVersion(pkgs[i]))
      suppressPackageStartupMessages(require(pkgs[i], character.only=TRUE))
    }, error = function(e){
      packageStartupMessage("\tFailed To Load Package : ", pkgs[i])
    })
  }

  #Load Custom Packages
  pkgs <- basename(jg_github)
  for(i in seq_along(pkgs)){
    tryCatch({
      packageStartupMessage("\tLoading Package : ", pkgs[i], " v", packageVersion(pkgs[i]))
      suppressPackageStartupMessages(require(pkgs[i], character.only=TRUE))
    }, error = function(e){
      packageStartupMessage("\tFailed To Load Github Package : devtools::install_github('", jg_github[i], "')")
      packageStartupMessage("\t\tAttempting to install...")
      devtools::install_github(jg_github)
    })
  }

  #Set Defaults
  jgplot2()

  #Set Grid
  set_grid_mm()

}

