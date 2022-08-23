jg_logo <- "
   _                               
  (_) __ ___   _____ _ __ ___  ___ 
  | |/ _` \\ \\ / / _ \\ '__/ __|/ _ \\
  | | (_| |\\ V /  __/ |  \\__ \\  __/
 _/ |\\__, | \\_/ \\___|_|  |___/\\___|
|__/ |___/                         
"

jg_dependency <- c(
  "parallel"
)

jg_github <- c(
  "jeffmgranja/jgplot2"
)

.onAttach <- function(libname, pkgname){
  
  cat(jg_logo)
  packageStartupMessage("Logo created from patorjk.com")

  #Package Startup
  v <- packageVersion("jgverse")
  packageStartupMessage("jgverse : Version ", v)
  packageStartupMessage("jgverse website : https://github.com/jeffmgranja/jgverse")
  
  #Load Packages
  packageStartupMessage("Loading Required Packages...")
  pkgs <- jg_dependency
  for(i in seq_along(pkgs)){
    tryCatch({
      packageStartupMessage("\tLoading Package : ", pkgs[i], " v", packageVersion(pkgs[i]))
      .quiet(suppressPackageStartupMessages(require(pkgs[i], character.only=TRUE)))
    }, error = function(e){
      packageStartupMessage("\tFailed To Load Package : ", pkgs[i])
    })
  }

  #Load Custom Packages
  pkgs <- basename(jg_github)
  for(i in seq_along(pkgs)){
    tryCatch({
      packageStartupMessage("\tLoading Package : ", pkgs[i], " v", packageVersion(pkgs[i]))
      .quiet(suppressPackageStartupMessages(require(pkgs[i], character.only=TRUE)))
    }, error = function(e){
      packageStartupMessage("\tFailed To Load Github Package : devtools::install_github('", jg_github[i], "')")
      packageStartupMessage("\t\tAttempting to install...")
      devtools::install_github(jg_github)
    })
  }

}

.quiet <- function(x) { 
  tmp <- tempfile()
  sink(tmp) 
  on.exit(sink()) 
  invisible(force(x)) 
  if(file.exists(tmp)) file.remove(tmp)
  0
} 