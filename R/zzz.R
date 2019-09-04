
#.onLoad <- function(libname, pkgname) {
#}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("###########################################################################################")
  packageStartupMessage("Loading package FishData, developed by James Thorson for the Northwest Fisheries Science Center")
  packageStartupMessage("This package is in development, and comes with no implied or explicit guaruntee of accuracy")
  packageStartupMessage("Interested users should read the code and consult the survey-specific websites being queried")
  packageStartupMessage("###########################################################################################")
  if( !"icesDatras" %in% installed.packages()[,1] ){
    #print("Installing package: icesDatras...")
    #devtools::install_github('ices-tools-prod/icesDatras', ref="1.1-1")
  }else{
    #Data = data( package="icesDatras", verbose=FALSE )
    #if( !("aphia" %in% Data$results[,'Item']) ) stop("Must use `icesDatras` version `1.1=1` from GitHub")
  }
  packageStartupMessage("Not downloading 'icesDatras', so European data access will not work")
}
