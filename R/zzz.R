
#.onLoad <- function(libname, pkgname) {
#}

.onAttach <- function(libname, pkgname) {
  print("###########################################################################################")
  print("Loading package FishData, developed by James Thorson for the Northwest Fisheries Science Center")
  print("This package is in development, and comes with no implied or explicit guaruntee of accuracy"
  print("Interested users should read the code and colsult the survey-specific websites being queried"
  print("###########################################################################################")
  #if( !"ThorsonUtilities" %in% installed.packages()[,1] ){
  #  print("Installing package: ThorsonUtilities...")
  #  devtools::install_github("james-thorson/utilities")
  #}
}
