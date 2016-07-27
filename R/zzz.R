
#.onLoad <- function(libname, pkgname) {
#}

.onAttach <- function(libname, pkgname) {
  if( !"SpatialDeltaGLMM" %in% installed.packages()[,1] ){
    print("Installing package: ThorsonUtilities...")
    devtools::install_github("james-thorson/utilities")
  }
}
