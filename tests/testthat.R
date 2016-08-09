library(testthat)
library(FishData)

# Automated testing
testthat::test_check("FishData")

# Local testing
if(FALSE){
  # Run from local directory
  testthat::test_dir( "C:/Users/James.Thorson/Desktop/Project_git/FishData/tests/testthat/", reporter="check" )
  # Full build check
  devtools::check( "C:/Users/James.Thorson/Desktop/Project_git/FishData/" )
  # Update documentation
  devtools::document( "C:/Users/James.Thorson/Desktop/Project_git/FishData/" )
}
