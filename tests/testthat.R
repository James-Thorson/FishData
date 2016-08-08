library(testthat)
library(FishData)

# Automated testing
testthat::test_check("FishData")

# Local testing
if(FALSE){
  # Run from local directory
  testthat::test_dir( "C:/Users/James.Thorson/Desktop/Project_git/FishData/tests/testthat/", reporter="check" )
}
