
# Tutorial: http://r-pkgs.had.co.nz/tests.html
# And example see: https://github.com/ss3sim/ss3sim/tree/master/tests/testthat
context("Testing 'add_missing_zeros' for WGBHL")

# Test 'add_missing_zeros' for Hook-and-line
  # Hard because TowID is duplicated in year 1
  # Fast because few observations
test_that("add_missing_zeros is working for WCGHL ", {
  # Download data
  Data0 = download_catch_rates( survey="WCGHL", add_zeros=FALSE, species_set=Inf )
  Data = Data0[ which(Data0[,'Sci']=="Sebastes paucispinis"), ]

  # add_missing_zeros -- combine
  DF1_fast = add_missing_zeros( data_frame=Data0, unique_sample_ID_colname="TowID", sample_colname="Wt", species_subset="Sebastes paucispinis", species_colname="Sci", Method="Fast", if_multiple_records="Combine" )
  DF1_slow = add_missing_zeros( data_frame=Data0, unique_sample_ID_colname="TowID", sample_colname="Wt", species_subset="Sebastes paucispinis", species_colname="Sci", Method="Slow", if_multiple_records="Combine" )

  # add_missing_zeros -- first
  DF2_fast = add_missing_zeros( data_frame=Data0, unique_sample_ID_colname="TowID", sample_colname="Wt", species_subset="Sebastes paucispinis", species_colname="Sci", Method="Fast", if_multiple_records="First" )
  DF2_slow = add_missing_zeros( data_frame=Data0, unique_sample_ID_colname="TowID", sample_colname="Wt", species_subset="Sebastes paucispinis", species_colname="Sci", Method="Slow", if_multiple_records="First" )

  # Check number of observations
  expect_equal( length(unique(Data0$TowID)), nrow(DF1_slow), tolerance=1e-10 )
  expect_equal( length(unique(Data0$TowID)), nrow(DF1_fast), tolerance=1e-10 )
  expect_equal( length(unique(Data0$TowID)), nrow(DF2_slow), tolerance=1e-10 )
  expect_equal( length(unique(Data0$TowID)), nrow(DF2_fast), tolerance=1e-10 )

  # Check number of encounters
    # Won't work because two are aggregated
  #expect_equal( tapply(Data$Wt,INDEX=Data$Year,FUN=function(vec){sum(vec>0)}), tapply(DF1_slow$Wt,INDEX=DF1_slow$Year,FUN=function(vec){sum(vec>0)}), tolerance=1e-10 )
  #expect_equal( tapply(Data$Wt,INDEX=Data$Year,FUN=function(vec){sum(vec>0)}), tapply(DF1_fast$Wt,INDEX=DF1_fast$Year,FUN=function(vec){sum(vec>0)}), tolerance=1e-10 )
  #expect_equal( tapply(Data$Wt,INDEX=Data$Year,FUN=function(vec){sum(vec>0)}), tapply(DF2_slow$Wt,INDEX=DF2_slow$Year,FUN=function(vec){sum(vec>0)}), tolerance=1e-10 )
  #expect_equal( tapply(Data$Wt,INDEX=Data$Year,FUN=function(vec){sum(vec>0)}), tapply(DF2_fast$Wt,INDEX=DF2_fast$Year,FUN=function(vec){sum(vec>0)}), tolerance=1e-10 )

  # Check total biomass by year
    # Only works when if_multiple_records=="Combine", because if_multiple_records=="First" drops a duplicated TowID
  expect_equal( tapply(Data$Wt,INDEX=Data$Year,FUN=sum), tapply(DF1_slow$Wt,INDEX=DF1_slow$Year,FUN=sum), tolerance=1e-10 )
  expect_equal( tapply(Data$Wt,INDEX=Data$Year,FUN=sum), tapply(DF1_fast$Wt,INDEX=DF1_fast$Year,FUN=sum), tolerance=1e-10 )
  #expect_equal( tapply(Data$Wt,INDEX=Data$Year,FUN=sum), tapply(DF2_slow$Wt,INDEX=DF2_slow$Year,FUN=sum), tolerance=1e-10 )
  #expect_equal( tapply(Data$Wt,INDEX=Data$Year,FUN=sum), tapply(DF2_fast$Wt,INDEX=DF2_fast$Year,FUN=sum), tolerance=1e-10 )
})


