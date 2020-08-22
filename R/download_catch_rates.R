
#' Compile and harmonize fish survey data
#'
#' \code{download_catch_rates} queries and harmonizes fish survey data from public databases
#'
#' @param survey name of survey to be queried
#' @param add_zeros Boolean, whether to add zeros for species in samples that were conducted but where the species was not encountered (recommended: TRUE)
#' @param species_set either a character vector (giving scientific names of species) or a integer (giving number of most-frequently sighted species) to be queried
#' @param error_tol tolerance for errors when error-checking the algorithm for adding zeros
#' @param localdir local directory to save and load data from regional databases (to potentially avoid download times or problems without internet access)

#' @return A data frame of survey data with the following columns
#' \describe{
#'   \item{Sci}{Scientific name}
#'   \item{Year}{Calendar year}
#'   \item{TowID}{UniqueID associated with each sampling occasion}
#'   \item{Lat}{Latitude}
#'   \item{Long}{Longitude}
#'   \item{Wt}{Catch in KG (may be standardized by effort, check survey for details)}
#'   \item{Num}{Catch in numbers (may be standardized by effort, check survey for details)}
#'   \item{...}{Potentially other column labels}
#' }

#' @export
download_catch_rates = function( survey="Eastern_Bering_Sea", add_zeros=TRUE, species_set=10, error_tol=1e-12, localdir=NULL, measurement_type="biomass" ){
  ########################
  # Initial book-keeping
  ########################

  # Match survey
  survey = switch( survey, "Vancouver_Island"="WCVI",
                   "Haida_Gwaii"="WCHG",
                   "Queen_Charlotte_Sound"="QCS",
                   "Hecate_Strait"="HS",
                   "Eastern_Bering_Sea"="EBSBTS", "EBS"="EBSBTS", "EBSBTS"="EBSBTS",
    "Northern_Bering_Sea"="NBSBTS", "NBS"="NBSBTS", "West_coast_groundfish_bottom_trawl_survey"="WCGBTS",
    "West_coast_triennial"="WCT","WCT"="WCT", "WCGBTS"="WCGBTS", "West_coast_groundfish_hook_and_line"="WCGHL",
    "WCGHL"="WCGHL", "GOABTS"="GOABTS", "GOA"="GOABTS", "Gulf_of_Alaska"="GOABTS", "Aleutian_Islands"="AIBTS",
    "AIBTS"="AIBTS", "Bering_Sea_slope"="BSslope", NA)
  if( is.na(survey) ){
    message("'survey' input didn't match available options, please check help file")
    message("Options include:  'Eastern_Bering_Sea', 'Northern_Bering_Sea', 'Gulf of Alaska', 'Aleutian_Islands', 'West_coast_groundfish_bottom_trawl_survey', 'West_coast_groundfish_hook_and_line','West_coast_triennial','Vancouver_Island','Haida_Gwaii','Queen_Charlotte_Sound','Hecate_Strait'")
    return( invisible(NULL) )
  }else{
    message("Obtaining data for ",survey," survey...")
  }

  ########################
  # Local functions
  ########################

  # Loads or saves data locally if localdir is provided
  load_or_save = function( Downloaded_data, localdir, name ){
    # Load if locally available
    if( !is.null(localdir) & file.exists(paste0(localdir,"/",name,".RData")) ){
      load( file=paste0(localdir,"/",name,".RData") )
      message("Loading all data from local directory")
    }
    # Save if not locally available
    if( !is.null(localdir) & !file.exists(paste0(localdir,"/",name,".RData")) ){
      save( Downloaded_data, file=paste0(localdir,"/",name,".RData") )
      message("Saving downloaded data to local directory")
    }
    return( Downloaded_data )
  }

  # Rename columns of matrix or data frame (taken from ThorsonUtilities: https://github.com/james-thorson/utilities)
  rename_columns = function( DF, origname=colnames(DF), newname ){
    DF_new = DF
    for(i in 1:length(origname)){
      Match = match( origname[i], colnames(DF_new) )
      if(length(Match)==1) colnames(DF_new)[Match] = newname[i]
    }
    return(DF_new)
  }

  remove_header_rows <- function(Data_tmp){
    year_column = which(grepl("year", tolower(colnames(Data_tmp)))) # year column is not always called YEAR

    if (any(Data_tmp[, year_column] == "YEAR" & is.na(Data_tmp[, year_column]) == F)) {
      Which2Remove = which( Data_tmp[, year_column]=="YEAR" )
      Data_tmp = Data_tmp[-Which2Remove,]
      utils::write.csv( Data_tmp, file=paste0(tempdir(),"rewrite_data",files[i],".csv"), row.names=FALSE )
      Data_tmp = utils::read.csv( paste0(tempdir(),"rewrite_data",files[i],".csv"))
    }else{
      Data_tmp = Data_tmp
    }
  }

  ########################
  # Obtain data
  ########################

  # West Coast groundfish bottom trawl survey
  # https://www.nwfsc.noaa.gov/data/
  if( survey=="WCGBTS" ){
    # Names of pieces
    files = 2003:2018
    Vars = c("field_identified_taxonomy_dim$scientific_name", "date_dim$year", "tow",
      "latitude_dd", "longitude_dd", "centroid_id", "area_swept_ha_der",
      "cpue_kg_per_ha_der", "cpue_numbers_per_ha_der",
      "vessel_id", "project", "actual_station_design_dim$mean_depth_m", "blank")
    URLbase <- "https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey,performance=Satisfactory,"

    # Loop through download pieces
    Downloaded_data = NULL
    if( is.null(localdir) | !file.exists(paste0(localdir,"/WCGBTS_download.RData")) ){
      for(i in 1:length(files)){
        # Download and unzip
        Url_text = paste0(URLbase,"date_dim$year=",files[i],"&variables=",paste0(Vars,collapse=","))
        message("Downloading all WCGBTS catch-rate data for ",files[i]," from NWFSC database:  https://www.webapps.nwfsc.noaa.gov/data/")
        Data_tmp = jsonlite::fromJSON( Url_text )

        Data_tmp <- remove_header_rows(Data_tmp)

        # Append
        Downloaded_data = rbind( Downloaded_data, Data_tmp )
      }
    }
    # Load if locally available, and save if not
    Downloaded_data = load_or_save( Downloaded_data=Downloaded_data, localdir=localdir, name="WCGBTS_download")


    # Convert from KG and Num per Hectare to KG and Num, with hectares as a separate column (not currently working properly because area_swept_ha_der not present)
    if( "area_swept_ha_der" %in% colnames(Downloaded_data) ){
      Downloaded_data[c('cpue_kg_per_ha_der','cpue_numbers_per_ha_der')] = Downloaded_data[c('cpue_kg_per_ha_der','cpue_numbers_per_ha_der')] * outer(Downloaded_data[,'area_swept_ha_der'],c(1,1))
    }else{
      Downloaded_data = cbind( Downloaded_data, "area_swept_ha_der"=1 )
    }
    # setting numbers to NA for now to avoid issue with function that inserts zero values for hauls without observations
    Downloaded_data[,'cpue_numbers_per_ha_der'] = NA
    
    # Harmonize column names
    Data = rename_columns( Downloaded_data[,Vars[which(Vars%in%names(Downloaded_data))]], newname=c("Sci","Year","TowID","Lat","Long","Cell","AreaSwept_ha","Wt","Num","Vessel","Proj","Depth_m")[which(Vars%in%names(Downloaded_data))] )
    Data[,'TowID'] = paste0( Data[,'Year'], "_", Data[,'TowID'], "_", Data[,'Cell'] )
  }
   
   
   # West Coast triennial survey
   # https://www.nwfsc.noaa.gov/data/
   if( survey=="WCT" ){
     # Names of pieces
     files = seq(1977, 2004, 3)
     Vars = c("field_identified_taxonomy_dim$scientific_name", "date_dim$year", "trawl_id",
              "latitude_dd", "longitude_dd", "area_swept_ha_der",
              "cpue_kg_per_ha_der", "cpue_numbers_per_ha_der",
              "vessel_id", "project", "actual_station_design_dim$mean_depth_m", "blank")
     URLbase <- "https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=project=Groundfish%20Triennial%20Shelf%20Survey,performance=Satisfactory,"
     
     # Loop through download pieces
     Downloaded_data = NULL
     if( is.null(localdir) | !file.exists(paste0(localdir,"/WCT_download.RData")) ){
       for(i in 1:length(files)){
         # Download and unzip
         Url_text = paste0(URLbase,"date_dim$year=",files[i],"&variables=",paste0(Vars,collapse=","))
         message("Downloading all WCT catch-rate data for ",files[i]," from NWFSC database:  https://www.webapps.nwfsc.noaa.gov/data/")
         Data_tmp = jsonlite::fromJSON( Url_text )
         
         Data_tmp <- remove_header_rows(Data_tmp)
         
         # Append
         Downloaded_data = rbind( Downloaded_data, Data_tmp )
       }
     }
     # Load if locally available, and save if not
     Downloaded_data = load_or_save( Downloaded_data=Downloaded_data, localdir=localdir, name="WCT_download")
     
     
     # Convert from KG and Num per Hectare to KG and Num, with hectares as a separate column (not currently working properly because area_swept_ha_der not present)
     if( "area_swept_ha_der" %in% colnames(Downloaded_data) ){
       Downloaded_data[c('cpue_kg_per_ha_der','cpue_numbers_per_ha_der')] = Downloaded_data[c('cpue_kg_per_ha_der','cpue_numbers_per_ha_der')] * outer(Downloaded_data[,'area_swept_ha_der'],c(1,1))
     }else{
       Downloaded_data = cbind( Downloaded_data, "area_swept_ha_der"=1 )
     }
     # setting numbers to NA for now to avoid issue with function that inserts zero values for hauls without observations
     Downloaded_data[,'cpue_numbers_per_ha_der'] = NA
     
     # Harmonize column names
     Data = rename_columns( Downloaded_data[,Vars[which(Vars%in%names(Downloaded_data))]], newname=c("Sci","Year","TowID","Lat","Long","AreaSwept_ha","Wt","Num","Vessel","Proj","Depth_m")[which(Vars%in%names(Downloaded_data))] )
     #  Data[,'TowID'] = paste0( Data[,'Year'], "_", Data[,'TowID'], "_", Data[,'Cell'] )
     # using the trawl_id from the raw data which is a unique tow identifier 
   }
   
   
   # West Coast groundfish hook and line survey
   # https://www.nwfsc.noaa.gov/data/
   if( survey=="WCGHL" ){
     # Names of pieces
     # Vars = c("operation_type", "best_available_taxonomy_dim$scientific_name", "date_dim$yyyymmdd", "date_dim$year", "site_dim$site_latitude_dd", "site_dim$site_longitude_dd", "total_catch_wt_kg", "total_catch_numbers", "vessel", "sampling_start_time_dim$military_hour", "sampling_start_time_dim$minute", "sampling_end_time_dim$military_hour", "sampling_end_time_dim$minute" )
     Vars = c("operation_type", "best_available_taxonomy_dim$genus_70","best_available_taxonomy_dim$species_80", "date_dim$full_date", "date_dim$year", "site_dim$site_latitude_dd", "site_dim$site_longitude_dd", "total_catch_wt_kg", "total_catch_numbers", "vessel", "sampling_start_time_dim$military_hour", "sampling_start_time_dim$minute", "sampling_end_time_dim$military_hour", "sampling_end_time_dim$minute" )
     
     # Download data
     Downloaded_data = NULL
     if( is.null(localdir) | !file.exists(paste0(localdir,"/WCGHL_download.RData")) ){
       # Download and unzip
       Url_text = paste0("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/hooknline.catch_hooknline_view/selection.json?variables=",paste0(Vars,collapse=","))
       message("Downloading all WCGHL catch-rate data from NWFSC database:  https://www.webapps.nwfsc.noaa.gov/data/")
       
       
       Downloaded_data = jsonlite::fromJSON( Url_text )
       
       Downloaded_data <- remove_header_rows(Downloaded_data)
       
       
     }
     # Load if locally available, and save if not
     Downloaded_data = load_or_save( Downloaded_data=Downloaded_data, localdir=localdir, name="WCGHL_download")
     
     # add scientific name
     Downloaded_data$scientific_name <-
       paste(
         Downloaded_data$`best_available_taxonomy_dim$genus_70`,
         ifelse(is.na(Downloaded_data$`best_available_taxonomy_dim$species_80`), 'sp.', Downloaded_data$`best_available_taxonomy_dim$species_80`)
       )
     
     # Add HaulID
     WCGHL_data = cbind( Downloaded_data, "TowID"=paste(Downloaded_data[,'date_dim$year'],Downloaded_data[,'site_dim$site_latitude_dd'],Downloaded_data[,'site_dim$site_longitude_dd'],sep="_") )
     
     # Calculate effort measure
     WCGHL_data = cbind( WCGHL_data, "soak_time"=WCGHL_data[,'sampling_end_time_dim$military_hour']*60+WCGHL_data[,'sampling_end_time_dim$minute']-(WCGHL_data[,'sampling_start_time_dim$military_hour']*60+WCGHL_data[,'sampling_start_time_dim$minute']))
     if( !all(is.na(WCGHL_data[,'soak_time']) | WCGHL_data[,'soak_time']<600) ) stop("Check soak_time calculation for possible error")
     
     # Harmonize column names
     Data = rename_columns( WCGHL_data[,c("total_catch_wt_kg","total_catch_numbers","date_dim$year","scientific_name","site_dim$site_latitude_dd","site_dim$site_longitude_dd","TowID","soak_time","vessel")], newname=c("Wt","Num","Year","Sci","Lat","Long","TowID","Soak_Time_Minutes","Vessel"))
   }
   
   
  # Eastern Bering Sea
  # http://www.afsc.noaa.gov/RACE/groundfish/survey_data/data.htm
  if( survey=="EBSBTS" ){
    # Names of pieces
    files = c("1982_1984","1985_1989","1990_1994","1995_1999","2000_2004","2005_2008","2009_2012","2013_2016","2017_2018")

    # Loop through download pieces
    Downloaded_data = NULL
    if( is.null(localdir) | !file.exists(paste0(localdir,"/EBSBTS_download.RData")) ){
      for(i in 1:length(files)){
        # Download and unzip
        Tempdir = paste0( tempdir(), "/" )
        dir.create(Tempdir)
        temp = tempfile(pattern="file_", tmpdir=Tempdir, fileext=".zip")
        utils::download.file(paste0("http://www.afsc.noaa.gov/RACE/groundfish/survey_data/downloads/ebs",files[i],".zip"), temp)
        Data_tmp = utils::read.csv( unz(temp, paste0("ebs",files[i],".csv")) )
        unlink(temp)

        # Remove any row that repeats column headers again

        Data_tmp <- remove_header_rows(Data_tmp)

        # Append
        Downloaded_data = rbind( Downloaded_data, Data_tmp )
      }
    }
    # Load if locally available, and save if not
    Downloaded_data = load_or_save( Downloaded_data=Downloaded_data, localdir=localdir, name="EBSBTS_download")

    # Add TowID
    Data = cbind( Downloaded_data, "TowID"=paste0(Downloaded_data[,'YEAR'],"_",Downloaded_data[,'STATION'],"_",Downloaded_data[,'HAUL']) )
    # Harmonize column names
    Data = rename_columns( Data[,c('SCIENTIFIC','YEAR','TowID','LATITUDE','LONGITUDE','WTCPUE','NUMCPUE')], newname=c('Sci','Year','TowID','Lat','Long','Wt','Num') )
    Data = data.frame( Data, "AreaSwept_ha"=1 )
    # Exclude missing species
    Data = Data[ which(!Data[,'Sci']%in%c(""," ")), ]
  }
   
  # Bering Sea slope
  # http://www.afsc.noaa.gov/RACE/groundfish/survey_data/data.htm
  if( survey=="BSslope" ){
    # Names of pieces
    files = c("2002_2016")

    # Loop through download pieces
    Downloaded_data = NULL
    if( is.null(localdir) | !file.exists(paste0(localdir,"/BSslope_download.RData")) ){
      for(i in 1:length(files)){
        # Download and unzip
        Tempdir = paste0( tempdir(), "/" )
        dir.create(Tempdir)
        temp = tempfile(pattern="file_", tmpdir=Tempdir, fileext=".zip")
        utils::download.file(paste0("http://www.afsc.noaa.gov/RACE/groundfish/survey_data/downloads/bsslope",files[i],".zip"), temp)
        Data_tmp = utils::read.csv( unz(temp, paste0("bsslope",files[i],".csv")) )
        unlink(temp)

        # Remove any row that repeats column headers again

        Data_tmp <- remove_header_rows(Data_tmp)

        # Append
        Downloaded_data = rbind( Downloaded_data, Data_tmp )
      }
    }
    # Load if locally available, and save if not
    Downloaded_data = load_or_save( Downloaded_data=Downloaded_data, localdir=localdir, name="BSslope_download")

    # Add TowID
    Data = cbind( Downloaded_data, "TowID"=paste0(Downloaded_data[,'YEAR'],"_",Downloaded_data[,'STATION'],"_",Downloaded_data[,'HAUL']) )
    # Harmonize column names
    Data = rename_columns( Data[,c('SCIENTIFIC','YEAR','TowID','LATITUDE','LONGITUDE','WTCPUE','NUMCPUE')], newname=c('Sci','Year','TowID','Lat','Long','Wt','Num') )
    Data = data.frame( Data, "AreaSwept_ha"=1 )
    # Exclude missing species
    Data = Data[ which(!Data[,'Sci']%in%c(""," ")), ]
  }
  
 
  # Northern Bering Sea
  # http://www.afsc.noaa.gov/RACE/groundfish/survey_data/data.htm
  if( survey=="NBSBTS" ){
    # Names of pieces
    files = c("1982_2018")

    # Loop through download pieces
    Downloaded_data = NULL
    if( is.null(localdir) | !file.exists(paste0(localdir,"/NBSBTS_download.RData")) ){
      for(i in 1:length(files)){
        # Download and unzip
        Tempdir = paste0( tempdir(), "/" )
        dir.create(Tempdir)
        temp = tempfile(pattern="file_", tmpdir=Tempdir, fileext=".zip")
        utils::download.file(paste0("http://www.afsc.noaa.gov/RACE/groundfish/survey_data/downloads/nbs",files[i],".zip"), temp)
        Data_tmp = utils::read.csv( unz(temp, paste0("nbs",files[i],".csv")) )
        unlink(temp)

        # Remove any row that repeats column headers again

        Data_tmp <- remove_header_rows(Data_tmp)

        # Append
        Downloaded_data = rbind( Downloaded_data, Data_tmp )
      }
    }
    # Load if locally available, and save if not
    Downloaded_data = load_or_save( Downloaded_data=Downloaded_data, localdir=localdir, name="NBSBTS_download")

    # Add TowID
    Data = cbind( Downloaded_data, "TowID"=paste0(Downloaded_data[,'YEAR'],"_",Downloaded_data[,'STATION'],"_",Downloaded_data[,'HAUL']) )
    # Harmonize column names
    Data = rename_columns( Data[,c('SCIENTIFIC','YEAR','TowID','LATITUDE','LONGITUDE','WTCPUE','NUMCPUE')], newname=c('Sci','Year','TowID','Lat','Long','Wt','Num') )
    Data = data.frame( Data, "AreaSwept_ha"=1 )
    # Exclude missing species
    Data = Data[ which(!Data[,'Sci']%in%c(""," ")), ]
  }

  # Gulf of Alaska
  # http://www.afsc.noaa.gov/RACE/groundfish/survey_data/data.htm
  if( survey=="GOABTS" ){
    # Names of pieces
    files = c("1984_1987","1990_1999","2001_2005","2007_2013","2015_2017")
    # Loop through download pieces
    Downloaded_data = NULL
    if( is.null(localdir) | !file.exists(paste0(localdir,"/GOA_download.RData")) ){
      for(i in 1:length(files)){
        # Download and unzip
        temp = tempfile(pattern="file_", tmpdir=tempdir(), fileext=".zip")
        utils::download.file(paste0("http://www.afsc.noaa.gov/RACE/groundfish/survey_data/downloads/goa",files[i],".zip"),temp)
        Data_tmp = utils::read.csv( unz(temp, paste0("goa",files[i],".csv")), stringsAsFactors = F)
        unlink(temp)
        # Remove any row that repeats column headers again
        Data_tmp <- remove_header_rows(Data_tmp)

        # Append
        Downloaded_data = rbind( Downloaded_data, Data_tmp )
      }
    }
    # Load if locally available, and save if not
    Downloaded_data = load_or_save( Downloaded_data=Downloaded_data, localdir=localdir, name="GOA_download")
    # Add TowID
    Data = cbind( Downloaded_data, "TowID"=paste0(Downloaded_data[,'YEAR'],"_",Downloaded_data[,'STATION'],"_",Downloaded_data[,'HAUL']) )
    # Harmonize column names
    Data = rename_columns( Data[,c('SCIENTIFIC','YEAR','TowID','LATITUDE','LONGITUDE','WTCPUE','NUMCPUE')], newname=c('Sci','Year','TowID','Lat','Long','Wt','Num') )
    Data = data.frame( Data, "AreaSwept_ha"=1 )
    # Exclude missing species
    Data = Data[ which(!Data[,'Sci']%in%c(""," ")), ]
  }


  # Aleutian Islands
  # http://www.afsc.noaa.gov/RACE/groundfish/survey_data/data.htm
  if( survey=="AIBTS" ){
    # Names of pieces
    files = c("1983_2000","2002_2012","2014_2018")

    # Loop through download pieces
    Downloaded_data = NULL
    if( is.null(localdir) | !file.exists(paste0(localdir,"/AI_download.RData")) ){
      for(i in 1:length(files)){
        # Download and unzip
        temp = tempfile(pattern="file_", tmpdir=tempdir(), fileext=".zip")
        utils::download.file(paste0("http://www.afsc.noaa.gov/RACE/groundfish/survey_data/downloads/ai",files[i],".zip"),temp)
        Data_tmp = utils::read.csv( unz(temp, paste0("ai",files[i],".csv")) )
        unlink(temp)

        Data_tmp <- remove_header_rows(Data_tmp)

        # Append
        Downloaded_data = rbind( Downloaded_data, Data_tmp )
      }
    }
    # Load if locally available, and save if not
    Downloaded_data = load_or_save( Downloaded_data=Downloaded_data, localdir=localdir, name="AI_download")

    # Add TowID
    Data = cbind( Downloaded_data, "TowID"=paste0(Downloaded_data[,'YEAR'],"_",Downloaded_data[,'STATION'],"_",Downloaded_data[,'HAUL']) )
    # Harmonize column names
    Data = rename_columns( Data[,c('SCIENTIFIC','YEAR','TowID','LATITUDE','LONGITUDE','WTCPUE','NUMCPUE')], newname=c('Sci','Year','TowID','Lat','Long','Wt','Num') )
    Data = data.frame( Data, "AreaSwept_ha"=1 )
    # Exclude missing species
    Data = Data[ which(!Data[,'Sci']%in%c(""," ")), ]
  }
  
  # British Columbia - West Coast Vancouver Island
  if( survey=="WCVI" ){
    
    # Loop through download pieces
    Downloaded_data = NULL
    if( is.null(localdir) | !file.exists(paste0(localdir,"/WCVI_download.RData")) ){
      # Download and unzip
      Tempdir = paste0( tempdir(), "/" )
      dir.create(Tempdir)
      temp = tempfile(pattern="file_", tmpdir=Tempdir, fileext=".zip")
      utils::download.file(paste0("https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Groundfish_Synoptic_Trawl_Surveys/West_Coast_VI/english.zip"), temp)
      Data_tmp = utils::read.csv( unz(temp,"english/WCVI-catch-pac-dfo-mpo-science-eng.csv"), stringsAsFactors = F)
      # get extra dataset for lat/lon of tows
      Data_tmp2 = utils::read.csv( unz(temp,"english/WCVI-effort-pac-dfo-mpo-science-eng.csv"), stringsAsFactors = F)
      
      unlink(temp)
      
      # merge two datasets
      Data_tmp <- merge(Data_tmp, Data_tmp2, by=c("Survey.Year","Trip.identifier","Set.number"), all.x=TRUE)
      
      # Remove any row that repeats column headers again
      
      Data_tmp <- remove_header_rows(Data_tmp)
      
      # Append
      Downloaded_data = rbind( Downloaded_data, Data_tmp )
    }
    
    # Load if locally available, and save if not
    Downloaded_data = load_or_save( Downloaded_data=Downloaded_data, localdir=localdir, name="WCVI_download")
    
    # Add TowID
    Data = cbind( Downloaded_data, "TowID"=paste0(Downloaded_data[,'Survey.Year'],"_",Downloaded_data[,'Trip.identifier'],"_",Downloaded_data[,'Set.number']) )
    
    # Average start/end lat and lon
    Data$Lat = (Data$Start.latitude+Data$End.latitude)/2
    Data$Long = (Data$Start.longitude+Data$End.longitude)/2
    
    # Calculate area swept, in ha
    Data$AreaSwept_ha = (Data$Distance.towed..m.)*(Data$Trawl.door.spread..m.)/10000
    
    # Harmonize column names
    Data = rename_columns( Data[,c('Scientific.name','Survey.Year','TowID','Lat','Long','Catch.weight..kg.','Catch.count..pieces.','AreaSwept_ha')], newname=c('Sci','Year','TowID','Lat','Long','Wt','Num','AreaSwept_ha') )
    #  Data = data.frame( Data, "AreaSwept_ha"=1 )
    Data$Sci <- tolower(Data$Sci)
    
    # Exclude missing species
    Data = Data[ which(!Data[,'Sci']%in%c(""," ")), ]
  }
  
  
  # British Columbia - West Coast Haida Gwaii
  if( survey=="WCHG" ){
    
    # Loop through download pieces
    Downloaded_data = NULL
    if( is.null(localdir) | !file.exists(paste0(localdir,"/WCHG_download.RData")) ){
      # Download and unzip
      Tempdir = paste0( tempdir(), "/" )
      dir.create(Tempdir)
      temp = tempfile(pattern="file_", tmpdir=Tempdir, fileext=".zip")
      utils::download.file(paste0("https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Groundfish_Synoptic_Trawl_Surveys/West_Coast_HG/english.zip"), temp)
      Data_tmp = utils::read.csv( unz(temp,"english/WCHG-catch-pac-dfo-mpo-science-eng.csv"), stringsAsFactors = F)
      # get extra dataset for lat/lon of tows
      Data_tmp2 = utils::read.csv( unz(temp,"english/WCHG-effort-pac-dfo-mpo-science-eng.csv"), stringsAsFactors = F)
      
      unlink(temp)
      
      # merge two datasets
      Data_tmp <- merge(Data_tmp, Data_tmp2, by=c("Survey.Year","Trip.identifier","Set.number"), all.x=TRUE)
      
      # Remove any row that repeats column headers again
      
      Data_tmp <- remove_header_rows(Data_tmp)
      
      # Append
      Downloaded_data = rbind( Downloaded_data, Data_tmp )
    }
    
    # Load if locally available, and save if not
    Downloaded_data = load_or_save( Downloaded_data=Downloaded_data, localdir=localdir, name="WCVI_download")
    
    # Add TowID
    Data = cbind( Downloaded_data, "TowID"=paste0(Downloaded_data[,'Survey.Year'],"_",Downloaded_data[,'Trip.identifier'],"_",Downloaded_data[,'Set.number']) )
    
    # Average start/end lat and lon
    Data$Lat = (Data$Start.latitude+Data$End.latitude)/2
    Data$Long = (Data$Start.longitude+Data$End.longitude)/2
    
    # Calculate area swept, in ha
    Data$AreaSwept_ha = (Data$Distance.towed..m.)*(Data$Trawl.door.spread..m.)/10000
    
    # Harmonize column names
    Data = rename_columns( Data[,c('Scientific.name','Survey.Year','TowID','Lat','Long','Catch.weight..kg.','Catch.count..pieces.','AreaSwept_ha')], newname=c('Sci','Year','TowID','Lat','Long','Wt','Num','AreaSwept_ha') )
    #  Data = data.frame( Data, "AreaSwept_ha"=1 )
    Data$Sci <- tolower(Data$Sci)
    
    # Exclude missing species
    Data = Data[ which(!Data[,'Sci']%in%c(""," ")), ]
  }
  
  # British Columbia - Queen Charlotte Sound
  if( survey=="QCS" ){
    
    # Loop through download pieces
    Downloaded_data = NULL
    if( is.null(localdir) | !file.exists(paste0(localdir,"/QCS_download.RData")) ){
      # Download and unzip
      Tempdir = paste0( tempdir(), "/" )
      dir.create(Tempdir)
      temp = tempfile(pattern="file_", tmpdir=Tempdir, fileext=".zip")
      utils::download.file(paste0("https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Groundfish_Synoptic_Trawl_Surveys/Queen_Charlotte_Sound/english.zip"), temp)
      Data_tmp = utils::read.csv( unz(temp,"english/QCS-catch-pac-dfo-mpo-science-eng.csv"), stringsAsFactors = F)
      # get extra dataset for lat/lon of tows
      Data_tmp2 = utils::read.csv( unz(temp,"english/QCS-effort-pac-dfo-mpo-science-eng.csv"), stringsAsFactors = F)
      
      unlink(temp)
      
      # merge two datasets
      Data_tmp <- merge(Data_tmp, Data_tmp2, by=c("Survey.Year","Trip.identifier","Set.number"), all.x=TRUE)
      
      # Remove any row that repeats column headers again
      
      Data_tmp <- remove_header_rows(Data_tmp)
      
      # Append
      Downloaded_data = rbind( Downloaded_data, Data_tmp )
    }
    
    # Load if locally available, and save if not
    Downloaded_data = load_or_save( Downloaded_data=Downloaded_data, localdir=localdir, name="QCS_download")
    
    # Add TowID
    Data = cbind( Downloaded_data, "TowID"=paste0(Downloaded_data[,'Survey.Year'],"_",Downloaded_data[,'Trip.identifier'],"_",Downloaded_data[,'Set.number']) )
    
    # Average start/end lat and lon
    Data$Lat = (Data$Start.latitude+Data$End.latitude)/2
    Data$Long = (Data$Start.longitude+Data$End.longitude)/2
    
    # Calculate area swept, in ha
    Data$AreaSwept_ha = (Data$Distance.towed..m.)*(Data$Trawl.door.spread..m.)/10000
    
    # Harmonize column names
    Data = rename_columns( Data[,c('Scientific.name','Survey.Year','TowID','Lat','Long','Catch.weight..kg.','Catch.count..pieces.','AreaSwept_ha')], newname=c('Sci','Year','TowID','Lat','Long','Wt','Num','AreaSwept_ha') )
    #  Data = data.frame( Data, "AreaSwept_ha"=1 )
    Data$Sci <- tolower(Data$Sci)
    
    # Exclude missing species
    Data = Data[ which(!Data[,'Sci']%in%c(""," ")), ]
  }
  
  # British Columbia - Hecate Strait
  if( survey=="HS" ){
    
    # Loop through download pieces
    Downloaded_data = NULL
    if( is.null(localdir) | !file.exists(paste0(localdir,"/HS_download.RData")) ){
      # Download and unzip
      Tempdir = paste0( tempdir(), "/" )
      dir.create(Tempdir)
      temp = tempfile(pattern="file_", tmpdir=Tempdir, fileext=".zip")
      utils::download.file(paste0("https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Groundfish_Synoptic_Trawl_Surveys/Hecate_Strait/english.zip"), temp)
      Data_tmp = utils::read.csv( unz(temp,"english/HS-catch-pac-dfo-mpo-science-eng.csv"), stringsAsFactors = F)
      # get extra dataset for lat/lon of tows
      Data_tmp2 = utils::read.csv( unz(temp,"english/HS-effort-pac-dfo-mpo-science-eng.csv"), stringsAsFactors = F)
      
      unlink(temp)
      
      # merge two datasets
      Data_tmp <- merge(Data_tmp, Data_tmp2, by=c("Survey.Year","Trip.identifier","Set.number"), all.x=TRUE)
      
      # Remove any row that repeats column headers again
      
      Data_tmp <- remove_header_rows(Data_tmp)
      
      # Append
      Downloaded_data = rbind( Downloaded_data, Data_tmp )
    }
    
    # Load if locally available, and save if not
    Downloaded_data = load_or_save( Downloaded_data=Downloaded_data, localdir=localdir, name="HS_download")
    
    # Add TowID
    Data = cbind( Downloaded_data, "TowID"=paste0(Downloaded_data[,'Survey.Year'],"_",Downloaded_data[,'Trip.identifier'],"_",Downloaded_data[,'Set.number']) )
    
    # Average start/end lat and lon
    Data$Lat = (Data$Start.latitude+Data$End.latitude)/2
    Data$Long = (Data$Start.longitude+Data$End.longitude)/2
    
    # Calculate area swept, in ha
    Data$AreaSwept_ha = (Data$Distance.towed..m.)*(Data$Trawl.door.spread..m.)/10000
    
    # Harmonize column names
    Data = rename_columns( Data[,c('Scientific.name','Survey.Year','TowID','Lat','Long','Catch.weight..kg.','Catch.count..pieces.','AreaSwept_ha')], newname=c('Sci','Year','TowID','Lat','Long','Wt','Num','AreaSwept_ha') )
    #  Data = data.frame( Data, "AreaSwept_ha"=1 )
    Data$Sci <- tolower(Data$Sci)
    
    # Exclude missing species
    Data = Data[ which(!Data[,'Sci']%in%c(""," ")), ]
  }
  
  ########################
  # Determine species_set
  ########################
  if( is.numeric(species_set) ){
    Num_occur = tapply( ifelse(Data[,'Wt']>0,1,0), INDEX=Data[,'Sci'], FUN=sum, na.rm=TRUE )
    species_set = names(sort(Num_occur, decreasing=TRUE)[ 1:min(species_set,length(Num_occur)) ])
  }

  ######################
  # Add missing zeros
  ######################

  # Add zeros
  if( add_zeros==TRUE & survey%in%c("WCGBTS","WCT","WCGHL","EBSBTS","GOABTS","AIBTS","NBSBTS","BSslope","WCVI","WCHG","QCS") ){
    message( "Adding missing zeros")
    if( measurement_type=="biomass" ){
      DF = add_missing_zeros( data_frame=Data, unique_sample_ID_colname="TowID", sample_colname="Wt", species_subset=species_set, species_colname="Sci", Method="Fast", if_multiple_records="Combine", error_tol=error_tol)
      DF = DF[,c("Sci","Year","TowID","Lat","Long","Wt","AreaSwept_ha")]
    }
    if( measurement_type=="numbers" ){
      DF = add_missing_zeros( data_frame=Data, unique_sample_ID_colname="TowID", sample_colname="Num", species_subset=species_set, species_colname="Sci", Method="Fast", if_multiple_records="Combine", error_tol=error_tol)
      DF = DF[,c("Sci","Year","TowID","Lat","Long","Num","AreaSwept_ha")]
    }
  }else{  # FishData::
    message( "Not adding missing zeros")
    DF = Data[ which(Data[,'Sci'] %in% species_set), ]
    DF[,'Sci'] = droplevels( factor(DF[,'Sci'], levels=species_set) )
  }

  ######################
  # Other formatting changes
  # 1. In 'Sci', replace " " with "_" (because spaces don't work well with URLs or other text naming conventions)
  ######################
  DF[,'Sci'] = factor(gsub(DF[,'Sci'],pattern=" ",replacement="_"), levels=gsub(species_set,pattern=" ",replacement="_") )

  return(DF)
}


