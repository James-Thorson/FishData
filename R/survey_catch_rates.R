
#' Compile and harmonize fish survey data
#'
#' \code{survey_catch_rates} queries and harmonizes fish survey data from public databases
#'
#' @param survey name of survey to be queried
#' @param species_set either a character vector (giving scientific names of species) or a integer (giving number of most-frequently sighted species) to be queried
#' @param error_tol tolerance for errors when error-checking the algorithm for adding zeros
#' @param localdir local directory to save and load data from regional databases (to potentially avoid download times or problems without internet access)

#' @return A data frame with survey data

#' @export
survey_catch_rates = function( survey="Eastern_Bering_Sea", species_set=10, error_tol=1e-12, localdir=NULL ){

  ########################
  # Initial book-keeping
  ########################

  # Start
  DF = NULL

  # Match survey
  survey = switch( survey, "Eastern_Bering_Sea"="EBSBTS", "EBS"="EBSBTS", "EBSBTS"="EBSBTS", "West_coast_groundfish_bottom_trawl_survey"="WCGBTS", "WCGBTS"="WCGBTS", "West_coast_groundfish_hook_and_line"="WCGHL", "WCGHL"="WCGHL", "GOABTS"="GOABTS", "GOA"="GOABTS", "Gulf_of_Alaska"="GOABTS", "Aleutian_Islands"="AIBTS", "AIBTS"="AIBTS", NA)
  if( is.na(survey) ){
    message("'survey' input didn't match available options, please check help file")
    message("Options include:  'Eastern_Bering_Sea', 'Gulf of Alaska', 'Aleutian_Islands', 'West_coast_groundfish_bottom_trawl_survey', 'West_coast_groundfish_hook_and_line'")
    return( invisible(NULL) )
  }else{
    message("downloading ",survey," survey...")
  }

  ########################
  # Obtain data
  ########################

  # West Coast groundfish bottom trawl survey
  # https://www.nwfsc.noaa.gov/data/
  if( survey=="WCGBTS" ){
    # data to save
    WCGBTS_data = NULL

    # Names of pieces
    files = 2003:2015
    Vars = c("operation_dim$operation_id", "field_identified_taxonomy_dim$scientific_name", "date_dim$year", "haul_latitude_dim$latitude_in_degrees", "haul_longitude_dim$longitude_in_degrees", "cpue_kg_per_ha_der", "cpue_numbers_per_ha_der", "operation_dim$vessel_id", "operation_dim$project_name" )

    # Loop through download pieces
    if( is.null(localdir) | !file.exists(paste0(localdir,"/WCGTBS_data.RData")) ){
      for(i in 1:length(files)){
        # Download and unzip
        Url_text = paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=operation_dim$project_name=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey,date_dim$year=",files[i],"&variables=",paste0(Vars,collapse=","))
        message("Downloading all WCGBTS catch-rate data from ",files[i]," from NWFSC server")
        data = jsonlite::fromJSON( Url_text )
        # Append
        WCGBTS_data = rbind( WCGBTS_data, data )
      }
    }
    # Load if locally available
    if( !is.null(localdir) & file.exists(paste0(localdir,"/WCGTBS_data.RData")) ){
      load( file=paste0(localdir,"/WCGTBS_data.RData") )
    }
    # Save if not locally available
    if( !is.null(localdir) & !file.exists(paste0(localdir,"/WCGTBS_data.RData")) ){
      save( WCGBTS_data, file=paste0(localdir,"/WCGTBS_data.RData") )
    }

    # Harmonize column names
    Data = ThorsonUtilities::rename_columns( WCGBTS_data, newname=c("Wt","Num","Year","Sci","Lat","Long","TowID","Proj","Vessel"))
  }

  # West Coast groundfish hook and line survey
  # https://www.nwfsc.noaa.gov/data/
  if( survey=="WCGHL" ){
    # data to save
    WCGHL_data = NULL

    # Names of pieces
    Vars = c("operation_type", "best_available_taxonomy_dim$scientific_name", "date_dim$yyyymmdd", "date_dim$year", "site_dim$site_latitude_dd", "site_dim$site_longitude_dd", "total_catch_wt_kg", "total_catch_numbers", "vessel", "sampling_start_time_dim$military_hour", "sampling_start_time_dim$minute", "sampling_end_time_dim$military_hour", "sampling_end_time_dim$minute" )

    # Download data
    if( is.null(localdir) | !file.exists(paste0(localdir,"/WCGHL_data.RData")) ){
      # Download and unzip
      Url_text = paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/hooknline.catch_hooknline_view/selection.json?variables=",paste0(Vars,collapse=","))
      message("Downloading all WCGHL catch-rate data from NWFSC server")
      WCGHL_data = jsonlite::fromJSON( Url_text )
    }
    # Load if locally available
    if( !is.null(localdir) & file.exists(paste0(localdir,"/WCGHL_data.RData")) ){
      load( file=paste0(localdir,"/WCGHL_data.RData") )
    }
    # Save if not locally available
    if( !is.null(localdir) & !file.exists(paste0(localdir,"/WCGHL_data.RData")) ){
      save( WCGHL_data, file=paste0(localdir,"/WCGHL_data.RData") )
    }

    # Add HaulID
    WCGHL_data = cbind( WCGHL_data, "TowID"=paste(WCGHL_data[,'date_dim$yyyymmdd'],WCGHL_data[,'site_dim$site_latitude_dd'],WCGHL_data[,'site_dim$site_longitude_dd'],sep="_") )

    # Calculate effort measure
    WCGHL_data = cbind( WCGHL_data, "soak_time"=WCGHL_data[,'sampling_end_time_dim$military_hour']*60+WCGHL_data[,'sampling_end_time_dim$minute']-(WCGHL_data[,'sampling_start_time_dim$military_hour']*60+WCGHL_data[,'sampling_start_time_dim$minute']))
    if( !all(is.na(WCGHL_data[,'soak_time']) | WCGHL_data[,'soak_time']<600) ) stop("Check soak_time calculation for possible error")

    # Harmonize column names
    Data = ThorsonUtilities::rename_columns( WCGHL_data[,c("total_catch_wt_kg","total_catch_numbers","date_dim$year","best_available_taxonomy_dim$scientific_name","site_dim$site_latitude_dd","site_dim$site_longitude_dd","TowID","soak_time","vessel")], newname=c("Wt","Num","Year","Sci","Lat","Long","TowID","Soak_Time_Minutes","Vessel"))
  }

  # Eastern Bering Sea
  # http://www.afsc.noaa.gov/RACE/groundfish/survey_data/data.htm
  if( survey=="EBSBTS" ){
    # data to save
    EBS_data = NULL

    # Names of pieces
    files = c("1982_1984","1985_1989","1990_1994","1995_1999","2000_2004","2005_2008","2009_2012","2013_2015")

    # Loop through download pieces
    if( is.null(localdir) | !file.exists(paste0(localdir,"/EBS_data.RData")) ){
      for(i in 1:length(files)){
        # Download and unzip
        temp <- tempfile(pattern="file_", tmpdir=tempdir(), fileext=".zip")
        download.file(paste0("http://www.afsc.noaa.gov/RACE/groundfish/survey_data/downloads/ebs",files[i],".zip"),temp)
        data <- read.csv( unz(temp, paste0("ebs",files[i],".csv")) )
        unlink(temp)
        # Append
        EBS_data = rbind( EBS_data, data )
      }
    }
    # Load if locally available
    if( !is.null(localdir) & file.exists(paste0(localdir,"/EBS_data.RData")) ){
      load( file=paste0(localdir,"/EBS_data.RData") )
    }
    # Save if not locally available
    if( !is.null(localdir) & !file.exists(paste0(localdir,"/EBS_data.RData")) ){
      save( EBS_data, file=paste0(localdir,"/EBS_data.RData") )
    }

    # Add TowID
    Data = cbind( EBS_data, "TowID"=paste0(EBS_data[,'YEAR'],"_",EBS_data[,'STATION'],"_",EBS_data[,'HAUL']) )
    # Harmonize column names
    Data = ThorsonUtilities::rename_columns( Data[,c('COMMON','SCIENTIFIC','YEAR','TowID','LATITUDE','LONGITUDE','WTCPUE','NUMCPUE')], newname=c('Common','Sci','Year','TowID','Lat','Long','Wt','Num') )
    # Exclude missing species
    Data = Data[ which(!Data[,'Sci']%in%c(""," ")), ]
  }

  # Eastern Bering Sea
  # http://www.afsc.noaa.gov/RACE/groundfish/survey_data/data.htm
  if( survey=="GOABTS" ){
    # data to save
    GOA_data = NULL

    # Names of pieces
    files = c("1984_1987","1990_1999","2001_2005","2007_2013","2015")

    # Loop through download pieces
    if( is.null(localdir) | !file.exists(paste0(localdir,"/GOA_data.RData")) ){
      for(i in 1:length(files)){
        # Download and unzip
        temp <- tempfile(pattern="file_", tmpdir=tempdir(), fileext=".zip")
        download.file(paste0("http://www.afsc.noaa.gov/RACE/groundfish/survey_data/downloads/goa",files[i],".zip"),temp)
        data <- read.csv( unz(temp, paste0("goa",files[i],".csv")) )
        unlink(temp)
        # Append
        GOA_data = rbind( GOA_data, data )
      }
    }
    # Load if locally available
    if( !is.null(localdir) & file.exists(paste0(localdir,"/GOA_data.RData")) ){
      load( file=paste0(localdir,"/GOA_data.RData") )
    }
    # Save if not locally available
    if( !is.null(localdir) & !file.exists(paste0(localdir,"/GOA_data.RData")) ){
      save( GOA_data, file=paste0(localdir,"/GOA_data.RData") )
    }

    # Add TowID
    Data = cbind( GOA_data, "TowID"=paste0(GOA_data[,'YEAR'],"_",GOA_data[,'STATION'],"_",GOA_data[,'HAUL']) )
    # Harmonize column names
    Data = ThorsonUtilities::rename_columns( Data[,c('COMMON','SCIENTIFIC','YEAR','TowID','LATITUDE','LONGITUDE','WTCPUE','NUMCPUE')], newname=c('Common','Sci','Year','TowID','Lat','Long','Wt','Num') )
    # Exclude missing species
    Data = Data[ which(!Data[,'Sci']%in%c(""," ")), ]
  }

  # Eastern Bering Sea
  # http://www.afsc.noaa.gov/RACE/groundfish/survey_data/data.htm
  if( survey=="AIBTS" ){
    # data to save
    AI_data = NULL

    # Names of pieces
    files = c("1983_2000","2002_2012","2014")

    # Loop through download pieces
    if( is.null(localdir) | !file.exists(paste0(localdir,"/AI_data.RData")) ){
      for(i in 1:length(files)){
        # Download and unzip
        temp <- tempfile(pattern="file_", tmpdir=tempdir(), fileext=".zip")
        download.file(paste0("http://www.afsc.noaa.gov/RACE/groundfish/survey_data/downloads/ai",files[i],".zip"),temp)
        data <- read.csv( unz(temp, paste0("ai",files[i],".csv")) )
        unlink(temp)
        # Append
        AI_data = rbind( AI_data, data )
      }
    }
    # Load if locally available
    if( !is.null(localdir) & file.exists(paste0(localdir,"/AI_data.RData")) ){
      load( file=paste0(localdir,"/AI_data.RData") )
    }
    # Save if not locally available
    if( !is.null(localdir) & !file.exists(paste0(localdir,"/AI_data.RData")) ){
      save( AI_data, file=paste0(localdir,"/AI_data.RData") )
    }

    # Add TowID
    Data = cbind( AI_data, "TowID"=paste0(AI_data[,'YEAR'],"_",AI_data[,'STATION'],"_",AI_data[,'HAUL']) )
    # Harmonize column names
    Data = ThorsonUtilities::rename_columns( Data[,c('COMMON','SCIENTIFIC','YEAR','TowID','LATITUDE','LONGITUDE','WTCPUE','NUMCPUE')], newname=c('Common','Sci','Year','TowID','Lat','Long','Wt','Num') )
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
  if( survey %in% c("WCGBTS", "WCGHL", "EBSBTS", "GOABTS", "AIBTS") ){
    # data_frame=Data; unique_sample_ID_colname="TowID"; sample_colname="Wt"; species_subset=species_set; species_colname="Sci"; Method="Fast"
    # if_multiple_records="Combine"; verbose=TRUE; na.rm=FALSE; save_name=NULL
    DF = add_missing_zeros( data_frame=Data, unique_sample_ID_colname="TowID", sample_colname="Wt", species_subset=species_set, species_colname="Sci", Method="Fast", if_multiple_records="Combine", error_tol=error_tol)
  }      # FishData::

  return(DF)
}


