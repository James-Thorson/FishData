
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
download_catch_rates = function( survey="Eastern_Bering_Sea", add_zeros=TRUE, species_set=10, error_tol=1e-12, localdir=NULL ){

  ########################
  # Initial book-keeping
  ########################

  # Match survey
  survey = switch( survey, "Eastern_Bering_Sea"="EBSBTS", "EBS"="EBSBTS", "EBSBTS"="EBSBTS", "West_coast_groundfish_bottom_trawl_survey"="WCGBTS", "WCGBTS"="WCGBTS", "West_coast_groundfish_hook_and_line"="WCGHL", "WCGHL"="WCGHL", "GOABTS"="GOABTS", "GOA"="GOABTS", "Gulf_of_Alaska"="GOABTS", "Aleutian_Islands"="AIBTS", "AIBTS"="AIBTS", NA)
  if( is.na(survey) ){
    message("'survey' input didn't match available options, please check help file")
    message("Options include:  'Eastern_Bering_Sea', 'Gulf of Alaska', 'Aleutian_Islands', 'West_coast_groundfish_bottom_trawl_survey', 'West_coast_groundfish_hook_and_line'")
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

  ########################
  # Obtain data
  ########################
	# RDB wants to add more columns to what's returned (e.g., depth)
  # West Coast groundfish bottom trawl survey
  # https://www.nwfsc.noaa.gov/data/
  if( survey=="WCGBTS" ){
    # Names of pieces
    files = 2003:2015
		haul_vars <- c("area_swept_ha_der", "date_dim$year", "date_dim$yyyymmdd", "door_width_m_der", "fluorescence_at_surface_mg_per_m3_der", "haul_latitude_dim$latitude_in_degrees", "haul_longitude_dim$longitude_in_degrees", "invertebrate_weight_kg", "net_height_m_der", "net_width_m_der", "nonspecific_organics_weight_kg", "o2_at_gear_ml_per_l_der", "operation_dim$leg", "operation_dim$operation_id", "operation_dim$pass", "operation_dim$performance_result", "operation_dim$project_name", "operation_dim$vessel", "salinity_at_gear_psu_der", "sampling_end_time_dim$hh24miss", "sampling_start_time_dim$hh24miss", "seafloor_depth_m_der", "target_station_design_dim$stn_invalid_for_trawl_date_whid", "temperature_at_gear_c_der", "temperature_at_surface_c_der", "turbidity_ntu_der", "vertebrate_weight_kg") # taken from TrawlHaulChars.csv, downloaded from nwfsc website
		haul_vars_choose <- "seafloor_depth_m_der" # maybe something like this could be derived from an argument; this is a testing placeholder
		hvc_names <- "depth" # might be good to make haul_vars named, so that clean name can be inferred
		stopifnot(all(haul_vars_choose %in% haul_vars)) # a placeholder check to make sure a user-requested variable is available
    Vars_core = c("operation_dim$operation_id", "field_identified_taxonomy_dim$scientific_name", "date_dim$year", "haul_latitude_dim$latitude_in_degrees", "haul_longitude_dim$longitude_in_degrees", "cpue_kg_per_ha_der", "cpue_numbers_per_ha_der", "operation_dim$vessel_id", "operation_dim$project_name" )
		Vars <- c(Vars_core, haul_vars_choose)

    # Loop through download pieces
    Downloaded_data = NULL
    if( is.null(localdir) | !file.exists(paste0(localdir,"/WCGBTS_download.RData")) ){
      for(i in 1:length(files)){
        # Download and unzip
        Url_text = paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/trawl.catch_fact/selection.json?filters=operation_dim$project_name=Groundfish%20Slope%20and%20Shelf%20Combination%20Survey,date_dim$year=",files[i],"&variables=",paste0(Vars,collapse=","))
        message("Downloading all WCGBTS catch-rate data for ",files[i]," from NWFSC database:  https://www.nwfsc.noaa.gov/data/")
        Data_tmp = jsonlite::fromJSON( Url_text )
        # Append
        Downloaded_data = rbind( Downloaded_data, Data_tmp )
      }
    }
    # Load if locally available, and save if not
    Downloaded_data = load_or_save( Downloaded_data=Downloaded_data, localdir=localdir, name="WCGBTS_download")

    # Harmonize column names
    Data = rename_columns( Downloaded_data, newname=c("Wt","Num","Year","Sci","Lat","Long","TowID","Proj","Vessel", hvc_names))
  }

  # West Coast groundfish hook and line survey
  # https://www.nwfsc.noaa.gov/data/
  if( survey=="WCGHL" ){
    # Names of pieces
    Vars = c("operation_type", "best_available_taxonomy_dim$scientific_name", "date_dim$yyyymmdd", "date_dim$year", "site_dim$site_latitude_dd", "site_dim$site_longitude_dd", "total_catch_wt_kg", "total_catch_numbers", "vessel", "sampling_start_time_dim$military_hour", "sampling_start_time_dim$minute", "sampling_end_time_dim$military_hour", "sampling_end_time_dim$minute" )

    # Download data
    Downloaded_data = NULL
    if( is.null(localdir) | !file.exists(paste0(localdir,"/WCGHL_download.RData")) ){
      # Download and unzip
      Url_text = paste0("https://www.nwfsc.noaa.gov/data/api/v1/source/hooknline.catch_hooknline_view/selection.json?variables=",paste0(Vars,collapse=","))
      message("Downloading all WCGHL catch-rate data from NWFSC database:  https://www.nwfsc.noaa.gov/data/")
      Downloaded_data = jsonlite::fromJSON( Url_text )
    }
    # Load if locally available, and save if not
    Downloaded_data = load_or_save( Downloaded_data=Downloaded_data, localdir=localdir, name="WCGHL_download")

    # Add HaulID
    WCGHL_data = cbind( Downloaded_data, "TowID"=paste(Downloaded_data[,'date_dim$yyyymmdd'],Downloaded_data[,'site_dim$site_latitude_dd'],Downloaded_data[,'site_dim$site_longitude_dd'],sep="_") )

    # Calculate effort measure
    WCGHL_data = cbind( WCGHL_data, "soak_time"=WCGHL_data[,'sampling_end_time_dim$military_hour']*60+WCGHL_data[,'sampling_end_time_dim$minute']-(WCGHL_data[,'sampling_start_time_dim$military_hour']*60+WCGHL_data[,'sampling_start_time_dim$minute']))
    if( !all(is.na(WCGHL_data[,'soak_time']) | WCGHL_data[,'soak_time']<600) ) stop("Check soak_time calculation for possible error")

    # Harmonize column names
    Data = rename_columns( WCGHL_data[,c("total_catch_wt_kg","total_catch_numbers","date_dim$year","best_available_taxonomy_dim$scientific_name","site_dim$site_latitude_dd","site_dim$site_longitude_dd","TowID","soak_time","vessel")], newname=c("Wt","Num","Year","Sci","Lat","Long","TowID","Soak_Time_Minutes","Vessel"))
  }

  # Eastern Bering Sea
  # http://www.afsc.noaa.gov/RACE/groundfish/survey_data/data.htm
  if( survey=="EBSBTS" ){
    # Names of pieces
    files = c("1982_1984","1985_1989","1990_1994","1995_1999","2000_2004","2005_2008","2009_2012","2013_2016")

    # Loop through download pieces
    Downloaded_data = NULL
    if( is.null(localdir) | !file.exists(paste0(localdir,"/EBSBTS_download.RData")) ){
      for(i in 1:length(files)){
        # Download and unzip
        temp = tempfile(pattern="file_", tmpdir=tempdir(), fileext=".zip")
        utils::download.file(paste0("http://www.afsc.noaa.gov/RACE/groundfish/survey_data/downloads/ebs",files[i],".zip"), temp)
        Data_tmp = utils::read.csv( unz(temp, paste0("ebs",files[i],".csv")) )
        unlink(temp)
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
    # Exclude missing species
    Data = Data[ which(!Data[,'Sci']%in%c(""," ")), ]
  }

  # Gulf of Alaska
  # http://www.afsc.noaa.gov/RACE/groundfish/survey_data/data.htm
  if( survey=="GOABTS" ){
    # Names of pieces
    files = c("1984_1987","1990_1999","2001_2005","2007_2013","2015")

    # Loop through download pieces
    Downloaded_data = NULL
    if( is.null(localdir) | !file.exists(paste0(localdir,"/GOA_download.RData")) ){
      for(i in 1:length(files)){
        # Download and unzip
        temp = tempfile(pattern="file_", tmpdir=tempdir(), fileext=".zip")
        utils::download.file(paste0("http://www.afsc.noaa.gov/RACE/groundfish/survey_data/downloads/goa",files[i],".zip"),temp)
        Data_tmp = utils::read.csv( unz(temp, paste0("goa",files[i],".csv")) )
        unlink(temp)
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
    # Exclude missing species
    Data = Data[ which(!Data[,'Sci']%in%c(""," ")), ]
  }

  # Aleutian Islands
  # http://www.afsc.noaa.gov/RACE/groundfish/survey_data/data.htm
  if( survey=="AIBTS" ){
    # Names of pieces
    files = c("1983_2000","2002_2012","2014_2016")

    # Loop through download pieces
    Downloaded_data = NULL
    if( is.null(localdir) | !file.exists(paste0(localdir,"/AI_download.RData")) ){
      for(i in 1:length(files)){
        # Download and unzip
        temp = tempfile(pattern="file_", tmpdir=tempdir(), fileext=".zip")
        utils::download.file(paste0("http://www.afsc.noaa.gov/RACE/groundfish/survey_data/downloads/ai",files[i],".zip"),temp)
        Data_tmp = utils::read.csv( unz(temp, paste0("ai",files[i],".csv")) )
        unlink(temp)
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
  if( add_zeros==TRUE & survey%in%c("WCGBTS","WCGHL","EBSBTS","GOABTS","AIBTS") ){
    DF = add_missing_zeros( data_frame=Data, unique_sample_ID_colname="TowID", sample_colname="Wt", species_subset=species_set, species_colname="Sci", Method="Fast", if_multiple_records="Combine", error_tol=error_tol)
  }else{  # FishData::
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


