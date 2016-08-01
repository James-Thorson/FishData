
#' Compile and harmonize fish survey data
#'
#' \code{scrape_data} queries and harmonizes fish survey data from public databases
#'
#' @param region name of region to be queried (currently only "Eastern_Bering_Sea")
#' @param species_set either a character vector (giving scientific names of species) or a integer (giving number of most-frequently sighted species) to be queried
#' @param error_tol tolerance for errors when error-checking the algorithm for adding zeros
#' @param localdir local directory to save and load data from regional databases (to potentially avoid download times or problems without internet access)

#' @return A data frame with survey data

#' @export
scrape_data = function( region="Eastern_Bering_Sea", species_set=10, error_tol=1e-12, localdir=NULL ){

  # Start
  DF = NULL

  # California_current
  # https://www.nwfsc.noaa.gov/data/
  if( region=="California_current" ){
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

    # Determine species_set
    if( is.numeric(species_set) ){
      Num_occur = tapply( ifelse(Data[,'Wt']>0,1,0), INDEX=Data[,'Sci'], FUN=sum, na.rm=TRUE )
      species_set = names(sort(Num_occur, decreasing=TRUE)[1:species_set])
    }

    # Add zeros
    DF = ThorsonUtilities::add_missing_zeros( data_frame=Data, unique_sample_ID_colname="TowID", sample_colname="Wt", species_subset=species_set, species_colname="Sci", Method="Fast", error_tol=error_tol)
  }

  # Eastern Bering Sea
  # http://www.afsc.noaa.gov/RACE/groundfish/survey_data/data.htm
  if( region=="Eastern_Bering_Sea" ){
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

    # Determine species_set
    if( is.numeric(species_set) ){
      Num_occur = tapply( ifelse(Data[,'Wt']>0,1,0), INDEX=Data[,'Sci'], FUN=sum)
      species_set = names(sort(Num_occur, decreasing=TRUE)[1:species_set])
    }

    # Add zeros
    DF = ThorsonUtilities::add_missing_zeros( data_frame=Data, unique_sample_ID_colname="TowID", sample_colname="Wt", species_subset=species_set, species_colname="Sci", Method="Fast", error_tol=error_tol)
  }

  # Eastern Bering Sea
  # http://www.afsc.noaa.gov/RACE/groundfish/survey_data/data.htm
  if( region=="Gulf_of_Alaska" ){
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

    # Determine species_set
    if( is.numeric(species_set) ){
      Num_occur = tapply( ifelse(Data[,'Wt']>0,1,0), INDEX=Data[,'Sci'], FUN=sum)
      species_set = names(sort(Num_occur, decreasing=TRUE)[1:species_set])
    }

    # Add zeros
    DF = ThorsonUtilities::add_missing_zeros( data_frame=Data, unique_sample_ID_colname="TowID", sample_colname="Wt", species_subset=species_set, species_colname="Sci", Method="Fast", error_tol=error_tol)
  }

  # Eastern Bering Sea
  # http://www.afsc.noaa.gov/RACE/groundfish/survey_data/data.htm
  if( region=="Aleutian_Islands" ){
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

    # Determine species_set
    if( is.numeric(species_set) ){
      Num_occur = tapply( ifelse(Data[,'Wt']>0,1,0), INDEX=Data[,'Sci'], FUN=sum)
      species_set = names(sort(Num_occur, decreasing=TRUE)[1:species_set])
    }

    # Add zeros
    DF = ThorsonUtilities::add_missing_zeros( data_frame=Data, unique_sample_ID_colname="TowID", sample_colname="Wt", species_subset=species_set, species_colname="Sci", Method="Fast", error_tol=error_tol)
  }

  # Debugging -- check for errors in equal weight in add_missing_zeros function
  if(FALSE){
    DF1 = ThorsonUtilities::add_missing_zeros( data_frame=Data, unique_sample_ID_colname="TowID", sample_colname="Wt", species_subset=species_set, species_colname="Sci", Method="Fast", error_tol=Inf)
    DF2 = Data[which(Data[,'Sci']%in%species_set),]
    # Check sums
    tapply( DF1[,'Wt'], INDEX=DF1[,'Sci'], FUN=sum, na.rm=TRUE )
    tapply( DF2[,'Wt'], INDEX=DF2[,'Sci'], FUN=sum, na.rm=TRUE )
  }

  # Return stuff
  if( is.null(DF) ) stop("region didn't match options, please check code")
  return(DF)
}


