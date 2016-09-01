
#' Download and process satellite data from ERDDAP
#'
#' \code{Download_ERDDAP} queries the ERDDAP API for global sea-surface temperature and chlorophyll-a with reasonable temperal coverage
#'
#' @param Lat_bounds minimum and maximum Latitude boundaries for downloading data
#' @param Lon_bounds minimum and maximum Longitude boundaries
#' @param Date_bounds minimum and maximum dates (in 'month-day' format)
#' @param Year_set Numeric vector of years to query
#' @param Variable character giving variable to download (options are 'SST' or 'Chl-a')

#' @return A data frame of ERDDAP data
#' \describe{
#'   \item{Lat}{Latitude}
#'   \item{Lon}{Longitude}
#'   \item{...}{ERDDAP data columns}
#' }

#' @export

download_ERDDAP = function( Lat_bounds=c("min"=53,"max"=66), Lon_bounds=c("min"=-180,"max"=-157), Date_bounds=c("min"="06-01","max"="10-01"), Year_set=2010, Variable="SST", by=c('Lat'=1,'Lon'=1,'Day'=1) ){
  # File to save
  DF = NULL

  # Loop through years
  for( tI in 1:length(Year_set) ){

    # SST (1981-2015):  https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplL4AvhrrOIv1fv2.html
    if( Variable=="SST" ){
      Url_text = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplL4AvhrrOIv1fv2.json?analysed_sst[(",Year_set[tI],"-",Date_bounds['min'],"):",by['Day'],":(",Year_set[tI],"-",Date_bounds['max'],")][(",Lat_bounds['min'],"):",by['Lat'],":(",Lat_bounds['max'],")][(",Lon_bounds['min'],"):",by['Lon'],":(",Lon_bounds['max'],")]")
    }
    # Chlorophyll-a (2003-2015): http://upwell.pfeg.noaa.gov/erddap/griddap/erdMH1chlamday
    if( Variable=="Chl-a" ){
      # Numbers latitude negative to positive
      Url_text = paste0("http://upwell.pfeg.noaa.gov/erddap/griddap/erdMH1chlamday.json?chlorophyll[(",Year_set[tI],"-",Bounds['min','Dates'],"):",by['Day'],":(",Year_set[tI],"-",Date_bounds['max'],")][(",Lat_bounds['max'],"):",by['Lat'],":(",Lat_bounds['min'],")][(",Lon_bounds['min'],"):",by['Lon'],":(",Lon_bounds['max'],")]")
    }
    if( !exists("Url_text") ) stop("Check 'Variable'")

    # Download data
    message( "Downloading ",Variable," from ERDDAP for ", Year_set[tI] )
    message( Url_text )
    Start_time = Sys.time()
    Downloaded_data = jsonlite::fromJSON( Url_text )
    #temp = tempfile(pattern="file_", tmpdir=tempdir(), fileext=".csv")
    #download.file(url=gsub(x=Url_text, pattern="json", replacement="csv"), destfile=temp)
    #Downloaded_data = read.csv(file=temp)
    message( "Download took ", Sys.time()-Start_time, " seconds" )

    # Format data
    Data = data.frame(Downloaded_data[["table"]][[4]][,2:4], stringsAsFactors=FALSE)
    colnames( Data ) = Downloaded_data[["table"]][["columnNames"]][-1]
    for(i in 1:ncol(Data)) Data[,i] = as.numeric( Data[,i] )

    # Average data at each site
    Data = cbind( Data, "Site"=paste0(Data[,'latitude'],"_",Data[,'longitude']))
    Mean_Data = tapply( Data[,-match(c('latitude','longitude','Site'),names(Data))], INDEX=Data[,'Site'], FUN=mean )

    # Reform
    Mean_Data = cbind( "sst"=Mean_Data, "Lat"=sapply(names(Mean_Data), FUN=function(char){as.numeric(strsplit(char,split="_")[[1]][1])}), "Lon"=sapply(names(Mean_Data), FUN=function(char){as.numeric(strsplit(char,split="_")[[1]][2])}) )
    rownames(Mean_Data) = NULL
    DF = rbind( DF, cbind("Year"=Year_set[tI],Mean_Data) )
  }

  # Return
  return( DF )
}

# Plot download
if(FALSE){
  DF_plot = download_ERDDAP( Variable=c("SST","Chl-a")[1], Year_set=2003 )
  DF_plot = na.omit(DF_plot)
  library( mapdata )
  map( "worldHires", xlim=range(DF_plot[,'Lon']), ylim=range(DF_plot[,'Lat']))
  points( y=DF_plot[,'Lat'], x=DF_plot[,'Lon'] )
}
