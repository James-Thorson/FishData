

download_datras = function(survey="NS-IBTS", species_set=10, years=1981:2015, quarters=1, localdir=getwd(), verbose=TRUE ){
  # NOTES
  # CatCatchWgt varies among species within the same haul
  # CatCatchWgt seems to have many cases where it is implausibly low given fish lengths in that tow (e.g., HaulID="1991.1.ARG.NA.13" for "Gadus morhua")

  # Area-swept notes:
  # Irish West Coast Gov gear has a door width of 48 meters (https://datras.ices.dk/Home/Descriptions.aspx)
  # Fresh survey has door width of 20 meters (https://datras.ices.dk/Home/Descriptions.aspx)
  # Duration is in minutes

  # Download data if necessary
  if( !file.exists(paste0(localdir,"/",survey,"_hh.RData")) ){
    hh = icesDatras::getDATRAS("HH", survey=survey, years=years, quarters=quarters)
    save( hh, file=paste0(localdir,"/",survey,"_hh.RData"))
  }else{
    load( file=paste0(localdir,"/",survey,"_hh.RData"))
  }
  if( !file.exists(paste0(localdir,"/",survey,"_hl.RData")) ){
    hl = icesDatras::getDATRAS("HL", survey=survey, years=years, quarters=quarters)
    save( hl, file=paste0(localdir,"/",survey,"_hl.RData"))
  }else{
    load( file=paste0(localdir,"/",survey,"_hl.RData"))
  }
  if( !file.exists(paste0(localdir,"/",survey,"_ca.RData")) ){
    ca = icesDatras::getDATRAS("CA", survey=survey, years=years, quarters=quarters)
    save( ca, file=paste0(localdir,"/",survey,"_ca.RData"))
  }else{
    load( file=paste0(localdir,"/",survey,"_ca.RData"))
  }

  # Load species name key
  data( aphia, package="icesDatras" )
  #on.exit( remove("aphia"), add=TRUE )
  unique_species = data.frame('code'=na.omit(unique(hl$Valid_Aphia)))
  unique_species$genus_species = with(unique_species, aphia[ match(unique_species$code,aphia$aphia_code), 'species'])

  # Add uniqueID
  hl$HaulID <- with(hl, paste(Year, Quarter, Ship, StNo, HaulNo, sep = "."))
  ca$HaulID <- with(ca, paste(Year, Quarter, Ship, StNo, HaulNo, sep = "."))
  hh$HaulID <- with(hh, paste(Year, Quarter, Ship, StNo, HaulNo, sep = "."))
  key <- c("Year", "Quarter", "Ship", "StNo", "HaulNo", "HaulID")

  # Add Lat/Lon
  hl = cbind(hl, hh[match(hl$HaulID,hh$HaulID), c("ShootLat","ShootLong","DataType","HaulDur")])

  # Add length units
  # from `DATRAS::addExtraVariables` here:  https://www.rforge.net/DATRAS/svn.html
  LngtCode2cm <- c(. = 0.1, "0" = 0.1, "1" = 1, "2" = 1, "5" = 1)
  hl$Length_units <- with(hl, sapply(hl$LngtCode, FUN=function(char){LngtCode2cm[match(char,names(LngtCode2cm))]}) )
  ca$Length_units <- with(ca, sapply(ca$LngtCode, FUN=function(char){LngtCode2cm[match(char,names(LngtCode2cm))]}) )

  # Loop though species and predict weight where missing
  hl$predicted_weight = with( hl, NA )
  # based on `DATRAS::addWeightByHaul`
  # BUT I don't predict to middle of LngtClass in `hl`
  for( pI in 1:nrow(unique_species)){
    # Observations with sufficient data for weight-length key
    DB = na.omit(ca[which(ca$Valid_Aphia==unique_species[pI,'code']),c('IndWgt','LngtClass','Length_units')])
    DB = DB[ which(DB[,'IndWgt']>0), ]

    # Only predict if data are sufficient
    if( nrow(DB)>10 ){
      # Fit weight-length key
      Lm = lm(log(IndWgt) ~ I(log(LngtClass*Length_units)), data=DB )
      if( verbose==TRUE ){
        plot( x=log(DB$LngtClass*DB$Length_units), y=log(DB$IndWgt) )
        abline( a=Lm$coef[1], b=Lm$coef[2] )
      }

      # Only predict if relationship makes sense
      if( Lm$coef['I(log(LngtClass * Length_units))']<4 | Lm$coef['I(log(LngtClass * Length_units))']>2.5 ){
        # Use weight-length key to predict missing weights
        hl$predicted_weight[which(hl$Valid_Aphia==unique_species[pI,'code'])] = exp(Lm$coef[1] + Lm$coef[2]*log(hl$LngtClass*hl$Length_units)[which(hl$Valid_Aphia==unique_species[pI,'code'])] )
      }else{
        warning("Possible problem with ", unique_species[pI,'genus_species'])
      }
    }
  }

  # Decide on set of species
  if( is.numeric(species_set) ){
    Num_occur = tapply( ifelse(hl[,'HLNoAtLngt']>0,1,0), INDEX=hl[,'Valid_Aphia'], FUN=sum, na.rm=TRUE )
    species_set = names(sort(Num_occur, decreasing=TRUE)[ 1:min(species_set,length(Num_occur)) ])
    species_set = data.frame( 'aphia'=species_set, 'species'=aphia[ match(species_set,aphia$aphia_code), 'species'])
  }else{
    species_set = data.frame( 'aphia'=aphia[match(species_set,aphia$species),'aphia_code'], 'species'=species_set)
  }

  # Add expansion factors
  # TotalNo = sum( HLNoAtLngt*SubFactor )  # http://datras.ices.dk/Data_products/FieldDescription.aspx?Fields=HLNoAtLngt&SurveyID=2341
  # TotalNo depends upon DataType (C is scaled to 1-hour tows, whereas other codes depend upon SubFactor)  # Pers. Comm. from Casper Berg
  hl$expansion_factor = with( hl, ifelse(hl[,'DataType']=="C", hl[,'HaulDur']/60, hl[,'SubFactor']) )
  hl$expanded_number = with( hl, hl[,'HLNoAtLngt']*hl[,'expansion_factor'] )    #
  hl$expanded_weight = with( hl, hl[,'predicted_weight']*hl[,'HLNoAtLngt']*hl[,'expansion_factor'] )    #

  # Add missing zeros, and compress accross length bins
  DF = FishData::add_missing_zeros( data_frame=hl, unique_sample_ID_colname="HaulID", sample_colname="expanded_weight", species_subset=species_set[,'aphia'], species_colname="Valid_Aphia", Method="Fast", if_multiple_records="Combine", error_tol=1e-2, verbose=verbose)
  DF2 = FishData::add_missing_zeros( data_frame=hl, unique_sample_ID_colname="HaulID", sample_colname="expanded_number", species_subset=species_set[,'aphia'], species_colname="Valid_Aphia", Method="Fast", if_multiple_records="Combine", error_tol=1e-2, verbose=verbose)
  DF$species = with( DF, species_set[match(x=DF[,'Valid_Aphia'], table=species_set[,'aphia']), 'species'])
  DF2$species = with( DF2, species_set[match(x=DF2[,'Valid_Aphia'], table=species_set[,'aphia']), 'species'])

  # Check for missing data
  for( pI in 1:nrow(species_set)){
    if( all(is.na(hl[ which( hl$Valid_Aphia==species_set[pI,'aphia'] ), 'expanded_weight'])) ){
      DF[ which(DF[,'Valid_Aphia']==species_set[pI,'aphia']), 'expanded_weight'] = NA
    }
  }

  # Replace with known catch weight when possible
  Match = match( paste(DF$HaulID,DF$Valid_Aphia,sep="_"), paste(hl$HaulID,hl$Valid_Aphia,sep="_") )
  DF$observed_weight = with( DF, hl[Match,'CatCatchWgt'] )
  DF$catch_weight = with( DF, ifelse(is.na(DF$observed_weight),DF$expanded_weight,DF$observed_weight) )

  # Sub in
  DF[,'TotalNo'] = DF2[,'TotalNo'] = with( DF, hl[Match,'TotalNo'] )

  # Return hl for selected species
  Return = list("hl"=hl, "DF"=DF, "DF2"=DF2, "species_set"=species_set)
  return(Return)
}
