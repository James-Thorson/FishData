
#' Add missing zeros to data frame of catch rates
#'
#' \code{add_missing_zeros} takes a compressed `long-form` data frame, and adds rows for samples that were conducted but didn't catch any of a given fish species
#'
#' @param data_frame data frame containing data that missing zeros
#' @param unique_sample_ID_colname column name from 'data_frame' that gives a unique ID for each sample (where rows with the same unique ID are different species from the same sample)
#' @param sample_colname column name from 'data_frame' for sample of each species (e.g., representing catch in weight)
#' @param species_colname column name from 'data_frame' identifying species sampled for each row
#' @param species_subset character vector giving levels of 'species_colname' that should be retained (representing which species to keep);  if NULL, then \code{species_subset=levels(data_frame[,'species_colname'])}
#' @param if_multiple_records What to do if multiple rows have the same vales for \code{data_frame[,'unique_sample_ID_colname']} and \code{data_frame[,'species_colname']}
#' @param Method whether to use fast or slow method (fast may be more robust, and has more options for multiple_records)
#' @param verbose Boolean, whether to print output to screen
#' @param save_name optional directory for saving result locally
#' @param error_tol value used in checking that final biomass matches original biomass (if \code{error_tol=Inf} then no error checking is done)

#' @return A data frame with number of rows equal to number of unique samples times number of species

#' @export
add_missing_zeros = function( data_frame, unique_sample_ID_colname, sample_colname, species_colname, species_subset=NULL, Method="Slow", if_multiple_records="Error", verbose=TRUE, save_name=NULL, error_tol=1e-12 ){

  # Pre-processing
  if( !is.factor(data_frame[,unique_sample_ID_colname]) ){
    data_frame[,unique_sample_ID_colname] = factor(data_frame[,unique_sample_ID_colname])
  }

  # Load previous results if possible
  if( !is.null(save_name) && file.exists(save_name) ){
    load(file=save_name)
    if( verbose==TRUE ){
      species_set = unique( data_frame[,species_colname] )
      unique_sample_ID_set = unique(data_frame[,unique_sample_ID_colname])
      message("Loading from ", save_name)
      message( "Species included: ", paste(species_set,collapse=", ") )
      message( "Number of samples included for each species: ",length(unique_sample_ID_set) )
    }
  }else{
  # Run algorithm
    # record-keeping
    new_data_frame = NULL

    # set of species and samples
    species_set = unique( data_frame[,species_colname] )
    if( !is.null(species_subset)) species_set = species_subset[which(species_subset%in%species_set)]
    unique_sample_ID_set = unique(data_frame[,unique_sample_ID_colname])
    if( verbose==TRUE ){
      message( "Species to include: ", paste(species_set,collapse=", ") )
      message( "Number of samples to include for each species: ",length(unique_sample_ID_set) )
    }

    # Data frame for unique tows
    unique_data_frame = data_frame[match( unique_sample_ID_set, data_frame[,unique_sample_ID_colname]),]
    unique_data_frame[,sample_colname] = 0

    # Slow method uses loops to check every row
    if( Method=="Slow" ){
      # Loop through species
      for(p in 1:length(species_set)){
        # temporary data frame for data for species p
        temp_data_frame = unique_data_frame
        temp_data_frame[,species_colname] = species_set[p]
        # Loop through samples
        for(i in 1:nrow(temp_data_frame)){               #
          Match = which( data_frame[,unique_sample_ID_colname]==unique_sample_ID_set[i] )
          Match = Match[which( data_frame[Match,species_colname]==species_set[p]) ]
          # If only one match, then record
          if( length(Match)==1 ){
            temp_data_frame[i,sample_colname] = data_frame[Match,sample_colname]
          }
          # If more than one match, then depends on 'if_multiple_records'
          if( length(Match)>1 ){
            if( if_multiple_records=="Error") stop( "multiple unique-IDs with catch for same species")
            if( if_multiple_records=="Combine") temp_data_frame[i,sample_colname] = sum(data_frame[Match,sample_colname])
            if( if_multiple_records=="First") temp_data_frame[i,sample_colname] = data_frame[Match[1],sample_colname]
          }
          if( verbose==TRUE & (i%%1000)==0 ) message( "Processing row ",i," for ",species_set[p] )
        }
        # Append
        new_data_frame = rbind(new_data_frame, temp_data_frame)
        if( verbose==TRUE ) message( "Finished processing for ",species_set[p] )
      }
    }

    # Fast method
    if( Method=="Fast" ){
      for(p in 1:length(species_set)){
        # temporary data frame for data for species p
        temp_data_frame = unique_data_frame
        temp_data_frame[,species_colname] = species_set[p]
        # extract data for species p from input data frame
        species_data_frame = data_frame[ which(data_frame[,species_colname]==species_set[p]), ]
        # Combine across observations according to 'if_multiple_records'
        if( if_multiple_records=="First"){
          # Take first matching record for a given species and TowID
          Match = match( species_data_frame[,unique_sample_ID_colname], temp_data_frame[,unique_sample_ID_colname] )
          temp_data_frame[Match,sample_colname] = species_data_frame[,sample_colname]
        }
        if( if_multiple_records=="Combine"){
          # Sum across matching records for a given species and TowID
          temp_data_frame = temp_data_frame[ match(levels(species_data_frame[,unique_sample_ID_colname]), temp_data_frame[,unique_sample_ID_colname]), ]
          temp_data_frame[,sample_colname] = tapply( species_data_frame[,sample_colname], INDEX=species_data_frame[,unique_sample_ID_colname], FUN=sum )
        }
        if( if_multiple_records=="Error"){
          # Combination not implemented
          stop( "if_multiple_records='Error' is not implemented when Methods='Fast'" )
        }
        temp_data_frame[,sample_colname] = ifelse( is.na(temp_data_frame[,sample_colname]), 0, temp_data_frame[,sample_colname])
        # bind temporary data frame to record
        new_data_frame = rbind(new_data_frame, temp_data_frame)
        if( verbose==TRUE ) message( "Finished processing for ",species_set[p] )
      }
    }

    # Relevel
    new_data_frame[,species_colname] = factor(new_data_frame[,species_colname], levels=species_set)

    # Check that total biomass is preserved
    if( if_multiple_records!="First" ){
      # Check that total biomass is identical
      Which = which( data_frame[,species_colname] %in% species_set )
      almost_equal = function(a,b,tol){ ifelse( is.na(a) | is.na(b) | (a==0 & b==0) | abs(a-b)/((a+b)/2)<tol, TRUE, FALSE ) }
      New_counts = tapply( new_data_frame[,sample_colname], INDEX=factor(new_data_frame[,species_colname],levels=species_set), FUN=sum, na.rm=TRUE)
      Old_counts = tapply( data_frame[Which,sample_colname], INDEX=factor(data_frame[Which,species_colname],levels=species_set), FUN=sum, na.rm=TRUE)
      if( any(!almost_equal(New_counts, Old_counts, tol=error_tol)) ){
        stop( "missing rows in new data frame")
      }
      if( verbose==TRUE ){
        message("Total count in original: ", formatC(sum(data_frame[Which,sample_colname], na.rm=TRUE),format="f",digits=2) )
        message("Total count in new: ", formatC(sum(new_data_frame[,sample_colname], na.rm=TRUE),format="f",digits=2) )
      }
    }

    # Check that number of encounters is identical
    if( if_multiple_records=="Error" ){
      #### To be added
    }

    # Save
    if( !is.null(save_name) ) save(new_data_frame, file=save_name)
  }

  # Return new data frame
  return( new_data_frame )
}

