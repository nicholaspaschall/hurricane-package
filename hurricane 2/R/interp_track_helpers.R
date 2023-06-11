fill_values <- function( variable, storm_dat ){
    #identify column of interest and values which are not NA
    variable.col <- which( colnames( storm_dat ) == variable )
    variable.val <- which( !is.na( storm_dat[,variable.col] ) )

    #use these non NA values to fill in NA values associated with new times
    #not found in original dataset
    for( i in seq_along( variable.val ) ){
        if( i ==length( variable.val ) ){ break }
        storm_dat[,variable.col][ (variable.val[i]+1):(variable.val[i+1]-1) ] <-
            storm_dat[,variable.col][variable.val[i]]
    }
    return(storm_dat[,variable.col])
}

