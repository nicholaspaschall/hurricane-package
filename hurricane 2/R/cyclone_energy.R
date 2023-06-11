#'Accumulated Cyclone Energy of a given storm
#'
#'Computes the accumulated cyclone energy of a given storm based on its max
#'wind, location, and time. The ACE is in 10e-04 kn^2 units.
#'
#'@param storm name or id of a storm. Storms prior to 1950 require storm id.
#'@param year corresponding year in which storm initially occurred
#'@param dat data frame containing information of each storm
#'@return a numeric value indicating the accumulated cyclone energy for the
#'        given storm and year. The value is computed based on the storm's
#'        max wind each 6 hours excluding max winds less than 35 and excluding
#'        points where the storm falls outside the Atlantic Basin
#'@examples
#'ace<- cyclone_energy("IVAN", 2004, hurdat)
#'ace<- cyclone_energy("AL091953", 1953, hurdat)
#'ace<- cyclone_energy("florence", 1994, hurdat)
#' @export
cyclone_energy <- function( storm, year, dat ){
    storm <- toupper(storm)
    if( !(storm %in% c(dat$name, dat$id) ) ){ stop( "Storm not found." ) }
    if( storm=="UNNAMED" ){ stop( "UNNAMED storms require storm ID." ) }
    if( !grepl("AL\\d+", storm) ){
        storm <- dplyr::filter(dat,
            name==storm & grepl(year, date.time))[1,]$id
        if(is.na(storm)){stop ("Incorrect year for storm.")}
    }

    #compute ACE based on following conditions
    storm_dat <- subset(dat, id==storm & year==year &
        max.wind >= 35 & grepl("0000|0600|1200|1800", dat$UTC.time))
    val <- sum(storm_dat$max.wind**2)/10000
    return(val)
}



