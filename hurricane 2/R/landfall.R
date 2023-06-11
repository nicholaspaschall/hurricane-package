#' Determining if a storm made landfall in continental US
#'
#' Used to either determine if a storm made landfall, produce a storm's
#' strongest landfall, or generate a data frame containing information
#' regarding its track and when and where it was over land or sea.
#'
#' @param storm name or id of a storm. Storms prior to 1950 require a storm id.
#' @param year corresponding year in which storm initially occurred.
#' @param dat data frame containing information of each storm.
#' @param method method of choice regarding information to be returned. Methods
#' include "landfall.check", "landfall.data", "landfall.strongest".
#' @return method "landfall.check" returns TRUE/FALSE indicating
#' whether storm made landfall in continental US. Method "landfall.data",
#' returns data frame regarding storm's landfall status at specific times and
#' positions. Method "landfall.strongest", returns data frame of storm's
#' strongest landfall along with position and max.wind
#' @examples
#' landfall_check<- landfall("KATRINA", 2005, hurdat, "landfall.check")
#' landfall_data<- landfall("irene", 2011, hurdat, "landfall.data")
#' landfall_strongest<- landfall("AL092004", 2004, hurdat, "landfall.strongest")
#' @export
landfall <- function( storm, year, dat,
    method=c("landfall.check", "landfall.data", "landfall.strongest") ){

    # retrieve storm's track
    storm_dat <- interp_track( storm, year, hurdat )

    # determine if storm's position makes landfall
    us.outline <- ggplot2::map_data("usa")
    inside <- rep( 0, times=nrow(storm_dat) )

    for( i in seq_along( unique(us.outline$group) ) ){
        tmp.dat <- subset( us.outline, group==i )
        group.outline.df <- data.frame(
            lon=tmp.dat$long,
            lat=tmp.dat$lat )
        group.border.poly <- sp::Polygon(group.outline.df)
        coords <- group.border.poly@coords
        lon <- coords[,1]
        lat <- coords[,2]

        inside.points <- sp::point.in.polygon(
            storm_dat$longitude, storm_dat$latitude, lon, lat )

      # method landfall.check returns TRUE on first occurance of landfall
        if( method=="landfall.check" & sum(inside.points %in% c(1,2)) != 0){
            return(TRUE)
        }

        inside.index <- which( inside.points %in% c(1,2) )
        inside[inside.index] <- 1
    }

    if( method == "landfall.check" ){
          if( sum(inside %in% c(1,2)) == 0 ){
              return(FALSE)
          }

      # method landfill.data returns data regardless of landfill or not
    }else if( method == "landfall.data" ){
        result.dat <- cbind( storm_dat, inside )
        result.dat <- dplyr::select(result.dat, id, date.time,
            latitude, longitude, max.wind, inside)
        return(result.dat)

      # method landfill.strongest returns data if storm made landfall
    }else if( method == "landfall.strongest" ){
         result.dat <- cbind( storm_dat, inside )
         result.dat <- dplyr::select(result.dat, id, date.time,
             latitude, longitude, max.wind, inside)

         max.wind <- 0
         max.wind.index <- NULL
         for( i in seq_along(inside) ){
             if( i != length(inside) ){
                 if( inside[i] == 0 & inside[i+1] == 1 ){
                     if( result.dat$max.wind[i+1] > max.wind ){
                         max.wind <- result.dat$max.wind[i+1]
                         max.wind.index <- i+1
                     }
                 }
             }
         }
         if( is.null(max.wind.index ) ){
             return( cat( paste(
                toupper(storm),
                "did not make landfall in the continental United States." ) ) )
         }else{
             return(result.dat[max.wind.index,])
         }
     }
}
