#' Plots the positions and size of given storms
#'
#'Generates a map displaying the position and size of a storm along with its
#'track if specified. Storm position is determined by its latitude and
#'longitude, and storm size is determined by its wind radii at 34 knots, 50
#'knots, and 64 knots.
#'
#' @param storm.list vector of one or more storm names or ids. Storms prior to
#' 1950 require storm id.
#' @param year.list vector of one or more years in which respective storm
#' initially occurred.
#' @param dat data frame containing information for each storm.
#' @param date.time.list a vector of date and time values in the
#' format: Y-m-d H:M:S UTC.
#' @param path logical. If TRUE (default), storm path is plotted.
#' @return a \code{ggplot} object displaying map along with position and size of
#' storm. A red circular shape represents radii of wind at 64 knots. An orange
#' circular shape represents radii of wind at 50 knots. A green circular shape
#' represents radii of wind at 34 knots.
#' @examples
#' size1<- storm_size("KATRINA", 2005, hurdat, "2005-08-28 06:00:00 UTC")
#' size2<- storm_size("KATRINA", 2005, hurdat, "2005-08-28 06:00:00",
#'     path=FALSE)
#' size3<- storm_size(c("KATRINA", "SANDY"), c(2005, 2012), hurdat,
#'     c("2005-08-28 06:00:00", "2012-10-25 18:00:00"), path=TRUE)
#' size4<- storm_size(c("AL122005", "AL182012"), c(2005, 2012), hurdat,
#'     c("2005-08-25 18:00:00", "2012-10-28 12:00:00"), path=FALSE)
#' @export
storm_size <- function( storm.list, year.list, dat, date.time.list, path=TRUE ){

    # obtain interpolated latitude and longitude for each storm
    storm_dat <- interp_track( storm.list[1], year.list[1], dat )
    if( length( storm.list ) > 1 ){
        for( i in 2:length( storm.list ) ){
            tmp.dat <- interp_track( storm.list[i], year.list[i], dat )
            storm_dat <- rbind(storm_dat, tmp.dat)
        }
    }

    # create first layer of map depending on if path=TRUE
    if( path==TRUE ){ map <- track_map( storm.list, year.list, dat ) }
    else{ map <- track_map( storm.list, year.list, dat, date.time.list ) }

    # iteratively plot position and size of each storm
    for( i in seq_along( unique( storm_dat$id ) ) ){
        date.time <- as.POSIXlt(date.time.list, tz="UTC")
        tmp.dat <- dplyr::filter( storm_dat,
            id==unique(storm_dat$id)[i] & date.time==date.time.list[i])
        tmp.dat$labels <- paste0( tmp.dat$id," (",tmp.dat$name,")")
        storm.center.lat <- tmp.dat$latitude
        storm.center.lon <- tmp.dat$longitude
        radii.coordinates <- distance_coordinates2( tmp.dat, date.time )
        center.point <- c(storm.center.lon, storm.center.lat)

        if( sum(is.na(radii.coordinates)) == 24 ){
          stop( "Storm size not available.")
        }

      # plot size at 34 knots
        if( sum( radii.coordinates[,c(1,4,7,10)] == 0 ) != 8 ){
            NE.34.point <- as.numeric( radii.coordinates[,1] )
            SE.34.point <- as.numeric( radii.coordinates[,4] )
            NW.34.point <- as.numeric( radii.coordinates[,7] )
            SW.34.point <- as.numeric( radii.coordinates[,10] )
            storm.34.points <- storm_size_interp_34( center.point,
                NE.34.point, SE.34.point, SW.34.point, NW.34.point )
            map <- map + ggplot2::geom_polygon( data=storm.34.points,
                ggplot2::aes(x=lon.theta, y=lat.theta),
                    colour="green", lwd=1.2, fill="green", alpha=0.3)
      }

      # plot size at 50 knots
      if( sum( radii.coordinates[,c(2,5,8,11)] == 0 ) != 8 ){
        NE.50.point <- as.numeric( radii.coordinates[,2] )
        SE.50.point <- as.numeric( radii.coordinates[,5] )
        NW.50.point <- as.numeric( radii.coordinates[,8] )
        SW.50.point <- as.numeric( radii.coordinates[,11] )
        storm.50.points <- storm_size_interp_50( center.point,
            NE.50.point, SE.50.point, SW.50.point, NW.50.point )
        map <- map + ggplot2::geom_polygon( data=storm.50.points,
            ggplot2::aes(x=lon.theta, y=lat.theta),
            colour="orange", lwd=1.2, fill="orange", alpha=0.3)
      }

      # plot size at 64 knots
      if( sum( radii.coordinates[,c(3,6,9,12)] == 0 ) != 8 ){
        NE.64.point <- as.numeric( radii.coordinates[,3] )
        SE.64.point <- as.numeric( radii.coordinates[,6] )
        NW.64.point <- as.numeric( radii.coordinates[,9] )
        SW.64.point <- as.numeric( radii.coordinates[,12] )
        storm.64.points <- storm_size_interp_64( center.point,
            NE.64.point, SE.64.point, SW.64.point, NW.64.point )
        map <- map + ggplot2::geom_polygon( data=storm.64.points,
            ggplot2::aes(x=lon.theta, y=lat.theta),
            colour="red", lwd=1.2, fill="red", alpha=0.3)
      }
      map <- map + ggplot2::geom_point( data=tmp.dat,
                       ggplot2::aes(x = longitude, y=latitude, bg=labels),
                       pch=23, size=3, show.legend = FALSE) +
          ggplot2::geom_segment(
                ggplot2::aes(x=storm.center.lon, y=storm.center.lat,
                    xend=radii.coordinates[1,], yend=radii.coordinates[2,]),
                    arrow=ggplot2::arrow(length=ggplot2::unit(0.3, "cm")))
    }
    return( map )
}
