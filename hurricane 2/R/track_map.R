#' Map of storm tracks for a selection of storms
#'
#'Generates a map displaying the storm tracks or selective points in time for
#'each given storm
#'
#' @param storm.list A vector of one or more storm names or ids. Storms prior to
#' 1950 require a storm id.
#' @param year.list A vector of one or more years in which the respective storm
#' initially occurred.
#' @param dat A data frame containing information about each storm.
#' @param date.time.list An optional vector of date and time values in the
#' format = Y-m-d H:M:S UTC.
#' @return A ggplot displaying the world map along with color coded storm tracks
#' or specific locations of each inputted storm.
#' @examples
#' track_path<- track_map("KATRINA", 2005, hurdat)
#' track_point<- track_map("KATRINA", 2005, hurdat, "2005-08-23 18:00:00")
#' track_paths<-track_map(c("KATRINA", "SANDY"), c(2005, 2012), hurdat,
#'     date.time.list=c("2005-08-23 18:00:00", "2012-10-21 18:00:00 UTC"))
#' track_points<-track_map(c("KATRINA", "SANDY"), c(2005, 2012), hurdat,
#'     date.time.list=c("2005-08-23 18:00:00", "2012-10-21 18:00:00 UTC"))
#' @export
track_map <- function( storm.list, year.list, dat, date.time.list=NULL ){

    # generate interpolated latitude and longitudes for each storm
    storm_dat <- interp_track( storm.list[1], year.list[1], dat )
    if( length(storm.list) > 1 ){
        for( i in 2:length(storm.list) ){
            tmp.dat <- interp_track( storm.list[i], year.list[i], dat )
            storm_dat <- rbind(storm_dat, tmp.dat)
        }
    }

    # obtain appropriate latitude and longitude ranges for map xlim and ylim
    lat.range <- c(min(storm_dat$latitude), max(storm_dat$latitude))
    lon.range <- c(min(storm_dat$longitude), max(storm_dat$longitude))
    if( diff(lat.range) > diff(lon.range) ){
        diffs <- diff(lat.range) - diff(lon.range)
        diffs.div <- diffs/2
        lon.range <- c( lon.range[1]-diffs.div, lon.range[2]+diffs.div )
    }else{
        diffs <- diff(lon.range) - diff(lat.range)
        diffs.div <- diffs/2
        lat.range <- c( lat.range[1]-diffs.div, lat.range[2]+diffs.div )
    }


    # if date.time.list not given, function generates track of each storm
    # iteratively
    if( is.null(date.time.list) ){
        map <- world_map_helper( lon.range, lat.range )
        for( i in seq_along( unique( storm_dat$id ) ) ){
            tmp.dat <- dplyr::filter(storm_dat, id==unique(storm_dat$id)[i])
            if( nrow(tmp.dat) == 0 ){
                stop( paste("Incorrect date.time for", storm.list[i]) )
            }
            tmp.dat$labels <- paste0( tmp.dat$id," (",tmp.dat$name,")")
            map <- map + ggplot2::geom_path(data=tmp.dat,
                ggplot2::aes(x=longitude, y=latitude, color=labels), lwd=0.7)
        }
        map <- map + ggplot2::labs(color="Storm")
    }

    # if date.time.list given, function generates points of each date.time.list
    # iteratively
    else{
        map <- world_map_helper()
        for( i in seq_along(date.time.list) ){
            tmp.dat <- storm_dat[which(date.time.list[i]==storm_dat$date.time),]
            if( nrow(tmp.dat) == 0 ){
                stop( paste("Incorrect date.time for", storm.list[i]) )
            }
            tmp.dat$labels <- paste0( tmp.dat$id," (",tmp.dat$name,")")
            map <- map + ggplot2::geom_point(data=tmp.dat,
              ggplot2::aes(x=longitude,y=latitude,bg=labels), pch=23, size=5)
          }
          map <- map +
              ggplot2::coord_fixed(ratio=1, xlim=c(lon.range), ylim=c(lat.range),
                  expand=TRUE, clip="on") +
              ggplot2::labs(bg="Storm")
    }
    map <- map +
        ggplot2::theme(legend.box.background =
            ggplot2::element_rect(colour = "black" ))
    return(map)
}
