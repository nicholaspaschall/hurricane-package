#' Interpolated coordinates of a storm's track
#'
#'Return data frame containing interpolated latitude and longitude coordinates
#'of the storm's center at 30 minute increments. Estimated values for selected
#'variables from the original data set are also included.
#'
#' @param storm name or id of a storm. Storms prior to 1950 require storm id.
#' @param year corresponding year in which storm initially occurred.
#' @param dat data frame containing information of each storm.
#' @return a data frame with the following components:
#' \itemize{
#'   \item \code{name}: name of the storm.
#'   \item \code{date.time}: date and time throughout storm's duration in thirty
#'    minute increments.
#'   \item \code{record.id}: used to identify records that correspond to
#'    landfalls or to indicate the reason for inclusion of a record not at the
#'    standard synoptic times.
#'   \item \code{latitude}: interpolated latitude of storm's center, in
#'    degrees north.
#'   \item \code{longitude}: interpolated longitude of storm's center. Negative
#'    values indicate west longitudes, positive values indicate east longitudes.
#'   \item \code{id}: identification code of chosen storm.
#'   \item \code{storm.status}: status of storm, referring to level of
#'    intensity.
#'   \item \code{max.wind}: the maximum 1-min average wind (in knots)
#'    associated with the tropical cyclone at an elevation of 10 m with an
#'    unobstructed exposure.
#' }
#' @examples
#' storm <- interp_track("CLAUDETTE", 2021, hurdat)
#' storm <- interp_track("AL032021", 2021, hurdat)
#' @export
interp_track <- function( storm, year, dat ){
    storm <- toupper(storm)

    if( storm == "UNNAMED" ){ stop( "UNNAMED storms require storm id." ) }
    if( !(storm %in% c(dat$name, dat$id) ) ){ stop( "Storm not found." ) }

    # if storm id not given, find id of storm
    if( !grepl("AL\\d+", storm) ){
        storm <- dplyr::filter(dat,
            name==storm & grepl(year, date.time))[1,]$id
    }

    # create given storm data
    storm_dat <- dat[ which( dat$id == storm & grepl(year, dat$date.time)), ]
    storm_dat <- dplyr::select( storm_dat, -date, -UTC.time, -radius.max.wind)

    # create 30 minute increments based on initial and ending time of storm
    initial.date <- storm_dat$date.time[1]
    ending.date <- storm_dat$date.time[ nrow(storm_dat) ]
    new.times <- tryCatch(
        expr = seq( initial.date, ending.date, 30*60 ),
        error = function( cnd ){
          if( grepl("'to' must be of length 1", cnd$message) ){
            stop("Year of storm is incorrect.")
          }
        }
    )
    # add 30 minute increments to original storm_dat dataset
    new.times.dat <- data.frame( date.time = as.POSIXlt(new.times) )
    storm_dat <- dplyr::full_join( storm_dat, new.times.dat, by=c("date.time") )
    storm_dat <- dplyr::arrange(storm_dat, date.time)

    # interpolate latitude and longitude for new times
    lat <- storm_dat$latitude.N
    lon <- ifelse( storm_dat$longitude.E >= 180,
        storm_dat$longitude.E - 360, storm_dat$longitude.E)
    storm_dat$latitude.N <- suppressWarnings(round(
        stats::spline(storm_dat$date.time, lat, n=nrow(storm_dat))$y, 1) )
    storm_dat$longitude.E <- suppressWarnings(round(
        stats::spline(storm_dat$date.time, lon, n=nrow(storm_dat))$y, 1) )

    # fill each row between each 6 hour interval with data from initial data set
    storm_dat$id <- storm
    storm_dat$name <- storm_dat$name[1]
    storm_dat$storm.status <- fill_values( "storm.status", storm_dat )
    storm_dat$max.wind <- fill_values( "max.wind", storm_dat )
    storm_dat$min.pressure <- fill_values( "min.pressure", storm_dat )
    storm_dat$wind.NE.34kt <- fill_values( "wind.NE.34kt", storm_dat )
    storm_dat$wind.SE.34kt <- fill_values( "wind.SE.34kt", storm_dat )
    storm_dat$wind.SW.34kt <- fill_values( "wind.SW.34kt", storm_dat )
    storm_dat$wind.NW.34kt <- fill_values( "wind.NW.34kt", storm_dat )
    storm_dat$wind.NE.50kt <- fill_values( "wind.NE.50kt", storm_dat )
    storm_dat$wind.SE.50kt <- fill_values( "wind.SE.50kt", storm_dat )
    storm_dat$wind.SW.50kt <- fill_values( "wind.SW.50kt", storm_dat )
    storm_dat$wind.NW.50kt <- fill_values( "wind.NW.50kt", storm_dat )
    storm_dat$wind.NE.64kt <- fill_values( "wind.NE.64kt", storm_dat )
    storm_dat$wind.SE.64kt <- fill_values( "wind.SE.64kt", storm_dat )
    storm_dat$wind.SW.64kt <- fill_values( "wind.SW.64kt", storm_dat )
    storm_dat$wind.NW.64kt <- fill_values( "wind.NW.64kt", storm_dat )

    storm_dat <- dplyr::rename(storm_dat,
        latitude=latitude.N, longitude=longitude.E)

    return(storm_dat)
}

