#' Atlantic Hurricane Database Information from the National Hurricane Center (NHC)
#'
#' A dataset including all available observations of hurricanes dating back to 1851.
#'
#' @format a dataframe with 53972 rows and 23 columns
#' \describe{
#'      \item{id}{storm id, representative of the basin of the hurricane (Atlantic), the hurricane number of the year, and the year it took place}
#'      \item{name}{name of the hurricane, if available, or else "UNNAMED}
#'      \item{date}{the date of the hurricane (YEAR-MONTH-DAY)}
#'      \item{UTC.time}{the time of the hurricane in Universal Time Coordinate.
#'                      Nearly all records correspond to the synoptic times of 0000, 0600, 1200, and 1800.}
#'      \item{record.id}{code used to identify records that correspond to landfalls or to indicate the reason for inclusion of a record
#'      not at the standard synoptic times (0000, 0600, 1200, 1800 UTC) }
#'      \item{storm.status}{status of system. Options include Tropical cyclone of tropical depression intensity (TD),
#'                        tropical cyclone of tropical storm intensity(TS), tropical cyclone of hurricane intensity(HU),
#'                        extratropical cyclone(EX), Subtropical cyclone of subtropical depression intensity(SD),
#'                        Subtropical cyclone of subtropical storm intensity(SS), A low that is neither a tropical cyclone, a subtropical cyclone,
#'                        nor an extratropical cyclone(LO), Tropical wave(WV), or Disturbance(DB)}
#'      \item{lattitude}{lattitude of where the hurricane took place}
#'      \item{longitude}{longitude of where the hurricane took place}
#'      \item{max.wind}{the maximum 1-min average wind (in knots) associated with the tropical cyclone at an
#'                      elevation of 10 m with an unobstructed exposure }
#'      \item{min.pressure}{minimum pressure in millibars}
#'      \item{34kt.wind.NE}{34 knot wind radii maximum extent in northeastern quadrant (in nautical miles)}
#'      \item{34kt.wind.SE}{34 knot wind radii maximum extent in southeastern quadrant (in nautical miles)}
#'      \item{34kt.wind.SW}{34 knot wind radii maximum extent in southwestern quadrant (in nautical miles)}
#'      \item{34kt.wind.NW}{34 knot wind radii maximum extent in northwestern quadrant (in nautical miles)}
#'      \item{50kt.wind.NE}{50 knot wind radii maximum extent in northeastern quadrant (in nautical miles)}
#'      \item{50kt.wind.SE}{50 knot wind radii maximum extent in southeastern quadrant (in nautical miles)}
#'      \item{50kt.wind.SW}{50 knot wind radii maximum extent in southwestern quadrant (in nautical miles)}
#'      \item{50kt.wind.NW}{50 knot wind radii maximum extent in northwestern quadrant (in nautical miles)}
#'      \item{64kt.wind.NE}{64 knot wind radii maximum extent in northeastern quadrant (in nautical miles)}
#'      \item{64kt.wind.SE}{64 knot wind radii maximum extent in southeastern quadrant (in nautical miles)}
#'      \item{64kt.wind.SW}{64 knot wind radii maximum extent in southwestern quadrant (in nautical miles)}
#'      \item{64kt.wind.NW}{64 knot wind radii maximum extent in northwestern quadrant (in nautical miles)}
#'      \item{radius.max.wind}{Radius of Maximum Wind (in nautical miles)}
#'      }
"hurdat"
