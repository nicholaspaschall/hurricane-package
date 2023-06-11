world_map_helper <- function(lon.range=NULL, lat.range=NULL){
    # generates a base layer of world map based on previously computed
    # latitude and longitude values
    map <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data=ggplot2::map_data("world"),
            ggplot2::aes(x=long, y=lat, group=group),
                fill="white", color="black") +
        ggplot2::geom_polygon(data=ggplot2::map_data("state"),
            ggplot2::aes(x=long, y=lat, group=group),
                fill="white", color="black") +
        ggplot2::ylab("Latitude") + ggplot2::xlab("Longitude") +
        ggplot2::theme_linedraw() +
        ggplot2::theme(panel.background=ggplot2::element_rect(fill="lightblue1"),
            panel.grid=ggplot2::element_blank())

    if( !is.null(lon.range) & !is.null(lat.range)){
        map <- map + ggplot2::coord_fixed(ratio=1, xlim=c(lon.range),
            ylim=c(lat.range), expand=TRUE, clip="on")
    }
    return(map)
}







