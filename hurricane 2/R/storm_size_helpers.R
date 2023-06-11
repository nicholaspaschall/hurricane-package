distance_coordinates2 <- function( storm.dat, date.time ){
    # initialize data frame containing center point in degrees and radians
    distance.coordinates.dat <- data.frame(
        lat.degree = storm.dat$latitude,
        lon.degree = storm.dat$longitude,
        lat.radian = (storm.dat$latitude * pi/180),
        lon.radian = (storm.dat$longitude * pi/180) )

    # convert nautical miles to kilometers
    NE.dat <- storm.dat[ ,grepl("NE", colnames(storm.dat)) ] * 1.852
    SE.dat <- storm.dat[ ,grepl("SE", colnames(storm.dat)) ] * 1.852
    SW.dat <- storm.dat[ ,grepl("SW", colnames(storm.dat)) ] * 1.852
    NW.dat <- storm.dat[ ,grepl("NW", colnames(storm.dat)) ] * 1.852

    # compute latitude/longitudes based off previously computed distances and
    # center point (in radians)
    NE.lat.dat <- (NE.dat/6371)*cos(45*pi/180)
    NE.lon.dat <- (NE.dat/6371)*sin(45*pi/180)/
        cos(distance.coordinates.dat$lat.radian)
    SE.lat.dat <- (SE.dat/6371)*cos(45*pi/180)
    SE.lon.dat <- (SE.dat/6371)*sin(45*pi/180)/
        cos(distance.coordinates.dat$lat.radian)
    SW.lat.dat <- (SW.dat/6371)*cos(45*pi/180)
    SW.lon.dat <- (SW.dat/6371)*sin(45*pi/180)/
        cos(distance.coordinates.dat$lat.radian)
    NW.lat.dat <- (NW.dat/6371)*cos(45*pi/180)
    NW.lon.dat <- (NW.dat/6371)*sin(45*pi/180)/
        cos(distance.coordinates.dat$lat.radian)

    # compute longitude/latitude values of each radii in NE,SE,SW,NW quadrants
    NE.new.lat <- (NE.lat.dat + distance.coordinates.dat$lat.radian)*180/pi
    NE.new.lon <- (NE.lon.dat + distance.coordinates.dat$lon.radian)*180/pi
    NE.combined <- as.matrix( rbind(NE.new.lon, NE.new.lat) )

    SE.new.lat <- (-SE.lat.dat + distance.coordinates.dat$lat.radian)*180/pi
    SE.new.lon <- (SE.lon.dat + distance.coordinates.dat$lon.radian)*180/pi
    SE.combined <- as.matrix( rbind(SE.new.lon, SE.new.lat) )

    SW.new.lat <- (-SW.lat.dat + distance.coordinates.dat$lat.radian)*180/pi
    SW.new.lon <- (-SW.lon.dat + distance.coordinates.dat$lon.radian)*180/pi
    SW.combined <- as.matrix( rbind(SW.new.lon, SW.new.lat) )

    NW.new.lat <- (NW.lat.dat + distance.coordinates.dat$lat.radian)*180/pi
    NW.new.lon <- (-NW.lon.dat + distance.coordinates.dat$lon.radian)*180/pi
    NW.combined <- as.matrix( rbind(NW.new.lon, NW.new.lat) )

    mat <- cbind( NE.combined, SE.combined, NW.combined, SW.combined )
    rownames(mat) <- rep( c("longitude", "latitude"), times=nrow(mat)/2 )
    return(mat)
}




storm_size_interp_34 <- function( center.point,
    NE.point, SE.point, SW.point, NW.point ){

    lon.rad <- center.point[1] * pi/180
    lat.rad <- center.point[2] * pi/180
    dist.NE <- fields::rdist.earth(
        matrix(center.point, ncol=2), matrix(NE.point, ncol=2), miles=F, R=6371)
    dist.SE <- fields::rdist.earth(
        matrix(center.point, ncol=2), matrix(SE.point, ncol=2), miles=F, R=6371)
    dist.NW <- fields::rdist.earth(
        matrix(center.point, ncol=2), matrix(NW.point, ncol=2), miles=F, R=6371)
    dist.SW <- fields::rdist.earth(
        matrix(center.point, ncol=2), matrix(SW.point, ncol=2), miles=F, R=6371)

    # linear interpolation of storm size at 34 knots in southern quadrant
    S.quadrant.theta <- seq( 135, 225, length.out=100)
    dist.theta.S <- c()
    for( i in seq_along(S.quadrant.theta) ){
        dist.theta.S[i] <- dist.SE +
            (( S.quadrant.theta[i] - S.quadrant.theta[1] )/
            ( S.quadrant.theta[length(S.quadrant.theta )] -
            S.quadrant.theta[1])) *
            ( dist.SW - dist.SE )
    }
    lat.rad.theta.S <- (dist.theta.S/6371) * cos(S.quadrant.theta*pi/180)
    lon.rad.theta.S <- (dist.theta.S/6371) * sin(S.quadrant.theta*pi/180) /
        cos(lat.rad)
    lat.theta.S <- (lat.rad.theta.S + lat.rad) * (180/pi)
    lon.theta.S <- (lon.rad.theta.S + lon.rad) * (180/pi)


    # linear interpolation of storm size at 34 knots in western quadrant
    W.quadrant.theta <- seq( 225, 315, length.out=100)
    dist.theta.W <- c()
    for( i in seq_along(W.quadrant.theta) ){
      dist.theta.W[i] <- dist.SW +
          (( W.quadrant.theta[i] - W.quadrant.theta[1] )/
          ( W.quadrant.theta[length(W.quadrant.theta )] -
          W.quadrant.theta[1])) *
          (dist.NW - dist.SW)
    }
    lat.rad.theta.W <- (dist.theta.W/6371) * cos(W.quadrant.theta*pi/180)
    lon.rad.theta.W <- (dist.theta.W/6371) * sin(W.quadrant.theta*pi/180) /
         cos(lat.rad)
    lat.theta.W <- (lat.rad.theta.W + lat.rad) * (180/pi)
    lon.theta.W <- (lon.rad.theta.W + lon.rad) * (180/pi)


    # linear interpolation of storm size at 34 knots in northern quadrant
    N.quadrant.theta <- seq(315, 405, length.out=100)
    dist.theta.N <- c()
    for( i in seq_along(N.quadrant.theta) ){
      dist.theta.N[i] <- dist.NW +
          ((N.quadrant.theta[i] - N.quadrant.theta[1] )/
          ( N.quadrant.theta[length(N.quadrant.theta )] -
          N.quadrant.theta[1])) *
          (dist.NE - dist.NW)
    }
    lat.rad.theta.N <- (dist.theta.N/6371) * cos(N.quadrant.theta*pi/180)
    lon.rad.theta.N <- (dist.theta.N/6371) * sin(N.quadrant.theta*pi/180) /
        cos(lat.rad)
    lat.theta.N <- (lat.rad.theta.N + lat.rad) * (180/pi)
    lon.theta.N <- (lon.rad.theta.N + lon.rad) * (180/pi)


    # linear interpolation of storm size at 34 knots in eastern quadrant
    E.quadrant.theta <- seq(90, 135, length.out=100)
    dist.theta.E <- c()
    for( i in seq_along(E.quadrant.theta) ){
        dist.theta.E[i] <- dist.NE +
        ((E.quadrant.theta[i] - E.quadrant.theta[1] )/
        ( E.quadrant.theta[length(E.quadrant.theta )] -
        E.quadrant.theta[1])) *
        (dist.SE - dist.NE)
    }
    lat.rad.theta.E <- (dist.theta.E/6371) * cos(E.quadrant.theta*pi/180)
    lon.rad.theta.E <- (dist.theta.E/6371) * sin(E.quadrant.theta*pi/180) /
        cos(lat.rad)
    lat.theta.E <- (lat.rad.theta.E + lat.rad) * (180/pi)
    lon.theta.E <- (lon.rad.theta.E + lon.rad) * (180/pi)

    return(
      data.frame(
          lon.theta = c(lon.theta.S, lon.theta.W, lon.theta.N, lon.theta.E),
          lat.theta = c(lat.theta.S, lat.theta.W, lat.theta.N, lat.theta.E)
      )
    )
}


storm_size_interp_50 <- function( center.point,
    NE.point, SE.point, SW.point, NW.point ){

    lon.rad <- center.point[1] * pi/180
    lat.rad <- center.point[2] * pi/180
    dist.NE <- fields::rdist.earth(
        matrix(center.point, ncol=2), matrix(NE.point, ncol=2), miles=F, R=6371)
    dist.SE <- fields::rdist.earth(
        matrix(center.point, ncol=2), matrix(SE.point, ncol=2), miles=F, R=6371)
    dist.NW <- fields::rdist.earth(
        matrix(center.point, ncol=2), matrix(NW.point, ncol=2), miles=F, R=6371)
    dist.SW <- fields::rdist.earth(
        matrix(center.point, ncol=2), matrix(SW.point, ncol=2), miles=F, R=6371)


    # linear interpolation of storm size at 50 knots in southern quadrant
    S.quadrant.theta <- seq( 135, 225, length.out=100)
    dist.theta.S <- c()
    for( i in seq_along(S.quadrant.theta) ){
      dist.theta.S[i] <- dist.SE +
          (( S.quadrant.theta[i] - S.quadrant.theta[1] )/
          ( S.quadrant.theta[length(S.quadrant.theta )] -
          S.quadrant.theta[1])) *
          ( dist.SW - dist.SE )
    }
    lat.rad.theta.S <- (dist.theta.S/6371) * cos(S.quadrant.theta*pi/180)
    lon.rad.theta.S <- (dist.theta.S/6371) * sin(S.quadrant.theta*pi/180) /
        cos(lat.rad)
    lat.theta.S <- (lat.rad.theta.S + lat.rad) * (180/pi)
    lon.theta.S <- (lon.rad.theta.S + lon.rad) * (180/pi)


    # linear interpolation of storm size at 50 knots in western quadrant
    W.quadrant.theta <- seq( 225, 315, length.out=100)
    dist.theta.W <- c()
    for( i in seq_along(W.quadrant.theta) ){
        dist.theta.W[i] <- dist.SW +
            (( W.quadrant.theta[i] - W.quadrant.theta[1] )/
            ( W.quadrant.theta[length(W.quadrant.theta )] -
            W.quadrant.theta[1])) *
            (dist.NW - dist.SW)
    }
    lat.rad.theta.W <- (dist.theta.W/6371) * cos(W.quadrant.theta*pi/180)
    lon.rad.theta.W <- (dist.theta.W/6371) * sin(W.quadrant.theta*pi/180) /
        cos(lat.rad)
    lat.theta.W <- (lat.rad.theta.W + lat.rad) * (180/pi)
    lon.theta.W <- (lon.rad.theta.W + lon.rad) * (180/pi)


    # linear interpolation of storm size at 50 knots in northern quadrant
    N.quadrant.theta <- seq(315, 405, length.out=100)
    dist.theta.N <- c()
    for( i in seq_along(N.quadrant.theta) ){
        dist.theta.N[i] <- dist.NW +
            ((N.quadrant.theta[i] - N.quadrant.theta[1] )/
            ( N.quadrant.theta[length(N.quadrant.theta )] -
            N.quadrant.theta[1])) *
            (dist.NE - dist.NW)
    }
    lat.rad.theta.N <- (dist.theta.N/6371) * cos(N.quadrant.theta*pi/180)
    lon.rad.theta.N <- (dist.theta.N/6371) * sin(N.quadrant.theta*pi/180) /
        cos(lat.rad)
    lat.theta.N <- (lat.rad.theta.N + lat.rad) * (180/pi)
    lon.theta.N <- (lon.rad.theta.N + lon.rad) * (180/pi)


    # linear interpolation of storm size at 50 knots in eastern quadrant
    E.quadrant.theta <- seq(90, 135, length.out=100)
    dist.theta.E <- c()
    for( i in seq_along(E.quadrant.theta) ){
        dist.theta.E[i] <- dist.NE +
        ((E.quadrant.theta[i] - E.quadrant.theta[1] )/
        ( E.quadrant.theta[length(E.quadrant.theta )] -
        E.quadrant.theta[1])) *
        (dist.SE - dist.NE)
    }
    lat.rad.theta.E <- (dist.theta.E/6371) * cos(E.quadrant.theta*pi/180)
    lon.rad.theta.E <- (dist.theta.E/6371) * sin(E.quadrant.theta*pi/180) /
        cos(lat.rad)
    lat.theta.E <- (lat.rad.theta.E + lat.rad) * (180/pi)
    lon.theta.E <- (lon.rad.theta.E + lon.rad) * (180/pi)

    return(
      data.frame(
          lon.theta = c(lon.theta.S, lon.theta.W, lon.theta.N, lon.theta.E),
          lat.theta = c(lat.theta.S, lat.theta.W, lat.theta.N, lat.theta.E)
        )
      )
}


storm_size_interp_64 <- function( center.point,
    NE.point, SE.point, SW.point, NW.point ){

    lon.rad <- center.point[1] * pi/180
    lat.rad <- center.point[2] * pi/180
    dist.NE <- fields::rdist.earth(
        matrix(center.point, ncol=2), matrix(NE.point, ncol=2), miles=F, R=6371)
    dist.SE <- fields::rdist.earth(
        matrix(center.point, ncol=2), matrix(SE.point, ncol=2), miles=F, R=6371)
    dist.NW <- fields::rdist.earth(
        matrix(center.point, ncol=2), matrix(NW.point, ncol=2), miles=F, R=6371)
    dist.SW <- fields::rdist.earth(
        matrix(center.point, ncol=2), matrix(SW.point, ncol=2), miles=F, R=6371)


    # linear interpolation of storm size at 64 knots in southern quadrant
    S.quadrant.theta <- seq( 135, 225, length.out=100)
    dist.theta.S <- c()
    for( i in seq_along(S.quadrant.theta) ){
        dist.theta.S[i] <- dist.SE +
            (( S.quadrant.theta[i] - S.quadrant.theta[1] )/
            ( S.quadrant.theta[length(S.quadrant.theta )] -
            S.quadrant.theta[1])) *
            ( dist.SW - dist.SE )
    }
    lat.rad.theta.S <- (dist.theta.S/6371) * cos(S.quadrant.theta*pi/180)
    lon.rad.theta.S <- (dist.theta.S/6371) * sin(S.quadrant.theta*pi/180) /
        cos(lat.rad)
    lat.theta.S <- (lat.rad.theta.S + lat.rad) * (180/pi)
    lon.theta.S <- (lon.rad.theta.S + lon.rad) * (180/pi)


    # linear interpolation of storm size at 64 knots in western quadrant
    W.quadrant.theta <- seq( 225, 315, length.out=100)
    dist.theta.W <- c()
    for( i in seq_along(W.quadrant.theta) ){
        dist.theta.W[i] <- dist.SW +
            (( W.quadrant.theta[i] - W.quadrant.theta[1] )/
            ( W.quadrant.theta[length(W.quadrant.theta )] -
            W.quadrant.theta[1])) *
            (dist.NW - dist.SW)
    }
    lat.rad.theta.W <- (dist.theta.W/6371) * cos(W.quadrant.theta*pi/180)
    lon.rad.theta.W <- (dist.theta.W/6371) * sin(W.quadrant.theta*pi/180) /
        cos(lat.rad)
    lat.theta.W <- (lat.rad.theta.W + lat.rad) * (180/pi)
    lon.theta.W <- (lon.rad.theta.W + lon.rad) * (180/pi)


    # linear interpolation of storm size at 64 knots in northern quadrant
    N.quadrant.theta <- seq(315, 405, length.out=100)
    dist.theta.N <- c()
    for( i in seq_along(N.quadrant.theta) ){
        dist.theta.N[i] <- dist.NW +
            ((N.quadrant.theta[i] - N.quadrant.theta[1] )/
            ( N.quadrant.theta[length(N.quadrant.theta )] -
            N.quadrant.theta[1])) *
            (dist.NE - dist.NW)
    }
    lat.rad.theta.N <- (dist.theta.N/6371) * cos(N.quadrant.theta*pi/180)
    lon.rad.theta.N <- (dist.theta.N/6371) * sin(N.quadrant.theta*pi/180) /
        cos(lat.rad)
    lat.theta.N <- (lat.rad.theta.N + lat.rad) * (180/pi)
    lon.theta.N <- (lon.rad.theta.N + lon.rad) * (180/pi)


    # linear interpolation of storm size at 64 knots in eastern quadrant
    E.quadrant.theta <- seq(90, 135, length.out=100)
    dist.theta.E <- c()
    for( i in seq_along(E.quadrant.theta) ){
        dist.theta.E[i] <- dist.NE +
            ((E.quadrant.theta[i] - E.quadrant.theta[1] )/
            ( E.quadrant.theta[length(E.quadrant.theta )] -
            E.quadrant.theta[1])) *
            (dist.SE - dist.NE)
    }
    lat.rad.theta.E <- (dist.theta.E/6371) * cos(E.quadrant.theta*pi/180)
    lon.rad.theta.E <- (dist.theta.E/6371) * sin(E.quadrant.theta*pi/180) /
        cos(lat.rad)
    lat.theta.E <- (lat.rad.theta.E + lat.rad) * (180/pi)
    lon.theta.E <- (lon.rad.theta.E + lon.rad) * (180/pi)

    return(
        data.frame(
          lon.theta = c(lon.theta.S, lon.theta.W, lon.theta.N, lon.theta.E),
          lat.theta = c(lat.theta.S, lat.theta.W, lat.theta.N, lat.theta.E)
        )
      )
}
