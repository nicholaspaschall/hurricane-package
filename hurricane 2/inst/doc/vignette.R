## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(echo=TRUE)

## ----setup--------------------------------------------------------------------
library(hurricane)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
# track of 2020 storms
dat.2020 <- hurdat[grepl(2020, hurdat$date.time),]
storm_name <- unique(dat.2020$name)
year <- rep(2020, times=length(storm_name))
map.2020 <- track_map(storm_name, year, hurdat) + 
            theme(legend.text = element_text(size=5),
            legend.key.size = unit(0.3, "cm"),
            legend.title = element_text(size=1)) +
  labs(title="Storm Tracks of 2020")

print(map.2020)

## -----------------------------------------------------------------------------
# track of 2021 storms
dat.2021 <- hurdat[grepl(2021, hurdat$date.time),]
storm_name <- unique(dat.2021$name)
year <- rep(2021, times=length(storm_name))
map.2021 <- track_map(storm_name, year, hurdat) + 
            theme(legend.text = element_text(size=5),
            legend.key.size = unit(0.3, "cm"),
            legend.title = element_text(size=1)) +
  labs(title="Storm Tracks of 2021")
print(map.2021)

## -----------------------------------------------------------------------------
# track of 2022 storms
dat.2022 <- hurdat[grepl(2022, hurdat$date.time),]
storm_name <- unique(dat.2022$name)
year <- rep(2022, times=length(storm_name))
map.2022 <- track_map(storm_name, year, hurdat) + 
            theme(legend.text = element_text(size=5),
            legend.key.size = unit(0.3, "cm"),
            legend.title = element_text(size=1)) +
  labs(title="Storm Tracks of 2022")
print(map.2022)

## -----------------------------------------------------------------------------
katrina <- landfall("KATRINA", 2005, hurdat, method="landfall.strongest")
sandy <- landfall("SANDY", 2012, hurdat, method="landfall.strongest")
harvey <- landfall("HARVEY", 2017, hurdat, method="landfall.strongest")
ian <- landfall("IAN", 2022, hurdat, method="landfall.strongest")

storm_size( c("KATRINA", "SANDY", "HARVEY", "IAN"), 
        c(2005, 2012, 2017, 2022), hurdat, 
        c(katrina$date.time, sandy$date.time, harvey$date.time, ian$date.time),
        path = TRUE ) + 
  labs(title="Storm Sizes of Katrina, Sandy, Harvey, and Ian")

## -----------------------------------------------------------------------------
storm.id = unique(hurdat$id)
storm.name <- c()
storm.max.wind <- c()
storm.min.pressure <- c()
landfall.check <- c()
ACE <- c()

for( i in seq_along(storm.id) ){
  tmp.dat <- hurdat[ which( hurdat$id == storm.id[i] ),]
  storm.name[i] <- tmp.dat$name[1]
  storm.max.wind[i] <- max( tmp.dat$max.wind )
  storm.min.pressure[i] <- min(tmp.dat$min.pressure )
  tmp.year <- substr( storm.id[i], 5, 8)
  landfall.check[i] <- landfall( storm.id[i], tmp.year, hurdat, method="landfall.check")
  ACE[i] <- cyclone_energy( storm.id[i], tmp.year, hurdat)
}

storm.info <- data.frame(
  id = storm.id,
  name = storm.name,
  max.wind = storm.max.wind,
  min.pressure = storm.min.pressure,
  landfall = landfall.check,
  ace = ACE )

head(storm.info)

## -----------------------------------------------------------------------------
storm.name.dat <- data.frame( name = storm.info$name )%>%
    filter( name != "UNNAMED" ) %>%
    count( name )

ggplot( data = storm.name.dat, aes(x=name, y=n, label=name, color=n) ) +
    geom_text(size=2, angle=45) +
  theme_light() + scale_y_continuous(n.breaks=12) +
  labs(y="Frequency", x="Storm Names", title="Storm Name Frequencies",
       color="") +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank() ) +
  viridis::scale_color_viridis(option="turbo")

## -----------------------------------------------------------------------------
storm.dat.dis <- data.frame( id = unique(hurdat$id ) )

for( i in seq_along(storm.dat.dis$id) ){
    tmp.dat <- hurdat %>% filter( id == storm.dat.dis$id[i] )
    tmp.year <- substr( tmp.dat$id[1], 5, 8)
    storm <- interp_track( tmp.dat$id[1], tmp.year, hurdat ) %>%
        select( longitude, latitude )
    dis <- fields::rdist.earth.vec(
    storm[1:(nrow(storm)-1),], storm[2:nrow(storm),], miles=TRUE )
    storm.dat.dis$distance[i] <- sum(dis)
}
storm.dat.dis$distance <- ifelse(is.na(storm.dat.dis$distance), 0, 
                                 storm.dat.dis$distance)
storm.dat.dis$year <- substr(storm.dat.dis$id, 5, 8)

distance.df <- data.frame(year = as.numeric( unique(storm.dat.dis$year)) )
for( i in seq_along(unique(distance.df$year)) ){
    tmp.df <- storm.dat.dis %>% filter(year==unique(storm.dat.dis$year)[i])
    distance.df$avg.distance[i] <- mean(tmp.df$distance)
}

#summary( lm( avg.distance~year, data=distance.df ) )

ggplot(data=distance.df) +
  geom_col(aes(x=year, y=avg.distance, fill=avg.distance)) +
  stat_function(fun=function(year) -4522.249 + 3.519*year, 
                aes(color="Regression Line:\n-4522.249 + 3.519*year"), lwd=1.2) +
  scale_color_manual(values = "black", guide = guide_legend(title = "Regression Line")) +
  theme_light() +
  labs(x="Year", y="Average Distance",
       title="Average Distance Storm Travels per Year", color="",
       fill="Average Distance", subtitle="Distance recorded in miles") +
  scale_y_continuous(n.breaks=10) +
  scale_x_continuous(n.breaks=10) +
  theme(panel.grid.major=element_blank()) +
  viridis::scale_fill_viridis(option="turbo")

## -----------------------------------------------------------------------------
# gather range of years available in hurdat dataset
storm.year.range <- range(hurdat$date.time)
storm.years <- seq( 1851, 2022, by=1)

# construct data frame for landfall data
landfall.dat <- data.frame( year = storm.years )
for( i in 1:nrow(landfall.dat ) ){
  tmp.dat <- storm.info[ grepl( landfall.dat$year[i], storm.info$id), ]
  landfall.dat$freq[i] <- sum( tmp.dat$landfall )
  landfall.dat$total.storms[i] <- nrow( tmp.dat )
  landfall.dat$rate[i] <- landfall.dat$freq[i] / landfall.dat$total.storms[i]
}

## -----------------------------------------------------------------------------
landfall.hist <- ggplot(data=landfall.dat ) +
  geom_col(aes(x=year, y=freq, fill=cut(freq, 12))) +
  labs(x="Year", y="Landfall", 
       title="Landfalling Hurricanes per Year (1851-2022)", fill="Frequency") +
  theme_light() + 
  scale_y_continuous(n.breaks=7) + scale_x_continuous(n.breaks = 10)
landfall.hist

## -----------------------------------------------------------------------------
landfall.mod <- glm( freq ~ year, data=landfall.dat, family=poisson)
summary(landfall.mod)

## -----------------------------------------------------------------------------
ggplot( data=landfall.dat, aes(x=year, y=freq) ) +
  stat_function( aes(colour="Identity Link"), lwd=1.2,
                 fun=function(year) exp(-4.9510224 + 0.0032334*year ) ) +
  stat_function( aes(colour="Log Link"), lwd=1.2,
                 fun=function(year) -4.9510224 + 0.0032334*year ) +
  geom_point( aes(x=year, y=freq, fill="Observed Data")) +
  scale_y_continuous( n.breaks=8 ) + scale_x_continuous( n.breaks = 10 ) +
  theme_light() + labs( y="Landfall", x="Year",
                        title="Hurricane Landfalls per Year (1851-2022)",
                        color = "", fill="")

## -----------------------------------------------------------------------------
CI <- confint( landfall.mod ) 
exp( CI[2, ] )
exp( 25 * CI[2, ] )

## -----------------------------------------------------------------------------
ggplot( data=landfall.dat ) +
  geom_point( aes(x=year, y=total.storms, color=cut(total.storms, 5)) ) +
  geom_smooth(aes(x=year, y=total.storms), 
              method=lm, se=FALSE, group=1, colour="grey40", alpha=0.1) +
  labs(x="Year", y="Storms", 
       title="Storms per Year (1851-2022)", color="Total Storms") +
  scale_y_continuous(n.breaks=7) + scale_x_continuous(n.breaks=10) +
  theme_light() + theme(panel.grid.minor = element_blank())

## -----------------------------------------------------------------------------
# gather range of years available in hurdat dataset
storm.year.range <- range(hurdat$date.time)
storm.years <- seq( 1851, 2022, by=1)

# construct data frame for ACE data
cyclone.dat <- data.frame( year = storm.years )
for( i in 1:nrow( cyclone.dat ) ){
  tmp.dat <- storm.info[ grepl( cyclone.dat$year[i], storm.info$id), ]
  cyclone.dat$ace[i] <- sum( tmp.dat$ace )
}

## -----------------------------------------------------------------------------
ggplot(data=cyclone.dat ) +
  geom_path(aes(x=year, y=ace)) +
  labs(x="Year", y="Accumulated Cyclone Energy", 
       title="Average ACE per Year (1851-2022)") +
  scale_y_continuous(n.breaks=7) + scale_x_continuous(n.breaks=10) +
  theme_light() + theme(panel.grid.minor = element_blank())

## -----------------------------------------------------------------------------
cyclone.mod <- lm( ace ~ year, data=cyclone.dat )
summary(cyclone.mod)

## -----------------------------------------------------------------------------
ggplot(data=cyclone.dat ) +
  stat_function( fun=function(year) -654.78611 + 0.38882*year, 
                 aes(color="Regression Line") ) + 
  geom_point(aes(x=year, y=ace, fill="Total ACE")) +
  scale_y_continuous(n.breaks=7) + scale_x_continuous(n.breaks=10) +
  theme_light() + theme(panel.grid.minor = element_blank()) +
  labs(x="Year", y="Accumulated Cyclone Energy", 
       title="Total ACE per Year (1851-2022)",
       colour="", fill="")

## -----------------------------------------------------------------------------
( CI <- c(0.38882 - 1.96*0.08271, 0.38882 + 1.96*0.08271) )
( 25*CI )

