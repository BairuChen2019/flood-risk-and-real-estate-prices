# Retrieve and clean shorepoint data ----------------------------------------------
## Visualize shorepoint data by mapping
library(ggplot2)
library(ggmap)

datadir <- "/Users/alissa/Desktop/0_SOM_Project/Dist2Coast/Shoreline/"

# West Coast --------------------------------------------------------------

#random sample of West Coast shore points
west.coast <- read.table("WesternCoastLatLong.txt",stringsAsFactors=FALSE)
names(west.coast) <- c("latitude","longitude")
west.coast <- west.coast[-1,]
west.coast <- west.coast[complete.cases(west.coast),]
west.coast$latitude <- as.numeric(west.coast$latitude)
west.coast$longitude <- as.numeric(west.coast$longitude) 

states <- map_data("state")
states_map <- ggplot()+geom_polygon(data=states, aes(x=long, y=lat, group=group), fill=NA, color="black",size=0.1)+coord_fixed(1.3)
wc_sample <- sample(1:2749968,1000000,replace=FALSE)
mapped_wc_points <- states_map + geom_point(data=west.coast[wc_sample,],aes(x=longitude,y=latitude),col="blue",size=0.5)

west.coast.clean <- read.table("West_Coast.txt",stringsAsFactors=FALSE)
west.coast.clean <- west.coast.clean[-1,]
west.coast.clean[,1] <- as.numeric(west.coast.clean[,1])
west.coast.clean[,2] <- as.numeric(west.coast.clean[,2])


# East Coast --------------------------------------------------------------

#southeast
southeast <- read.table(paste0(datadir,"Southeast_Caribbean.txt"),stringsAsFactors=FALSE)
names(southeast) <- c("latitude","longitude")
southeast <- southeast[-1,]
southeast <- southeast[complete.cases(southeast),]
southeast$latitude <- as.numeric(southeast$latitude)
southeast$longitude <- as.numeric(southeast$longitude)

#clean southeast
library(dplyr)
inland1 <- southeast[southeast$long < -81.24 & southeast$long > -82.5 & southeast$lat > 28 & southeast$lat < 30,]
southeast_clean <- anti_join(southeast, inland1, by=c("latitude","longitude"))
inland2 <- southeast_clean[southeast_clean$longitude > -81.25 & southeast_clean$longitude < -80.2 & southeast_clean$latitude < 27.25 & southeast_clean$latitude > 26.5,]
southeast_clean <- anti_join(southeast_clean, inland2, by=c("latitude","longitude"))
inland_ga_1 <- southeast_clean[southeast_clean$latitude < 34 & southeast_clean$latitude > 32.04 & southeast_clean$longitude < -80.91 & southeast_clean$longitude > -82.5,]
southeast_clean <- anti_join(southeast_clean, inland_ga_1, by=c("latitude","longitude"))
se_clean_samp <- sample(1:nrow(southeast_clean),1000000,replace=FALSE)
(mapped_southeast_clean <- states_map + geom_point(data=southeast_clean[se_clean_samp,],aes(x=longitude,y=latitude),col="blue",size=0.5) + coord_fixed(xlim=c(-85,-75),ylim=c(24,37),ratio=1.3))

#northatlantic
north.atl <- read.table(paste0(datadir,"North_Atlantic.txt"),stringsAsFactors=FALSE)
names(north.atl) <- c("latitude","longitude")
north.atl <- north.atl[-1,]
north.atl <- north.atl[complete.cases(north.atl),]
north.atl$latitude <- as.numeric(north.atl$latitude)
north.atl$longitude <- as.numeric(north.atl$longitude)
#clean north atl
inland_ny <- north.atl[north.atl$latitude < 44.5 & north.atl$latitude > 41.5 & north.atl$longitude > -76 & north.atl$longitude < -73,]
north.atl_clean <- anti_join(north.atl, inland_ny, by=c("latitude","longitude"))
north.atl_clean_samp <- sample(1:nrow(north.atl_clean),500000,replace=FALSE)
(mapped_north.atl_clean <- states_map + geom_point(data=north.atl_clean[north.atl_clean_samp,],aes(x=longitude,y=latitude),col="blue",size=0.5) + coord_fixed(xlim=c(-76,-72.5),ylim=c(40,45),ratio=1.3))

#north atl and southeast joined into east coast
east.coast <- rbind(north.atl_clean,southeast_clean)
east.coast <- east.coast[east.coast$latitude > 20,]
sample.east.coast <- sample(1:nrow(east.coast),1000000,replace=FALSE)
(mapped_east.coast <- states_map + geom_point(data=east.coast[sample.east.coast,],aes(x=longitude,y=latitude),col="blue",size=0.5) + coord_fixed(ratio=1.3))

#gulf coast
gulf.coast <- read.table(paste0(datadir,"gulf.coast.txt"),stringsAsFactors=FALSE)
gulf.coast <- gulf.coast[-c(1,2),]
gulf.coast$latitude <- as.character(gulf.coast$latitude)
gulf.coast$longitude <- as.character(gulf.coast$longitude)
gulf.coast$latitude <- as.numeric(gulf.coast$latitude)
gulf.coast$longitude <- as.numeric(gulf.coast$longitude)
gulf.coast <- na.omit(gulf.coast)
names(gulf.coast) <- c("latitude","longitude")
sample.gulf <- sample(1:nrow(gulf.coast),1000000,replace=FALSE)
states_map + geom_point(data=gulf.coast[sample.gulf,],aes(x=longitude,y=latitude),col="blue",size=0.4)+coord_fixed(ratio=1.3)
#missing FL west coordinates
fl.west <- read.table("/home/aw678/Shoreline/FL_west.txt")
fl.west <- na.omit(fl.west)
gulf.coast <- rbind(gulf.coast,fl.west)
fl_inland <- with(gulf.coast,gulf.coast[longitude < -80.5 & longitude > -82 & latitude > 26.5 & latitude < 27.5,])
gulf.coast <- anti_join(gulf.coast,fl_inland,by=c("longitude","latitude"))
#join gulf.coast with east.coast
east.coast <- rbind(east.coast,gulf.coast)

#CRUDE MAP ne.coastline.us
ne.coastline.us <- read.table("ne.coastline.us.txt",stringsAsFactors=FALSE)
(ne.map <- states_map + geom_point(data=ne.coastline.us,aes(x=longitude,y=latitude),col="blue",size=0.5) + coord_fixed(xlim=c(-130,-60),ylim=c(20,53),ratio=1.3))
ne.map + coord_fixed(xlim=c(-67.5,-65),ylim=c(44.6,46))
states_map + geom_point(data=canada,aes(x=longitude,y=latitude),col="blue",size=0.3)+coord_fixed(xlim=c(-81,-65),ylim=c(42.5,52.5),ratio=1.3)
#remove points in mexico
mexico <- with(ne.coastline.us,ne.coastline.us[longitude > -117 & longitude < -98 & latitude < 32.5,])
ne.coastline.us <- anti_join(ne.coastline.us,mexico,by=c("latitude","longitude"))
ne.coastline.us <- anti_join(ne.coastline.us,with(ne.coastline.us,ne.coastline.us[longitude < -115 & longitude > -120 & latitude < 30,]))
#remove islands east of florida
isl_fl <- with(ne.coastline.us,ne.coastline.us[longitude > -79.5&longitude < -72& latitude < 27& latitude > 23,])
ne.coastline.us <- anti_join(ne.coastline.us,isl_fl,by=c("latitude","longitude"))
#remove points in canada
canada <- with(ne.coastline.us,ne.coastline.us[longitude < -67.5&longitude > -75 & latitude > 44.9,])
ne.coastline.us <- anti_join(ne.coastline.us,canada,by=c("latitude","longitude"))
ne.coastline.us <- anti_join(ne.coastline.us,with(ne.coastline.us,ne.coastline.us[longitude > -67.6&longitude < -65& latitude > 47.5,]),by=c("latitude","longitude"))
ne.coastline.us <- anti_join(ne.coastline.us,with(ne.coastline.us,ne.coastline.us[longitude > -66.6 & latitude > 45,]),by=c("latitude","longitude"))