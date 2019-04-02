#Program to read in property data and add distance to the coastline.

library(sp)
library(plyr)
library(compiler)
options(scipen=999)

millionth <- 0
start <- millionth*1000000  
finish <- start+999999

if (Sys.info()["nodename"] == "SOM-SPIEGEL"){ #Yale office
	shorelinedir <- "F:/paper/Justin/Coastal Maps/"
	dist2shoredir <- "F:/paper/Justin/Coastal Maps/"
}else if(Sys.info()["nodename"] == "corelogic.som.yale.edu"){ #CoreLogic server
	shorelinedir <- "/data/CoastalMaps/"
	dist2shoredir <- "/data/CoastalMaps/"
}else if(Sys.info()["nodename"] == "SOM-D2UA6152PGC"){ #Home office
	shorelinedir <- "H:/paper/Justin/Coastal Maps/"
	dist2shoredir <- "H:/paper/Justin/Coastal Maps/"
}else if(Sys.info()["nodename"] == "research.som.yale.edu"){ #Research Server
	shorelinedir <- "/h5/aw678/Dist2Shore/"
	dist2shoredir <- "/h5/aw678/Dist2Shore/"
}

#xy.coast
xy.coast <- read.table(paste0(shorelinedir,"ShorelineLatLong.txt"),header=TRUE,stringsAsFactors=FALSE)
xy.coast <- xy.coast[,c("longitude","latitude")]
xy.coast <- na.omit(xy.coast)


# Dist2Coast calculation --------------------------------------------------

#Load minimaps
load(file=paste0(shorelinedir,"BoundingBox.Shoreline.East.rda"))


moe <- 0.2 #margin of error on bounding boxes when assigning to housing locations; use lower moe for faster code
choose_minimap <- function(lon,lat){
  mini <- c(which(BoundingBox$minlatitude <= lat+moe & BoundingBox$maxlatitude >= lat-moe & BoundingBox$minlongitude <= lon+moe & BoundingBox$maxlongitude >= lon-moe))
  if(length(mini)==0){
    #mini <- which.min(abs(BoundingBox$minlatitude - lat))
    mini <- NA
  }
  return(mini)
}

choose_minimap.cmp <- cmpfun(choose_minimap)
spDistsN1.cmp <- cmpfun(spDistsN1)

calc.dist2coast <- function(lon_local,lat_local){
  minimap_num <- choose_minimap.cmp(lon_local,lat_local)
  if(is.na(minimap_num)){
    return(NA)
  }else{
    joined_map <- rbind.fill(minimap[minimap_num])
    min_dist2coast <- min(spDistsN1.cmp(as.matrix(joined_map[,c("longitude","latitude")]),matrix(c(lon_local,lat_local),nrow=1), longlat = TRUE))
    return(min_dist2coast)
  }
}

calc.dist2coast.cmp <- cmpfun(calc.dist2coast)

## container for all the nearest points matching the input

east.coastal.coords <- read.table(paste0(dist2shoredir,"dist2shore.East.coastal.txt"),header=TRUE,stringsAsFactors=FALSE)
rawdat <- east.coastal.coords[,c(1,2,3)]
names(rawdat)<- c("CLID","longitude","latitude")
rawdat_lon <- rawdat[,"longitude"]
rawdat_lat <- rawdat[,"latitude"]
rawdat_CLID <- rawdat[,"CLID"]

outevery <- 10000

rawCoast.East.detailed <- rep(NA,outevery)
rawCoast.East.CLID <- rep(NA,outevery)
rawCoast.East.rowindex <- rep(NA,outevery)

once <- 0
counter <- 0

for (i in start:finish) {
	counter <- counter + 1
  
	#Choose relevant minimap using BoundingBox dataframe
	lon_local <- rawdat_lon[i]
	lat_local <- rawdat_lat[i]
	CLID_local <- rawdat_CLID[i]
	#Dist2Shore calculation
	min_dist2shore <- calc.dist2coast.cmp(lon_local,lat_local)

	#assign data to output vectors
	rawCoast.East.CLID[counter] <- CLID_local 
	rawCoast.East.detailed[counter] <- min_dist2shore
	rawCoast.East.rowindex[counter] <- i
	
	if (counter %% 1000 == 0|i == finish){
		cat("Row ",i,"; ",lon_local,lat_local,min_dist2shore,"\n")
		if (counter %% outevery == 0|i == finish){
		  rawCoast.CLID.dist2shore.bind <- data.frame(rawCoast.East.rowindex,rawCoast.East.CLID,rawCoast.East.detailed)
			if (once == 0){
				write.table(rawCoast.CLID.dist2shore.bind,paste0("dist2shore.east.detailed.withCLID.",millionth,"m.txt"),sep="\t",col.names=TRUE,row.names=FALSE,append=FALSE)
			  rawCoast.East.detailed <- rep(NA,outevery)
			  rawCoast.East.CLID <- rep(NA,outevery)
				once <- 1
				counter <- 0
      			}else{
      				write.table(rawCoast.CLID.dist2shore.bind,paste0("dist2shore.east.detailed.withCLID.",millionth,"m.txt"),sep="\t",col.names=FALSE,row.names=FALSE,append=TRUE)
      			  rawCoast.East.detailed <- rep(NA,outevery)
      			  rawCoast.East.CLID <- rep(NA,outevery)
      				counter <- 0
      			}
		}
	}
  
}

cat("Done!")

