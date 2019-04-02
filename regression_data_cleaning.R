library(odbc)
library(DBI)
library(sp)
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={ODBC Driver 17 for SQL Server};server=corelogic.som.yale.edu;database=Tax_RLO;uid=aw678;pwd=43Jzrutv42@<28")
q <- dbSendQuery(con,"SELECT * FROM dist2shore_east;") #or dist2shore_west
df <- dbFetch(q)

#****MAKE SURE CENSUS_TRACT AND FIPS_CODE ARE READ IN AS CHARACTERS, NOT NUMBERS****

#TRANSACTION TYPE, PROPERTY INDICATOR, RESIDENTIAL MODEL INDICATOR
df <- df[df$TRANSACTION_TYPE=="1" | df$TRANSACTION_TYPE=="3" | df$TRANSACTION_TYPE=="",]
df <- df[df$PROPERTY_INDICATOR =="10" | df$RESIDENTIAL_MODEL_INDICATOR=="Y",]

#SALE_YEAR 
df <- df[!is.na(df$SALE_DATE),]
df <- df[df$SALE_DATE > 9999,] #eliminate incorrectly coded sale dates
df$SALE_YEAR <- substr(as.character(df$SALE_DATE),1,4)
df$SALE_YEAR <- as.numeric(df$SALE_YEAR)

#AGE AT SALE/YEAR_BUILT_MERGED
df$YEAR_BUILT_MERGED <- with(df,ifelse(EFFECTIVE_YEAR_BUILT==0,YEAR_BUILT,EFFECTIVE_YEAR_BUILT)) #merge all available year built data
df <- df[df$YEAR_BUILT_MERGED!=0,]
df$AGE_AT_SALE <- df$SALE_YEAR - df$YEAR_BUILT_MERGED
df <- df[df$AGE_AT_SALE >= -2,]
df <- df[!(df$AGE_AT_SALE <=0 & (df$YEAR_BUILT != df$EFFECTIVE_YEAR_BUILT)),]
#NEW CONSTRUCTION
df$NEW_CONSTRUCTION <- df$AGE_AT_SALE <= 0 #Binary NEW CONSTRUCTION variable
df$AGE_AT_SALE[df$AGE_AT_SALE > 0] <- df$AGE_AT_SALE[df$AGE_AT_SALE > 0] + 1 #prep for next step
df$AGE_AT_SALE[df$AGE_AT_SALE<=0] <- 1 #Replace age of all houses where age <= 0 with 1 so we can take log()
df$YEAR_BUILT_MERGED <- as.factor(df$YEAR_BUILT_MERGED)


#elevation_WGS84
df$BELOW_SEA_LEVEL <- df$elevation_WGS84 <= 0
df$elevation_WGS84[df$elevation_WGS84>0] <- df$elevation_WGS84[df$elevation_WGS84>0] + 1 #prep for next step
df$elevation_WGS84[df$elevation_WGS84<=0] <- 1 #Replace all elevation <=0 with 1 so we can take log()

#BEDROOMS
df <- df[!is.na(df$BEDROOMS),]
df <- df[df$BEDROOMS < 15,]

#TOTAL_BATHS_MERGED_ROUNDED
df$TOTAL_BATHS_MERGED <- with(df,ifelse(TOTAL_BATHS==0,TOTAL_BATHS_CALCULATED,TOTAL_BATHS))
df$TOTAL_BATHS_MERGED_ROUNDED <- round(df$TOTAL_BATHS_MERGED*4)/4
df <- df[!is.na(df$TOTAL_BATHS_MERGED_ROUNDED),]
df <- df[df$TOTAL_BATHS_MERGED_ROUNDED < 15,]

#BED AND BATHS AS FACTORS
df$TOTAL_BATHS_MERGED_ROUNDED <- as.factor(df$TOTAL_BATHS_MERGED_ROUNDED)
levels(df$TOTAL_BATHS_MERGED_ROUNDED)[which(levels(df$TOTAL_BATHS_MERGED_ROUNDED)=='10'):length(levels(df$TOTAL_BATHS_MERGED_ROUNDED))] <- '10'  #group all baths 10+ as '10'
df$BEDROOMS <- as.factor(df$BEDROOMS) #convert to factor for dummy variable
levels(df$BEDROOMS)[10:length(levels(df$BEDROOMS))] <- '10' #group all bedrooms 10+ as '10'

#POOL
df$POOL[df$POOL==""] <- "N"

#CONVERT SOME MORE COLUMNS TO FACTORS
df$YEAR_BUILT_MERGED <- as.factor(df$YEAR_BUILT_MERGED)
df$POOL <- as.factor(df$POOL)

#REMOVE ROWS WITH VALUES TOO LOW
df <- df[df$SALE_PRICE>=1000,]
df <- df[df$ACRES>0,]
df <- df[df$UNIVERSAL_BUILDING_SQUARE_FEET>100,]

#SHORELINE_HOME
df$SHORELINE_HOME <- df$dist2shore <= (sqrt(df$ACRES*4046.86))/1000#acres converted to square meters (1 acres = 4046.86 meters squared)

#ADD 1 FOR LOG()
df$ACRES <- df$ACRES + 1
df$dist2shore <- df$dist2shore+1

#SLR
df <- df[!is.na(df$slr),]

#CENSUS_TRACT AND UNIQUE_CENSUS_TRACT
df$CENSUS_TRACT <- substr(df$CENSUS_TRACT,1,6)
df$UNIQUE_CENSUS_TRACT <- paste0(df$FIPS_CODE,'_',df$CENSUS_TRACT)



##AGGREGATING UNIQUE_CENSUS_TRACT INTO CLUSTERS IN WHICH NO. HOUSES >= 1000 FOR PURPOSE OF REGRESSION
#FIRST FIND LON,LAT OF CENTROIDS OF EACH UNIQUE_CENSUS TRACT BASED ON POPULATION CENTER
unique_tracts <- unique(df$UNIQUE_CENSUS_TRACT)
tracts_centroids.df <- data.frame(UNIQUE_CENSUS_TRACT=unique_tracts,CENTROID_LON=numeric(length(unique_tracts)),CENTROID_LAT=numeric(length(unique_tracts)))

tract_pop_centers <- read.table("CenPop2010_Mean_TR.txt",header=T,sep=",",colClasses=c(rep("character",3),rep("numeric",3)))
tract_pop_centers$UNIQUE_CENSUS_TRACT <- with(tract_pop_centers,paste0(STATEFP,COUNTYFP,"_",TRACTCE))

tracts_centroids.df$CENTROID_LON <- tract_pop_centers$LONGITUDE[match(tracts_centroids.df$UNIQUE_CENSUS_TRACT,tract_pop_centers$UNIQUE_CENSUS_TRACT)]
tracts_centroids.df$CENTROID_LAT <- tract_pop_centers$LATITUDE[match(tracts_centroids.df$UNIQUE_CENSUS_TRACT,tract_pop_centers$UNIQUE_CENSUS_TRACT)]

#MATCH LON,LAT OF CENTROIDS TO UNIQUE_CENSUS_TRACT IN MAIN REGRESSION DATA TABLE
df$CENSUS_TRACT_CENTROID_LON <- tracts_centroids.df$CENTROID_LON[match(df$UNIQUE_CENSUS_TRACT,tracts_centroids.df$UNIQUE_CENSUS_TRACT)]
df$CENSUS_TRACT_CENTROID_LAT <- tracts_centroids.df$CENTROID_LAT[match(df$UNIQUE_CENSUS_TRACT,tracts_centroids.df$UNIQUE_CENSUS_TRACT)]

df <- df[!is.na(df$CENSUS_TRACT_CENTROID_LON),] #some census tracts in CoastalStates_Tax do not match up with their SITUS_STATE or FIPS_CODE

#update tract_pop_centers to only include UNIQUE_CENSUS_TRACT that are also in df
tract_pop_centers <- tract_pop_centers[tract_pop_centers$UNIQUE_CENSUS_TRACT %in% df$UNIQUE_CENSUS_TRACT,]
#isolate UNIQUE_CENSUS_TRACT,LAT,LON
TRACTS_LIST <<- tract_pop_centers[,c('UNIQUE_CENSUS_TRACT','LONGITUDE','LATITUDE')]
#frequency table of unique census tracts
UNIQUE_TRACT_FREQUENCY <- data.frame(table(df$UNIQUE_CENSUS_TRACT),stringsAsFactors = F)
colnames(UNIQUE_TRACT_FREQUENCY) <- c('UNIQUE_CENSUS_TRACT','FREQUENCY')
UNIQUE_TRACT_FREQUENCY$UNIQUE_CENSUS_TRACT <- as.character(UNIQUE_TRACT_FREQUENCY$UNIQUE_CENSUS_TRACT)

#aggregate closest census tracts until no. homes >= 1000
AGGREGATED_CENSUS_TRACTS <<- data.frame(UNIQUE_CENSUS_TRACT=character(),AGGREGATE_TRACT_ASSIGNMENT=character())

#function to calculate nearest tract
nearest_tract <- function(aggregate_tract){
  dist_to_tracts <- spDistsN1(as.matrix(TRACTS_LIST[,c('LONGITUDE','LATITUDE')]),matrix(c(aggregate_tract$LONGITUDE,aggregate_tract$LATITUDE),nrow=1),longlat=TRUE) #all distances between aggregate tract and remaining tracts
  nearest_neighbor_idx <- which.min(dist_to_tracts) #index of nearest tract base on distance between population centers
  nearest_neighbor <- TRACTS_LIST[nearest_neighbor_idx,] 
  
  AGGREGATED_CENSUS_TRACTS <<- rbind(AGGREGATED_CENSUS_TRACTS,data.frame(UNIQUE_CENSUS_TRACT=nearest_neighbor$UNIQUE_CENSUS_TRACT,AGGREGATE_TRACT_ASSIGNMENT=aggregate_tract$UNIQUE_CENSUS_TRACT)) #add row to aggregate df containing unique tract and its assigned aggregate tract
  
  TRACTS_LIST <<- TRACTS_LIST[-nearest_neighbor_idx,] #remove assigned tract from tract list
  
}
while(nrow(TRACTS_LIST)>0){
  if(nrow(TRACTS_LIST) %% 1000 == 0){
    cat("Tracts left: ",nrow(TRACTS_LIST),"\n")
  }
  aggregate_tract <- TRACTS_LIST[1,] #new aggregate census tract 
  AGGREGATED_CENSUS_TRACTS <- rbind(AGGREGATED_CENSUS_TRACTS,data.frame(UNIQUE_CENSUS_TRACT=aggregate_tract$UNIQUE_CENSUS_TRACT,AGGREGATE_TRACT_ASSIGNMENT=aggregate_tract$UNIQUE_CENSUS_TRACT)) #add top tract to aggregate table as its own assigned aggregate tract
  TRACTS_LIST <- TRACTS_LIST[-1,]#remove top tract from tracts list
  
  tracts_in_aggregate <- AGGREGATED_CENSUS_TRACTS$UNIQUE_CENSUS_TRACT[AGGREGATED_CENSUS_TRACTS$AGGREGATE_TRACT_ASSIGNMENT==aggregate_tract$UNIQUE_CENSUS_TRACT] #unique census tracts that have been assigned the aggregate census tract
  nrow_aggregated_tract <- sum(UNIQUE_TRACT_FREQUENCY$FREQUENCY[UNIQUE_TRACT_FREQUENCY$UNIQUE_CENSUS_TRACT %in% tracts_in_aggregate]) #check if number of houses in aggregated tract is now >= 1000
  while(nrow_aggregated_tract < 1000){
    #add another tract to aggregate tracts
    nearest_tract(aggregate_tract)
    #update count of homes in aggregated tracts
    tracts_in_aggregate <- AGGREGATED_CENSUS_TRACTS$UNIQUE_CENSUS_TRACT[AGGREGATED_CENSUS_TRACTS$AGGREGATE_TRACT_ASSIGNMENT==aggregate_tract$UNIQUE_CENSUS_TRACT]
    nrow_aggregated_tract <- sum(UNIQUE_TRACT_FREQUENCY$FREQUENCY[UNIQUE_TRACT_FREQUENCY$UNIQUE_CENSUS_TRACT %in% tracts_in_aggregate])
  }
}
AGGREGATED_CENSUS_TRACTS$UNIQUE_CENSUS_TRACT <- as.character(AGGREGATED_CENSUS_TRACTS$UNIQUE_CENSUS_TRACT)
AGGREGATED_CENSUS_TRACTS$AGGREGATE_TRACT_ASSIGNMENT <- as.character(AGGREGATED_CENSUS_TRACTS$AGGREGATE_TRACT_ASSIGNMENT)

#add column of aggregate tract assignments to main regression df
df$AGGREGATED_CENSUS_TRACT_ASSIGNMENT <- AGGREGATED_CENSUS_TRACTS$AGGREGATE_TRACT_ASSIGNMENT[match(df$UNIQUE_CENSUS_TRACT,AGGREGATED_CENSUS_TRACTS$UNIQUE_CENSUS_TRACT)]
