library(httr)
library(dplyr)
library(jsonlite)
library(leaflet)
driveBC_map_major <- function(days,region=NA){
  start <- Sys.Date()-days
  start_date <- paste('>',start,sep="")
  url <- modify_url('https://api.open511.gov.bc.ca/events')
  if (is.na(region)==F){
    area <- paste('drivebc.ca/',region,sep="")
    resp <- GET(url,query=list(severity='MAJOR',status='ALL',created=start_date,limit=1000,area_id=area))
  } else {
    resp <- GET(url,query=list(severity='MAJOR',status='ALL',created=start_date,limit=1000))
  }
  http_status(resp)
  parsed <- fromJSON(content(resp, "text",encoding = 'UTF-8'))
  df <- data.frame(parsed$events)
  colnames(df)
  df$areas <- as.character(lapply(df$areas,'[[',2))
  df$day <- as.Date(strftime(df$created,format='%F'))
  df <- df%>%select(day,id,status,event_type,areas,geography,description)
  icon.fa <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', library='fa', iconColor = 'black')
  mp <- leaflet()
  mp <- addTiles(mp)
  for (i in seq(1,length(df$geography$coordinates))){
    if(df$geography$type[i]=='LineString'&df$status[i]=='ACTIVE'){
      mp <- addPolylines(mp,df$geography$coordinates[i][[1]][,1],df$geography$coordinates[i][[1]][,2],popup = df$description[i],color = 'red' ) 
    }
    if (df$geography$type[i]=='LineString'&df$status[i]=='ARCHIVED'){
      mp <- addPolylines(mp,df$geography$coordinates[i][[1]][,1],df$geography$coordinates[i][[1]][,2],popup = df$description[i],color = 'blue' )
    }
    if(df$geography$type[i]=='Point'&df$status[i]=='ACTIVE'){
      mp <- addAwesomeMarkers(mp,df$geography$coordinates[i][[1]][1],df$geography$coordinates[i][[1]][2],popup = df$description[i],icon=icon.fa)
    }
    if(df$geography$type[i]=='Point'&df$status[i]=='ARCHIVED'){
      mp <- addAwesomeMarkers(mp,df$geography$coordinates[i][[1]][1],df$geography$coordinates[i][[1]][2],popup = df$description[i])
    }
  }
  mp
}
map <- driveBC_map_major(15,1)
