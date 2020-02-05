#' Leaflet Map
#'
#' Builds an interactive map displaying the active road conditions between starting and destination cities
#' @param start City you are starting from
#' @param dest City you want to arrive at
#' @return A data frame containing a detailed description of each road event, a label for each road event (eg. POOR_VISIBILITY), the name of the specific highway, as well as the severity level
#' @return An interactive map that can be opened in viewer and provide severity level, and road event information
#' @export driveBC_map_route
#' @examples driveBC::driveBC_map_route(start="Kelowna",dest="Vernon")


driveBC_map_route <- function(start = "Kelowna",dest = "Vernon"){

  #Extract British Columbia city name and longitude/ latitude from data coming from maps library

  can_cities <- maps::canada.cities
  cities <- stringr::str_sub(can_cities$name, end = -4)
  can_cities$city <- cities
  british_columbia <- subset(can_cities, can_cities$country.etc == 'BC')

  #Subset starting and destination cities to extract coordinates

  start_city <- subset(british_columbia, british_columbia$city == start)
  dest_city <- subset(british_columbia, british_columbia$city == dest)

  if (is.na(match(start,british_columbia$city))==T){
    stop("City not in British Columbia", call. = FALSE)

  }

  if (is.na(match(dest,british_columbia$city))==T){
    stop("City not in British Columbia", call. = FALSE)

  }

  #Set xmin, ymin, xmax, and ymax values based on starting/ destination cities
  #Add buffer to coordinates so bbox is drawn slightly larger, to include roads that may curve outside of bbox

  xmin <- as.character(min(start_city$long-0.25,dest_city$long-0.25))
  ymin <- as.character(min(start_city$lat-0.25,dest_city$lat-0.25))
  xmax <- as.character(max(start_city$long+0.25,dest_city$long+0.25))
  ymax <- as.character(max(start_city$lat+0.25,dest_city$lat+0.25))

  region_of_interest <- paste(xmin,", ",ymin,", ",xmax,", ",ymax, sep = "")

  #Send request to driveBC api

  url <- httr::modify_url('https://api.open511.gov.bc.ca/events')
  resp <- httr::GET(url,query=list(status='ACTIVE',bbox=region_of_interest,limit=10000))

  #Convert data returned from request into a dataframe
  parsed <- jsonlite::fromJSON(httr::content(resp, "text",encoding = 'UTF-8'))
  df <- data.frame(parsed$events)
  output_df <- df[c('severity', 'description', 'event_subtypes', 'roads')]


  #Use geographic coordinates to highlight regions of highways with active events (road conditions), and visualize on a map
  icon.fa <- leaflet::makeAwesomeIcon(icon = 'flag', markerColor = 'red', library='fa', iconColor = 'black')
  mp <- leaflet::leaflet()
  mp <- leaflet::addTiles(mp)

  for (i in seq(1,length(df$geography$coordinates))){
    if(df$geography$type[i]=='LineString'&df$severity[i]=='MAJOR'){
      mp <- leaflet::addPolylines(mp,df$geography$coordinates[i][[1]][,1],
                                df$geography$coordinates[i][[1]][,2],
                                popup = df$event_subtypes[i],color = 'red' )
    }
    if (df$geography$type[i]=='LineString'&df$severity[i]=='MODERATE'){
      mp <- leaflet::addPolylines(mp,df$geography$coordinates[i][[1]][,1],
                                df$geography$coordinates[i][[1]][,2],
                                popup = df$event_subtypes[i],color = 'yellow' )
    }

    if (df$geography$type[i]=='LineString'&df$severity[i]=='MINOR'){
      mp <- leaflet::addPolylines(mp,df$geography$coordinates[i][[1]][,1],
                                df$geography$coordinates[i][[1]][,2],
                                popup = df$event_subtypes[i],color = 'green' )
    }
    if(df$geography$type[i]=='Point'&df$status[i]=='ACTIVE'){
      mp <- leaflet::addAwesomeMarkers(mp,df$geography$coordinates[i][[1]][1],
                                     df$geography$coordinates[i][[1]][2],
                                     popup = df$description[i],icon=icon.fa)
    }
  }
  output <- list("df" = output_df, "map" = mp)
  return(output)
}
