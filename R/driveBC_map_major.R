#' Leaflet Map
#'
#' Builds an interactive map displaying the locations of active and historical major incidents
#' @param days Number of days to include in search, in addition to today
#' @param region Default is \code{NA} which returns displays all of BC.  If you want a specific region, enter a
#' number that corresponds to one of the following regions.
#' \enumerate{
#' \item Lower Mainland
#' \item Vancouver Island
#' \item Rocky Mountain
#' \item West Kootenay
#' \item Okanagan-Shuswap
#' \item Thompson-Nicola
#' \item Cariboo
#' \item Peace
#' \item Fort George
#' \item Bulkley Stikine
#' \item Skeena
#' }
#' @param status Incident status.  Default is \code{ALL} which returns both ACTIVE and ARCHIVED.  If only want ACTIVE or ARCHIVED, specify in function call
#' @return An interactive map that can be opened in viewer and analyzed further
#' @export driveBC_map_major
#' @examples driveBC::driveBC_map_major(days=5,region=1,status='ALL')


driveBC_map_major <- function(days,region=NA,status='ALL'){

  # Modify inputs so can be passed as a valid URL to the API ------------------------------------------------------
  start <- Sys.Date()-days
  start_date <- paste('>',start,sep="")
  url <- httr::modify_url('https://api.open511.gov.bc.ca/events')

  # Make one of to get calls depending if want specific region or entire provice ----------------------------------
  if (is.na(region)==F){
    area <- paste('drivebc.ca/',region,sep="")
    resp <- httr::GET(url,query=list(severity='MAJOR',status=status,created=start_date,limit=10000,area_id=area))
  } else {
    resp <- httr::GET(url,query=list(severity='MAJOR',status=status,created=start_date,limit=10000))
  }

  # Error handling if format is incorrect of invalid url passed ---------------------------------------------------
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  if (httr::http_error(resp)){
    stop(paste("Invalid url, please revisit parameters \n",resp$url), call. = FALSE)
  }

  # Parse the data and modify formats as required so can pass as a dataframe to the mapping function --------------
  parsed <- jsonlite::fromJSON(httr::content(resp, "text",encoding = 'UTF-8'))
  df <- data.frame(parsed$events)
  df$areas <- as.character(lapply(df$areas,'[[',2))
  df$day <- as.Date(strftime(df$created,format='%F'))

  # Generate a map with appropriate labelling type for the types of incidents -------------------------------------
  icon.fa <- leaflet::makeAwesomeIcon(icon = 'flag', markerColor = 'red', library='fa', iconColor = 'black')
  mp <- leaflet::leaflet()
  mp <- leaflet::addTiles(mp)
  for (i in seq(1,length(df$geography$coordinates))){
    if(df$geography$type[i]=='LineString'&df$status[i]=='ACTIVE'){
      mp <- leaflet::addPolylines(mp,df$geography$coordinates[i][[1]][,1],
                                  df$geography$coordinates[i][[1]][,2],
                                  popup = df$description[i],color = 'red' )
    }
    if (df$geography$type[i]=='LineString'&df$status[i]=='ARCHIVED'){
      mp <- leaflet::addPolylines(mp,df$geography$coordinates[i][[1]][,1],
                                  df$geography$coordinates[i][[1]][,2],
                                  popup = df$description[i],color = 'blue' )
    }
    if(df$geography$type[i]=='Point'&df$status[i]=='ACTIVE'){
      mp <- leaflet::addAwesomeMarkers(mp,df$geography$coordinates[i][[1]][1],
                                       df$geography$coordinates[i][[1]][2],
                                       popup = df$description[i],icon=icon.fa)
    }
    if(df$geography$type[i]=='Point'&df$status[i]=='ARCHIVED'){
      mp <- leaflet::addAwesomeMarkers(mp,df$geography$coordinates[i][[1]][1],
                                       df$geography$coordinates[i][[1]][2],
                                       popup = df$description[i])
    }
  }
  mp
}
