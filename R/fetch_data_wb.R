#` Fetch data from the World Bank with API's
#`
#` A single function will be created that will help retrieve the 
#` correct data for analyses concerning economic growth theories
#`
#` @param begin A numeric or string indicating from which time to begin collecting data
#` @param end A numeric or string indicating until when to collect data
#`
#` @return
#`
#` @examples
#`
#` \dontrun{
#`
#` }
#`
fetch_data_wb <- function(years){
  
  # Caching
  
  # Structuring API call
  basic <- "https://api.worldbank.org/v2/countries/all/indicators/?"
  ## date
  begin <- min(as.numeric(years))
  end <- max(as.numeric(years))
  date <- paste0("date=", begin, ":", end)
  ## format
  format <- "&format=json" #hardcoded to be JSON
  ## concatenated
  api_call <- paste0(basic, date, format)
  
  # Fetching data
  jsonlite::fromJSON(api_call)
  
  # Structuring data
  
  # Return
  return()
}
