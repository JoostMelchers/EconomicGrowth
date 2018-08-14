#' Fetch data from the World Bank with API's
#'
#' A single function will be created that will help retrieve data for analyses 
#' concerning the global economy and more.
#'
#' @param begin A numeric. Indicating from which time to begin collecting data.
#' @param end A numeric. Indicating until when to collect data.
#' @param indicators A character vector. Indicator names need to exactly
#' match the names as used by the WB databank. 
#'
#' @return A tibble.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' }
fetch_data_wb <- function(begin, end, indicators){
  
  # input testing --------------------------------------------------------------
  assertthat::is.count(begin)
  assertthat::is.count(end)
  assertthat::assert_that(is.character(indicators), msg = "Error: 'indicators'
                          must be a character vector.")
  
  # caching --------------------------------------------------------------------
  # to be continued
  
  # structuring API call -------------------------------------------------------
  basic <- "https://api.worldbank.org/v2/countries/all/indicators/?"
  ## date
  begin <- min(as.numeric(years))
  end <- max(as.numeric(years))
  date <- paste0("date=", begin, ":", end)
  ## format
  format <- "&format=json" #hardcoded to be JSON
  ## concatenated
  api_call <- paste0(basic, date, format)
  
  # fetching data --------------------------------------------------------------
  output <- jsonlite::fromJSON(api_call)
  
  # return ---------------------------------------------------------------------
  return(output)
}