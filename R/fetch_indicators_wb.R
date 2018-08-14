#` Fetch the indicators from the World Bank
#`
#` A single function will be created that will help retrieve an overview 
#` of possible indicators for analyses concerning economic growth theories
#`
#` @param per_page A numeric or string indicating how many page results are wanted 
#`
#` @return a tibble 
#`
#` @examples
#`
#` \dontrun{
#` fetch_indicators_wb(-10000)
#` }
#`
fetch_indicators_wb <- function(per_page = 20000, include_list = FALSE) {
  
  # required packages and arguments check
  if (!requireNamespace("jsonlite")) { stop("Package 'jsonlite' is required to continue, please install.") }
  if (!requireNamespace("tibble")) { stop("Package 'tibble' is required to continue, please install.") }
  
  if (per_page > 0) {
    api_call <- paste0("https://api.worldbank.org/v2/indicators?per_page=", per_page, "&format=json")
  } else {
    stop("API cannot handle negative number of pages.")
  }
  
  # Fetch list of indicators with API call, immediately with jsonlite's help
  list_indicators <- jsonlite::fromJSON(api_call)
  
  # Gather Indicators
  ids <- data.frame(list_indicators[[2]][, c("id", "name", "unit", "sourceNote")])
  
  # Also gather and structure topic information
  topics <- data.frame()
  for (i in 1:length(list_indicators[[2]][["topics"]])) {
    
    topic_info <- list_indicators[[2]][["topics"]][[i]]
    
    # Some empty entries
    if (!(nrow(topic_info) == 2)) {
      topic_info <- data.frame(id = NA, value = NA)
    }
    
    topics <- rbind(topics, topic_info)
  }
  colnames(topics) <- c("topic_id", "topic_name") # more descriptive, and we don't get id.x and id.y column names
  
  # Combine as tibble
  tibble_indicators <- tibble::as.tibble(cbind(topics, ids))
  
  # Output
  if (!include_list) {
    
    return(tibble_indicators)
  } else {
    
    return(list(tibble_indicators, list_indicators))
  }
}

