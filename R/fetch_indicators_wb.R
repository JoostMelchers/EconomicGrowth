#' Fetch a list of available indicators from the World Bank
#'
#' A single function will be created that will help retrieve an overview 
#' of possible indicators for analyses concerning the economy and more.
#'
#' @param per_page A numeric. How many page results do you fancy?
#'
#' @return a tibble 
#' @export
#'
#' @examples
#'
#' \dontrun{
#' fetch_indicators_wb(-10000)
#' }
fetch_indicators_wb <- function(per_page = 20000, include_list = FALSE) {
  
  # input testing --------------------------------------------------------------
  assertthat::is.count(per_page)
  assertthat::assert_that(is.logical(include_list), msg = "Error: 'return_list'
                          must be a logical value (boolean).")
  assertthat::assert_that(per_page > 0, msg = "Error: API cannot handle negative 
                          number of pages. This doesn't make any sense.")

  # packages -------------------------------------------------------------------
  if (!requireNamespace("jsonlite")) { stop("Package 'jsonlite' is required to 
                                            continue, please install.") }
  if (!requireNamespace("tibble")) { stop("Package 'tibble' is required to 
                                          continue, please install.") }
  
  # fetch indicators -----------------------------------------------------------
  api_call <- paste0("https://api.worldbank.org/v2/indicators?per_page=", 
                     per_page, "&format=json")
  list_indicators <- jsonlite::fromJSON(api_call)
  
  # structure API response -----------------------------------------------------
  ids <- data.frame(list_indicators[[2]][, c("id", "name", "unit", "sourceNote")])
  
  # gather and structure topic information
  topics <- data.frame()
  for (i in 1:length(list_indicators[[2]][["topics"]])) {
    
    topic_info <- list_indicators[[2]][["topics"]][[i]]
    
    # Some empty entries
    if (!(nrow(topic_info) == 2)) {
      topic_info <- data.frame(id = NA, value = NA)
    }
    
    topics <- rbind(topics, topic_info)
  }
  
  # to be more descriptive, and avoiding id.x and id.y as column names
  colnames(topics) <- c("topic_id", "topic_name")
  
  # combine as tibble
  tibble_indicators <- tibble::as.tibble(cbind(topics, ids))
  
  # return ---------------------------------------------------------------------
  if (!include_list) {
    
    return(tibble_indicators)
  } else {
    
    return(list(tibble_indicators, list_indicators))
  }
}

