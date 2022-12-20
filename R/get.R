
#' survey search
#'
#' \code{get_surveys} Queries searches for surveys published between specified dates, and returns a dataframe of matching surveys.
#'
#' @param base_url The base url for the catelog (for IHSN this is http://catalog.ihsn.org/index.php/api/catalog/)
#' @param min_year lower bound for the year the survey was published
#' @param max_year upper bound for the year the survey was publishe.
#' @param ps maximum number of results
#' 
#' @return a dataframe
#' @export

# function to list surveys in the repository
get_surveys <- function(base_url, min_year, max_year, ps = 1e4) {
  
  url = paste0(base_url, "/search")
  response <- httr::GET(url, query = list(from = min_year, to = max_year, ps = ps))
  
  if (httr::http_type(response) != "application/json") {
  stop("API did not return json")
  }

  parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = TRUE)  

  if (httr::http_error(response)) {
    stop(
      sprintf(
        "IHSN API request failed [%s]\n%s", 
        status_code(response),
        parsed$errors
        )
    )
  }
 return(parsed$result$rows)
}


#' get variables
#'
#' \code{get_vars} Returns a dataframe listing the available variables for a specified survey
#'
#' @param base_url The base url for the catelog (for IHSN this is http://catalog.ihsn.org/index.php/api/catalog/)
#' @param  survey_id The survey id. Corresponds to idno returned from \code{get_surveys}
#' 
#' @return a list with the result and error. Result is the dataframe of available variables. Err is a logical variable and is TRUE when the api request failed.
#' @export

get_vars <- function(base_url, survey_id) {
  
  url = glue::glue("{base_url}/{survey_id}/variables") 
  response <- httr::GET(url) 
  
  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = TRUE)
  
  if (httr::http_error(response)) {
    warning(
      sprintf(
        "IHSN API request failed [%s] on survey [%s]\n%s", 
        status_code(resp),
        survey_id,
        parsed$message
      ), call. = FALSE
    )
    return(list(error = TRUE, result = FALSE))
  }
  return(list(error = FALSE, result = parsed$variables))
}


#' get variable metadata
#'
#' \code{get_var_meta} gets the metadata for a sepcified variable_id
#'
#' @param base_url The base url for the catelog (for IHSN this is http://catalog.ihsn.org/index.php/api/catalog/)
#' @param  survey_id  Corresponds to idno in the object returned from \code{get_surveys}.
#' @param variable_id Corresponds to vid in the object returned from \code{get_vars}.
#' 
#' @return a list with the result and error. If no error, err is set to FALSE and result is a signle row dataframe of variable metadata. 
#' Otherwise err is TRUE and result is a dataframe of the error message.
#' 
#'@export

#function to get metadata for a specified variable
get_var_meta <- function(base_url, survey_id, variable_id) {
  
  url = glue::glue("{base_url}/{survey_id}/variables/{variable_id}") 
  response <- httr::GET(url) 
  
  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = TRUE)
  
  #if error fetching variable metadata, return single row dataframe of variable affected 
  if (httr::http_error(response)) {
    out = tibble(survey_id = survey_id, 
                 variable_id = variable_id,
                 status_code = status_code(resp),
                 message = parsed$message)
    return(list(error = TRUE, result = out))
  }
  
  #parse result into dataframe, with, with fields with multiple repsonses as nested dataframes

  var_metadata = format_metadata(parsed, tibble())
  
  return(list(error = FALSE, result = var_metadata))
}


#' get variable metadata for multiple surveys
#'
#' \code{get_metadata} is a wrapper function that gets the metadata for a queried surveys. Operations will run in parellel if
#' \code{future::plan(multisession) is set before the function call }
#' @param base_url The base url for the catelog (for IHSN this is http://catalog.ihsn.org/index.php/api/catalog/)
#' @param min_year lower bound for the year the survey was published
#' @param max_year upper bound for the year the survey was published
#' @param ps maximum number of results
#' @param var_meta If true returns variable metadata for queried surveys
#' 
#' @return a list with three dataframes. 'Result' is for successful queries; 'failed_surveys' is for failed variable queries, by survey_id; 
#' 'failed_variables' is for faield variable queries, by survey and variable id.
#' 
#'  @export

get_metadata <- function(base_url, min_year, max_year, ps = 1e4, var_meta = TRUE) {
  
  #if variable metadata not requested, only return survey metadata
  surveys = get_surveys(base_url, min_year, max_year, ps = ps) 
  if(!var_meta){ return(surveys) } 

  #get variable metadata for specified surveys
  vars <- furrr::future_map(surveys$idno, ~get_vars(base_url, .x))
  purrr::set_names(vars, surveys$idno) 
  
  failed_surveys <- purrr::map(vars, "error") %>% 
    dplyr::tibble(error = ., idno = surveys$idno)  %>%
    dplyr::filter(isTRUE(error))
  
  passed_surveys <- purrr::map_dfr(vars, "result") 
  survey_vars <- dplyr::inner_join(surveys,  passed_surveys %>% dplyr::mutate(sid = as.numeric(sid)), by = c("id" = "sid")) 
  
  #get variable metadata
  survey_vars_meta <- furrr::future_map2(survey_vars$idno, survey_vars$vid, ~get_var_meta(base_url, .x, .y)) 
  
  failed_variables <- map_dfr(survey_vars_meta, ~if(.x[["error"]] == TRUE) .x[["result"]] else NULL) 
  
  passed_variables <-map_dfr(survey_vars_meta, ~if(.x[["error"]] == FALSE) .x[["result"]] else NULL) 
  if(nrow(passed_variables) > 0) {
    passed_variables <- inner_join(surveys, passed_variables %>% mutate(sid = as.numeric(sid)), by = c("id" = "sid"), suffix = c("survey", "var")) }
  
  #return data
  return(list(result = passed_variables, 
              failed_surveys = vars$failed_surveys,
              failed_variables = failed_variables))
}

