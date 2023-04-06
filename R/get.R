
#' survey search
#'
#' \code{get_surveys} Queries searches for surveys published between specified dates, and returns a dataframe of matching surveys.
#'
#' @param base_url The base url for the catelog (for IHSN this is http://catalog.ihsn.org/index.php/api/catalog/)
#' @param min_year lower bound for the year the survey was published
#' @param max_year upper bound for the year the survey was published.
#' @param created Filter  by date of creation. Use the date format YYYY-MM-DD. E.g 2020/04/01 returns records created on and after that date.
#' To specify a date range, use the format 2020/04/01-2020/04/20
#' @param ps maximum number of results
#'
#' @return a dataframe
#' @export

# function to list surveys in the repository
get_surveys <- function(base_url, min_year = NULL, max_year = NULL, created = NULL, ps = 10) {

  url = paste0(base_url, "/search")
  response <- httr::GET(url, query = list(from = min_year, to = max_year, created = created, ps = ps))

  if (httr::http_type(response) != "application/json") {
  stop("API did not return json")
  }

  parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = TRUE)

  if (httr::http_error(response)) {
    stop(
      sprintf(
        "API request failed [%s]\n%s",
        httr::status_code(response),
        parsed$errors
        ), call. = FALSE
    )
  }
 return(parsed$result$rows)
}


#' get survey metadata
#'
#' \code{get_survey_meta} Queries searches for surveys published between specified dates, and returns a dataframe of matching surveys.
#'
#' @param base_url The base url for the catelog (for IHSN this is http://catalog.ihsn.org/index.php/api/catalog/)
#' @param  survey_id  Corresponds to idno in the object returned from \code{get_surveys}.
#' @param ps maximum number of results
#'
#' @return a dataframe
#' @export
#'



get_survey_meta <- function(base_url, survey_id) {

  url = glue::glue("{base_url}/{survey_id}")
  response <- httr::GET(url)

  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = TRUE)
  return(parsed$dataset)

}


#' survey search with supplementary metadata
#'
#' \code{get_surveys_meta} Queries searches for surveys published between specified dates, and returns a dataframe of matching surveys, with extended metadata for each survey
#'
#' @param base_url The base url for the catelog (for IHSN this is http://catalog.ihsn.org/index.php/api/catalog/)
#' @param min_year lower bound for the year the survey was published
#' @param max_year upper bound for the year the survey was published.
#' @param created Filter  by date of creation. Use the date format YYYY-MM-DD. E.g 2020/04/01 returns records created on and after that date.
#' To specify a date range, use the format 2020/04/01-2020/04/20
#' @param ps maximum number of results
#'
#' @return a dataframe
#' @export

get_surveys_meta <- function(base_url, min_year = NULL, max_year = NULL, created = NULL, ps = 10) {

 surveys <- get_surveys(base_url = base_url, min_year = min_year, max_year = max_year, created = created, ps = ps)

 surveys_meta <- furrr::future_map(surveys$idno, ~get_survey_meta(base_url, .x)) |>
   furrr::future_map(~format_metadata(.x, tibble()))

 modal_types <- col_types(surveys_meta, 1000) #get modal  type of each column
 list_cols <- modal_types |>
   filter(modal_type == "list") |>
   pull(column_name)

 #reduce to a dataframe

 surveys_meta_df <-  map(surveys_meta, function(df) {
   for (col_name in list_cols) {
     if (is.character(df[[col_name]])) {
       df[[col_name]] <- list(tibble()) # Replace with an empty tibble (dataframe)
     }
   }
   return(df)
 })

 surveys_meta_df <- reduce(surveys_meta_df, bind_rows)

 return(surveys_meta_df)

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
        httr::status_code(resp),
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
get_var_meta <- function(base_url, survey_id, variable_id, format = TRUE) {

  url = glue::glue("{base_url}/{survey_id}/variables/{variable_id}")
  response <- httr::GET(url)

  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = TRUE)

  #if error fetching variable metadata, return single row dataframe of variable affected
  if (httr::http_error(response)) {
    out = dplyr::tibble(survey_id = survey_id,
                 variable_id = variable_id,
                 status_code = httr::status_code(resp),
                 message = parsed$message)
    return(list(error = TRUE, result = out))
  }

  if(isFALSE(format)) {
    return(list(error = FALSE, result = parsed))
  }

  #parse result into dataframe, with, with fields with multiple repsonses as nested dataframes

  var_metadata = format_metadata(parsed, dplyr::tibble())

  return(list(error = FALSE, result = var_metadata))
}


#' get variable metadata for multiple surveys
#'
#' \code{get_metadata} is a wrapper function that gets the metadata for a specified surveys. Operations will run in parallel if
#' \code{future::plan(multisession)} is set before the function call
#' @param base_url The base url for the catalog (for IHSN this is http://catalog.ihsn.org/index.php/api/catalog/)
#' @param ids character vector of survey ids (idno)
#' @param ps maximum number of results
#' @param var_meta if true returns variable metadata for queried surveys
#'
#' @return a list with three dataframes. 'Result' is for successful queries; 'failed_surveys' is for failed variable queries, by survey_id;
#' 'failed_variables' is for field variable queries, by survey and variable id.
#'
#' @export

get_metadata <- function(base_url,
                         ids = NULL,
                         var_meta = TRUE) {


  if(is.null(ids)) {
    stop("specify survey ids for which to extract metadata")
  }

  #get survey metadata

  survey_meta <- furrr::future_map(ids, ~get_survey_meta(base_url, .x))
  survey_meta_df <- reduce_survey_metadata(survey_meta)

  if(!isTRUE(var_meta)){
    return(survey_meta_df)
  }

  #get variable metadata for specified surveys
  vars <- furrr::future_map(survey_meta_df$idno, ~get_vars(base_url, .x))
  vars <- purrr::set_names(vars, survey_meta_df$idno)

  failed_surveys <- purrr::map2_dfr(vars, names(vars), function(x, id) { tibble(idno = id, error = x$error) }) |>
    dplyr::filter(isTRUE(error))

  passed_surveys <- purrr::map_dfr(vars, "result")
  empty_vars <- names(vars)[map_lgl(vars, ~ .x$error == FALSE & length(.x$result) == 0)]

  survey_vars <- dplyr::inner_join(survey_meta_df,  passed_surveys, by = c("id" = "sid"))

  #get variable metadata
  survey_vars_meta <- furrr::future_map2(survey_vars$idno, survey_vars$vid, ~get_var_meta(base_url, .x, .y))
  survey_vars_meta_fmt <- var_meta_format(survey_vars_meta)

  failed_variables <- purrr::map_dfr(survey_vars_meta_fmt, ~if(.x[["error"]] == TRUE) .x[["result"]] else NULL)

  passed_variables <- purrr::map_dfr(survey_vars_meta_fmt, ~if(.x[["error"]] == FALSE) .x[["result"]] else NULL)
  if(nrow(passed_variables) > 0) {
    passed_variables <- dplyr::inner_join(survey_meta_df, passed_variables, by = c("id" = "sid"), suffix = c("survey", "var")) }

  #return data
  return(list(result = passed_variables,
              no_vars = empty_vars,
              failed_surveys = failed_surveys,
              failed_variables = failed_variables))

}

