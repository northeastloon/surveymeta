
#' flatten metadata response into a dataframe
#'
#' \code{format_metadata} selectively flattens metadata response object into a dataframe. Atomic list items are written within dataframe columns,
#' and dataframe objects are nested with a column. The function is called from \code{get_var_meta}.
#'
#' @param meta A metadata response object (represetned by an r list) from the '/catalog/{IDNo}/variables/{varId}' endpoint
#' @param df an empty dataframe that is recursively appended
#'
#' @return a dataframe
#'

format_metadata <-function(meta, df) {

  for(item in names(meta)) {
    if(class(meta) == "list" && is.atomic(meta[[item]])) {
      if(is.null(meta[[item]])) {
        df[1, item] <- NA
      }else {
        df[1, item] <- meta[[item]]
      }
    }else if(class(meta[[item]]) == "data.frame"){
      df <- dplyr::mutate(df, !!dplyr::enquo(item) := list(dplyr::tibble(meta[[item]])))
    }else{
      df <- format_metadata(meta[[item]], df)
    }
  }
  return(df)
}


#' modal column types of a list of dataframes
#'
#' \code{col_types} returns the modal column types of a list of dataframes. To reduce computation time,
#' dataframes can be sampled from the supplied list.
#'
#' @param data a list of dataframes (with the expectation a single row per dataframe)
#' @param n_max number of dataframes to sample from the list. If n_max exceeds the length of the list,
#' the full set is analyzed.
#'
#' @return a dataframe
#'

col_types <- function(data, max_n) {

  if(!(is.list(data) && all(purrr::map_lgl(data, is.data.frame)))) {
    stop("data argument should be a list")
  }

  if(!(is.numeric(max_n) &&  length(max_n) ==1)) {
    stop("max_n should be a number")
  }

  #return maximum possible sample given max_n and length of list
  size = min(length(data), max_n)

  #return the mode type for each column

  find_mode <- function(x) {
    if(all(is.na(x))) {
      return("list")
    }
    tbl <- table(x)
    return(as.character(names(tbl[tbl == max(tbl)])))
  }

 col_types <- purrr::map_dfr(data[sample(1:length(data), size)], ~map(.x, typeof)) |>
   summarize(across(everything(), find_mode)) |>
   t() %>%
   as_tibble(rownames = "column_name") %>%
   rename(modal_type = V1)

}

#' format and reduce a list of survey metadata to a dataframe
#'
#' \code{reduce_metadata} returns a formatted dataframe of survey metadata, with a row per survey. As columns with no data that
#' would typically be expected as a list are of type character when after application of \code{format_metadata}, they are converted to list
#' based on inferring their correct type using \code{col_type}.
#'
#'
#' @param data a list of dataframes (with the expectation a single row per dataframe)

#' @return a dataframe
#'
#'@export

reduce_survey_metadata <- function(data) {


  survey_meta_format <- purrr::map(data, ~format_metadata(.x, tibble()))

  list_cols  <- col_types(survey_meta_format, 1000) |> #get modal column type of list of dataframes
    filter(modal_type == "list") |>
    pull(column_name)

  # convert empty character columns to list and reduce to dataframe
  survey_meta_format <- purrr::map(survey_meta_format, function(df) {
    for (col_name in list_cols) {
      if (is.character(df[[col_name]])) {
        df[[col_name]] <- list(tibble()) # Replace with an empty tibble (dataframe)
      }
    }
    return(df)
  })

  survey_meta_df <- reduce(survey_meta_format, bind_rows)
  return(survey_meta_df)

}


#' format and reduce a list of variable metadata
#'
#'Provided a list of variable meta data,  \code{var_meta_format} infers column types using \code{col_type} and converts
#'list and character variables that are not of the inferred type.
#'
#'@param data a list of dataframes (with the expectation a single row per dataframe)

#'@return a dataframe

var_meta_format <- function(data) {

  results <- purrr::map(data, ~if(.x[["error"]] == FALSE) .x[["result"]] else NULL)
  types <- col_types(results, 1000)
  list_cols <- filter(types, modal_type == "list") |> pull(column_name)
  char_cols <- filter(types, modal_type == "character") |> pull(column_name)


  # convert empty character columns to list and reduce to dataframe
  var_meta_format <- purrr::map(data, function(item) {
    if(isFALSE(item$error)) {
      for (col in names(item$result)) {
        if (col %in% list_cols && !is.list(item[["result"]][[col]])) {
          item[["result"]][[col]] <- list(tibble()) # Replace with an empty tibble (dataframe)
        }
        if (col %in% char_cols && !is.character(item[["result"]][[col]])) {
          item[["result"]][[col]] <- as.character(item[["result"]][[col]])
        }
      }
    }
    return(item)
  })

  return(var_meta_format)

}



#' extract metadata fields data relevant to analysis of survey coverage
#'
#'\code{extract_coverage_meta} selectively extracts fields related to survey coverage.
#'
#'@param meta A metadata response object (represented by an r list) from the '/catalog/{IDNo}/variables/{Surveyid}' endpoint

#'@return a dataframe
#'@export

extract_coverage_meta <- function(meta) {

  tibble(
  start = meta$metadata$study_desc$study_info$coll_dates$start,
  end = meta$metadata$study_desc$study_info$coll_dates$end,
  geog_coverage = meta$metadata$study_desc$study_info$geog_coverage,
  analysis_unit = meta$metadata$study_desc$study_info$analysis_unit,
  universe = meta$metadata$study_desc$study_info$universe,
  data_kind = meta$metadata$study_desc$study_info$data_kind,
  notes = meta$metadata$study_desc$study_info$notes

  )

}
