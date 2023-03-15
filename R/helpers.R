
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


#' extract
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
