
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
