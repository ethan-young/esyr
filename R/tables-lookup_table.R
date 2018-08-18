#' Create a \code{data.frame} of variable names and labels.
#'
#' @param data a \code{data.frame} with labels that you would like to
#'  convert to a table of variable names and labels
#' @param ... other arguments that can be passed to \code{paste()}
#'
#' @return a \code{data.frame} with three columns. The first is the names of the variables in
#'  \code{data}, the second is their labels, and the third are any value labels associated with each variable.
#'  If a certain variable does not have an variable label or value label it will be filled in with "".
#'  If there are one or more empty labels, a message will print
#'  listing the variables that do not have either variable and/or value labels.
#' @export
#'
lookup_table <- function(data, ...){

  lp_data <- tibble::data_frame(
    Variable = names(data),
    Label    = purrr::map_chr(data,function(x){
      extracted_label <- attr(x,which="label")
      if(is.null(extracted_label)){
        extracted_label <- ""
      }
      extracted_label
    }),
    Values = purrr::map_chr(data,function(x){
      extracted_values <- attr(x,which = "labels")
      if(is.null(extracted_values)){
        extracted_values <- ""
      } else{
        extracted_values <- paste(names(extracted_values),"=",extracted_values, ...)
      }
      extracted_values
    })
  )

  lp_table <- lp_data

  no_labels <- lp_table %>% dplyr::filter(Label == "")
  no_values <- lp_table %>% dplyr::filter(Values == "")

  if(nrow(no_labels)>0){
    message("The following variables have no labels:\n",
            paste(no_labels %>% dplyr::pull(Variable),collapse = "\n"))
  }
  if(nrow(no_values)>0){
    message("The following variables have no value labels:\n",
            paste(no_values %>% dplyr::pull(Variable),collapse = "\n"))
  }

  lp_table

}
