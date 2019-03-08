#' Average values across multiple columns for each row of a \code{data.frame}
#'
#' @param data a \code{data.frame} to use for averaging across rows
#' @param string a string that can be used in \code{dplyr}'s \code{select} function.
#' @param kind a string indicating which type of \code{dplyr} select helper functions.
#'   Appropriate values are "starts", "ends", or "matches".
#' @param reverse a numeric vector indicating items or variables to reverse code using
#'    \code{reverse.code} from the \code{psych} package.
#'
#' @return a vector of the averaged values for each row from the specified columns of the data frame.
#'
#' @section Details:
#' This function, and those specified below in 'See Also', work best when used
#' inside a call to \code{dplyr}'s \code{mutate()} for creating new variables.
#' The point of the function is to make it easy to use regular expressions to
#' select variables for aggregation, such as an average score for a number of
#' questionnaire items or the sum of correct responses.
#'
#' @export
#'
#' @seealso \code{\link{sum_rows}} for row summing, \code{\link{sum_missings}} for summing
#'   missing values, and \code{\link{sd_rows}} for calculating standard deviations across rows.
average_rows <- function(data,string,kind = "starts", reverse = NULL){
  if(is.null(reverse)){
    if(kind=="starts"){
      dplyr::select(data,dplyr::starts_with(string)) %>%
      rowMeans(na.rm=T)
    }
    else if(kind=="ends"){
      dplyr::select(data,dplyr::ends_with(string)) %>% rowMeans(na.rm=T)
    }
    else if(kind=="matches"){
      dplyr::select(data,dplyr::matches(string)) %>% rowMeans(na.rm=T)
    }
  } else{
    if(kind=="starts"){
      n.items <- dplyr::select(data,dplyr::starts_with(string)) %>% ncol()
      items <- rep(1,n.items)
      items[reverse] <- -1
      dplyr::select(data,dplyr::starts_with(string)) %>% psych::reverse.code(keys = items, items = .) %>% rowMeans(na.rm=T)
    }
    else if(kind=="ends"){
      n.items <- dplyr::select(data,dplyr::ends_with(string)) %>% ncol()
      items <- rep(1,n.items)
      items[reverse] <- -1
      dplyr::select(data,dplyr::ends_with(string)) %>%
        psych::reverse.code(keys = items, items = .) %>%
        rowMeans(na.rm=T)
    }
    else if(kind=="matches"){
      n.items <- dplyr::select(data,dplyr::matches(string)) %>% ncol()
      items <- rep(1,n.items)
      items[reverse] <- -1
      dplyr::select(data,dplyr::matches(string)) %>%
        psych::reverse.code(keys = items, items = .) %>%
        rowMeans(na.rm=T)
    }
  }
}

#' Sum missing values across multiple columns for each row of a \code{data.frame}
#'
#' @param data a \code{data.frame} to use for summing missing values
#' @param string a string that can be used in \code{dplyr}'s \code{select} function.
#' @param kind a string indicating which type of \code{dplyr} select helper functions.
#'   Appropriate values are "starts", "ends", or "matches".
#'
#' @return a \code{data.frame} with the newly created variable
#'
#' @section Details:
#' This function, and those specified below in 'See Also', work best when used
#' inside a call to \code{dplyr}'s \code{mutate()} for creating new variables.
#' The point of the function is to make it easy to use regular expressions to
#' select variables for aggregation, such as an average score for a number of
#' questionnaire items or the sum of correct responses.
#'
#' @export
#' @seealso \code{\link{sum_rows}} for row summing, \code{\link{average_rows}} for averaging
#'   across rows, and \code{\link{sd_rows}} for calculating standard deviations across rows.
sum_missings <- function(data,string,kind = "starts"){
  if(kind=="starts"){
    dplyr::select(data,dplyr::starts_with(string)) %>% is.na(.) %>% rowSums(na.rm=T)
  }
  else if(kind=="ends"){
    dplyr::select(data,dplyr::ends_with(string)) %>% is.na(.) %>% rowSums(na.rm=T)
  }
  else if(kind=="matches"){
    dplyr::select(data,dplyr::matches(string)) %>% is.na(.) %>%  rowSums(na.rm=T)
  }
}

#' Sum values across multiple columns for each row of a \code{data.frame}
#'
#' @param data a \code{data.frame} to use for summing across rows
#' @param string a string that can be used in \code{dplyr}'s \code{select} function.
#' @param kind a string indicating which type of \code{dplyr} select helper functions.
#'   Appropriate values are "starts", "ends", or "matches".
#'
#' @return a \code{data.frame} with the newly created variable
#'
#' @section Details:
#' This function, and those specified below in 'See Also', work best when used
#' inside a call to \code{dplyr}'s \code{mutate()} for creating new variables.
#' The point of the function is to make it easy to use regular expressions to
#' select variables for aggregation, such as an average score for a number of
#' questionnaire items or the sum of correct responses.
#'
#' @export
#' @seealso \code{\link{average_rows}} for row averaging across rows, \code{\link{sum_missings}} for summing
#'   missing values, and \code{\link{sd_rows}} for calculating standard deviations across rows.
sum_rows <- function(data,string,kind = "starts"){
  if(kind=="starts"){
    n.items <- dplyr::select(data,dplyr::starts_with(string)) %>% ncol()
    dplyr::select(data,dplyr::starts_with(string)) %>%
      dplyr::mutate(sums = ifelse(is.na(.) %>% rowSums() == n.items, NA, rowSums(.,na.rm=T))) %>%
      dplyr::pull(sums)
  }
  else if(kind=="ends"){
    n.items <- dplyr::select(data,dplyr::ends_with(string)) %>% ncol()
    dplyr::select(data,dplyr::ends_with(string)) %>%
      dplyr::mutate(sums = ifelse(is.na(.) %>% rowSums() == n.items, NA, rowSums(.,na.rm=T))) %>%
      dplyr::pull(sums)
  }
  else if(kind=="matches"){
    n.items <- dplyr::select(data,dplyr::matches(string)) %>% ncol()
    dplyr::select(data,dplyr::matches(string)) %>%
      dplyr::mutate(sums = ifelse(is.na(.) %>% rowSums() == n.items, NA, rowSums(.,na.rm=T))) %>%
      dplyr::pull(sums)
  }
}

#' Take the standard deviation across multiple columns for each row of a \code{data.frame}
#'
#' @param data a \code{data.frame} to use for taking the standard deviation across rows
#' @param string a string that can be used in \code{dplyr}'s \code{select} function.
#' @param kind a string indicating which type of \code{dplyr} select helper functions.
#'   Appropriate values are "starts", "ends", or "matches".
#'
#' @return a \code{data.frame} with the newly created variable
#'
#' @section Details:
#' This function, and those specified below in 'See Also', work best when used
#' inside a call to \code{dplyr}'s \code{mutate()} for creating new variables.
#' The point of the function is to make it easy to use regular expressions to
#' select variables for aggregation, such as an average score for a number of
#' questionnaire items or the sum of correct responses.
#'
#' @export
#' @seealso \code{\link{sum_rows}} for row summing, \code{\link{sum_missings}} for summing
#'   missing values, and \code{\link{average_rows}} for row averaging across rows.
sd_rows <- function(data,string,kind = "starts"){
  if(kind=="starts"){
    dplyr::select(data,dplyr::starts_with(string)) %>%
      dplyr::mutate(id = 1:n()) %>%
      tidyr::gather(key,value,-id) %>%
      dplyr::group_by(id) %>%
      dplyr::summarize(sd = sd(value,na.rm=T)) %>%
      dplyr::pull(sd)
  }
  else if(kind=="ends"){
    dplyr::select(data,dplyr::ends_with(string)) %>%
      dplyr::mutate(id = 1:n()) %>%
      tidyr::gather(key,value,-id) %>%
      dplyr::group_by(id) %>%
      dplyr::summarize(sd = sd(value,na.rm=T)) %>%
      dplyr::pull(sd)
  }
  else if(kind=="matches"){
    dplyr::select(data,dplyr::matches(string))  %>%
      dplyr::mutate(id = 1:n()) %>%
      tidyr::gather(key,value,-id) %>%
      dplyr::group_by(id) %>%
      dplyr::summarize(sd = sd(value,na.rm=T)) %>%
      dplyr::pull(sd)
  }
}

#' Updated version of \code{average_rows()}
#'
#' Average values across multiple columns for each row of a \code{data.frame}
#'
#' @param data a \code{data.frame} to use for averaging across rows
#' @param selector a \code{dplyr::select} helper function, for example \code{starts_with()}
#' @param reverse a numeric vector indicating items or variables to reverse code using
#'    \code{reverse.code} from the \code{psych} package.
#' @param show choose whether or not show the results of the reverse coding. If TRUE,
#'     the result of the function will be a \code{data.frame} with the original columns passed to
#'     the function along with the reverse coded columns denoted with an "_r". Useful for checking to
#'     make sure reverse coding was done to the appropriate columns.
#' @param mini the minimum value of a scale score (used when reverse = T)
#' @param maxi the maximum value of a scale score (used when reverse = T)
#'
#' @return a vector of the averaged values for each row from the specified columns of the data frame.
#'
#' This function, and those specified below in 'See Also', work best when used
#' inside a call to \code{dplyr}'s \code{mutate()} for creating new variables.
#' The point of the function is to make it easy to use select helpers from
#' the tidyselect (and used in dplyr extensively) to select variables for aggregation,
#' such as an average score for a number of questionnaire items, the sum of correct responses,
#' or finding the number of missing values in a row.
#'
#' @export
#'
#' @seealso \code{\link{tidy_sum_rows}} for row summing, \code{\link{tidy_sum_missings}} for summing
#'   missing values, and \code{\link{tidy_sd_rows}} for calculating standard deviations across rows.
tidy_avg_rows <- function(data, selector, reverse = NULL, show = F, mini = NULL, maxi = NULL){
  user_select <- enquo(selector)
  if(is.null(reverse)){
    dplyr::select(data, rlang::eval_tidy(user_select)) %>% rowMeans(na.rm=T)
  } else{
    n.items <- dplyr::select(data, rlang::eval_tidy(user_select)) %>% ncol()
    items <- rep(1,n.items)
    items[reverse] <- -1

    if(show){
      suppressWarnings(
        dplyr::bind_cols(
          dplyr::select(data, rlang::eval_tidy(user_select)),
          dplyr::select(data, rlang::eval_tidy(user_select)) %>%
            psych::reverse.code(keys = items, items = ., mini = mini, maxi = maxi) %>%
            dplyr::as_tibble() %>%
            dplyr::rename_at(vars(ends_with("-")), dplyr::funs(stringr::str_remove_all(.,"-|`") %>% paste0("_r"))) %>%
            dplyr::select(dplyr::ends_with("r"))
        )
      )
    } else{
      dplyr::select(data, rlang::eval_tidy(user_select)) %>%
        psych::reverse.code(keys = items, items = ., mini = mini, maxi = maxi) %>%
        rowMeans(na.rm=T)
    }
  }
}


#' Updated version of \code{sum_rows()}
#'
#' @param data a \code{data.frame} to use for summing across rows
#' @param selector a \code{dplyr::select} helper function, for example \code{starts_with()}
#'
#' @return a vector of the summned values for each row from the specified columns of the data frame.
#'
#' @section Details:
#' This function, and those specified below in 'See Also', work best when used
#' inside a call to \code{dplyr}'s \code{mutate()} for creating new variables.
#' The point of the function is to make it easy to use select helpers from
#' the tidyselect (and used in dplyr extensively) to select variables for aggregation,
#' such as an average score for a number of questionnaire items, the sum of correct responses,
#' or finding the number of missing values in a row.
#'
#' @export
#' @seealso \code{\link{tidy_avg_rows}} for row averaging across rows, \code{\link{tidy_sum_missings}} for summing
#'   missing values, and \code{\link{tidy_sd_rows}} for calculating standard deviations across rows.
tidy_sum_rows <- function(data, selector){
  user_select <- enquo(selector)
  n.items <- dplyr::select(data, rlang::eval_tidy(user_select)) %>% ncol()
  dplyr::select(data,rlang::eval_tidy(user_select)) %>%
    dplyr::mutate(sums = ifelse(is.na(.) %>% rowSums() == n.items, NA, rowSums(.,na.rm=T))) %>%
    dplyr::pull(sums)
}

#' Updated version of \code{sum_missings()}
#'
#' @param data a \code{data.frame} to use for summing missing values across rows
#' @param selector a \code{dplyr::select} helper function, for example \code{starts_with()}
#'
#' @return a vector of the number of missing values for each row from the specified columns of the data frame.
#'
#' @section Details:
#' This function, and those specified below in 'See Also', work best when used
#' inside a call to \code{dplyr}'s \code{mutate()} for creating new variables.
#' The point of the function is to make it easy to use select helpers from
#' the tidyselect (and used in dplyr extensively) to select variables for aggregation,
#' such as an average score for a number of questionnaire items, the sum of correct responses,
#' or finding the number of missing values in a row.
#'
#' @export
#' @seealso \code{\link{tidy_avg_rows}} for row averaging across rows, \code{\link{tidy_sum_rows}} for row summing
#' , and \code{\link{tidy_sd_rows}} for calculating standard deviations across rows.
tidy_sum_missings <- function(data, selector){
  user_select <- enquo(selector)
  dplyr::select(data,rlang::eval_tidy(user_select)) %>% is.na(.) %>% rowSums(na.rm=T)
}

#' Updated version of \code{sd_rows()}
#'
#' @param data a \code{data.frame} to use for row standard deviations
#' @param selector a \code{dplyr::select} helper function, for example \code{starts_with()}
#'
#' @return a vector of the standard deviations for each row from the specified columns of the data frame.
#'
#' @section Details:
#' This function, and those specified below in 'See Also', work best when used
#' inside a call to \code{dplyr}'s \code{mutate()} for creating new variables.
#' The point of the function is to make it easy to use select helpers from
#' the tidyselect (and used in dplyr extensively) to select variables for aggregation,
#' such as an average score for a number of questionnaire items, the sum of correct responses,
#' or finding the number of missing values in a row.
#'
#' @export
#' @seealso \code{\link{tidy_avg_rows}} for row averaging across rows, \code{\link{tidy_sum_rows}} for row summing
#' , and \code{\link{tidy_sum_missings}} for summing missing values
tidy_sd_rows <- function(data, selector){
  user_select <- enquo(selector)
  dplyr::select(data,rlang::eval_tidy(user_select)) %>%
    dplyr::mutate(id = 1:n()) %>%
    tidyr::gather(key,value,-id) %>%
    dplyr::group_by(id) %>%
    dplyr::summarize(sd = sd(value,na.rm=T)) %>%
    dplyr::pull(sd)
}

#' @import dplyr
#' @import rlang
#' @importFrom magrittr %>%
NULL
