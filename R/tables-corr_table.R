#' Create a formatted correlation table
#'
#' @param data a \code{data.frame} containing only the variable to be included in the table
#' @param sample_size logical. TRUE will add the sample size used in each pairwise correlation
#'  and display them in the upper triangle of the table. FALSE will leave the upper triangle empty
#' @param use a string, same as the arguemnt used in \code{psych::corr.table}. Indicates how to
#'  calculate correlations with missing data. Defualt is \code{"pairwise"}.
#' @param method a string, same as the argument in \code{psych::corr.table}. Indicates the type of correlation
#'  coefficient to compute. Default is \code{"pearson"}
#' @param stats defaults to NULL and does not include any descriptive statistics. Otherwise should be a
#'  vector of strings naming the stats that \code{psych::describe} outputs and will be displayed below the
#'  correlation matrix
#' @param c.names a character vector of user-provided names for the names of the variables in the table.
#' @param change logical, indicates if you would like to change the column names of the table to \code{c.names}
#' @param numbered logical, indicates if you would like to use numbers for the column names
#' @param flagged logical, indicates if you would like to flag significant (p <. 05) correlations in the table
#'
#' @return a \code{data.frame} containing the correlation matrix and descriptive statistics
#' @export
#'
corr_table <- function(data,
                       sample_size = T,
                       use         = "pairwise",
                       method      = "pearson",
                       stats       = NULL,
                       c.names     = names(data),
                       change      = F,
                       numbered    = F,
                       flagged     = T){

  my_ifelse <- function(...){
    suppressWarnings(ifelse(...))
  }

  descriptives <- psych::describe(data) %>% tibble::as_data_frame()
  corr_data <- psych::corr.test(x = data, use = use, method = method)
  ns <- corr_data$n
  rs <- corr_data$r %>% round(2)

  descriptives <- descriptives %>%
    tibble::rownames_to_column() %>%
    tidyr::gather(stat,value,-rowname) %>%
    tidyr::spread(rowname,value) %>%
    dplyr::mutate_at(dplyr::vars(2:ncol(.)),dplyr::funs(round(.,2)))

  if(!is.null(stats)){
    descriptives <- descriptives %>%
      dplyr::filter(stat %in% stats) %>%
      dplyr::slice(match(stats, stat))
  }

  if(flagged){
    ps <- corr_data$p %>%
      as.data.frame() %>%
      dplyr::mutate_all(.funs = dplyr::funs(my_ifelse(as.numeric(.) < .01, "**", .))) %>%
      dplyr::mutate_all(.funs = dplyr::funs(my_ifelse(as.numeric(.) < .05 & !is.na(as.numeric(.)), "*", .))) %>%
      dplyr::mutate_all(.funs = dplyr::funs(my_ifelse(as.numeric(.) > .05 & !is.na(as.numeric(.)), "", .))) %>%
      as.matrix()

    flagged.rs <- paste(rs, ps,sep="") %>% matrix(nrow=nrow(rs),ncol=ncol(rs))

    rs[lower.tri(rs)] <- flagged.rs[lower.tri(flagged.rs)]
  }

  if(sample_size){
    if(length(ns) > 1){rs[upper.tri(rs)] <- ns[upper.tri(ns)]
    }else{rs[upper.tri(rs)] <- ns}
  }else{rs[upper.tri(rs)] <- NA}

  corrs <- rs %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(stat = rowname) %>%
    dplyr::mutate(stat = paste(1:nrow(.),". ",c.names,sep="")) %>%
    dplyr::mutate_at(.vars = 2:ncol(.),dplyr::funs(my_ifelse(. == 1,"-",as.character(.)))) %>%
    dplyr::add_row(.before=1,stat="Correlations") %>%
    dplyr::add_row(stat="Descriptives") %>%
    dplyr::mutate_at(.vars = 2:ncol(.),.funs = dplyr::funs(my_ifelse(is.na(.),"",.))) %>%
    dplyr::bind_rows(if(flagged){descriptives %>% purrr::map(as.character)}else{descriptives})

  if(change){names(corrs) <- c("Variable",c.names)}
  if(numbered){names(corrs) <- c("Variable",1:length(c.names))}
  corrs

}
