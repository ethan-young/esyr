#' Perform a simple bivaraite correlation
#'
#' @param var1 a string indicating the 1st variable you want to correlate
#' @param var2 a string indicating the 2nd variable you want to correlate
#' @param data a \code{data.frame} containing the variables you are correlating
#' @param method a string, the same as the method argument that is normally passed to \code{cor}, indicates which
#'  correlation coefficient to compute. Must be "pearson" (default), "kendall", or "spearman".
#' @param use a string, the same 'use' arguemnt normally passed to \code{cor}. Indicates what to do with
#'  missing values. Options must be "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (default)
#'
#' @return this fucntion returns a list containing the following values:
#'
#' \describe{
#'   \item{vars}{variables that were correlated}
#'   \item{eff}{numeric value representing the correlation coefficient}
#'   \item{n}{sample size}
#'   \item{report}{string that provides the correlation coefficient and sample size}
#'   \item{method}{the method used to compute the correlation coefficient}
#' }
#'
#' @export
corr_report <- function(var1,var2,data,method = "pearson", use = "pairwise.complete.obs"){
  n <- data %>%
    dplyr::select(var1,var2) %>%
    tidyr::drop_na() %>%
    nrow()

  r <- cor(data[,var1],data[,var2], method = method, use = use) %>%
    as.numeric() %>%
    round(2)

  list(vars   = c(var1,var2),
       eff    = r,
       n      = n,
       report = paste0("r = ", r, ", n = ",n),
       method = method)

}
