#' Perform a simple multiple regression and get back a standardized beta
#'
#' @param IV a string indicating the name of the independent variable.
#' @param DV a string indicating the name of the dependent variable.
#' @param Covariate a string indicating the name of a covariate.
#' @param data a \code{data.frame} that you want to use for the analysis.
#'
#' @return This function returns a list containing the following items:
#'
#' \describe{
#'   \item{vars}{variables that were used in the model}
#'   \item{model_lm}{the lm object with undstandardized betas}
#'   \item{model_lm_z}{the lm.beta obeject with standardized betas}
#'   \item{table}{table of betas and standardized betas with standard error}
#'   \item{n}{sample size}
#'   \item{eff}{standarized beta weight for the IV}
#'   \item{report}{string that provides the beta weight and sample size}
#' }
#'
#' @export
#'
lm_beta_report <- function(IV,DV,Covariate,data){

  dv <- data %>%
    dplyr::pull(DV)
  iv <- data %>%
    dplyr::pull(IV)
  covariate <- data %>%
    dplyr::pull(Covariate)

  model_lm <- lm(dv ~ iv + covariate)
  model_lm_z <- lm.beta(model_lm)

  coef_table <- summary(model_lm_z) %>%
    coef() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "term") %>%
    dplyr::mutate(term = stringr::str_remove_all(term,"\\(|\\)"),
                  term = ifelse(term=="iv",IV,term),
                  term = ifelse(term=="covariate",Covariate,term)) %>%
    dplyr::select(1,2,4,3) %>%
    dplyr::rename(b = Estimate,B = Standardized)

  B <- coef_table %>%
    dplyr::filter(term == IV) %>%
    dplyr::pull(B)
  n <- model_lm$model %>%
    nrow()

  list(
    vars       = c(DV,IV,Covariate),
    model_lm   = model_lm,
    model_lm_z = model_lm_z,
    table      = coef_table,
    n          = n,
    eff        = round(B, 2),
    report     = paste0("B = ", round(B, 2), ", n = ", n)
  )
}

#' @importFrom lm.beta lm.beta
NULL
