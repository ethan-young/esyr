#' Winzorize outliers above a particular standardized value
#'
#' @param x a numeric vector of values to winsorize
#' @param z a single numeric value indicating the threshold above which
#'  extreme scores should be winsorized.
#'
#' @return a numeric vector with original values with extreme values replaced
#'  with the next extreme value in the distribution. The number of replacemnets
#'  and replacement values depend on the \code{z} argument.
#' @export
#'
winsor_z <- function(x,z){
  winsorized <- x
  winsor.scores <- which(x > (mean(x,na.rm=T) + z * sd(x,na.rm=T)))
  next.score <- x[which(x < (mean(x,na.rm=T) + z * sd(x,na.rm=T)))] %>% max()
  if(length(winsor.scores) < 1){
    message("- no values above ", z, " SDs")
    return(x)
  } else {
    winsorized[winsor.scores] <- next.score
    message(paste("- Winsorizing scores above", z, "SDs","\n-- extreme values are: "),
            paste(c(x[winsor.scores]),collapse=", "),
            paste("\n--- Replacing extreme values with: ", next.score))
    return(winsorized)
  }
}
