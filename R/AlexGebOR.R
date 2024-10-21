#' Calculate Odds Ratio with Confidence Intervals
#'
#' @param coef Coefficient Estimate from model summary output
#' @param se Standard Error from model summary output
#' @param siglevel Desired alpha/significance level
#' @param roundto Preferred number of decimals for rounding
#'
#' @return
#' @export
#'
#' @examples
#'
OR_95CI <- function(coef, se, siglevel, roundto){
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
  )
  return(ORresult)
}
