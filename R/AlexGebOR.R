#' Calculate Odds Ratio with Confidence Intervals
#'
#' @param coef Coefficient Estimate from model summary output
#' @param se Standard Error from model summary output
#' @param siglevel Desired alpha/significance level
#' @param roundto Preferred number of decimals for rounding
#'
#' @return OR with CI
#' @export
#'
#' @examples
#' OR_95CI(2,1,0.95,3)
#' "7.389 (6.940, 7.867)"
#'

OR_95CI <- function(coef, se, siglevel, roundto) {
  if (!is.numeric(coef) || !is.numeric(se) || !is.numeric(siglevel) || !is.numeric(roundto)) {
    stop("All inputs must be numeric")
  }

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


