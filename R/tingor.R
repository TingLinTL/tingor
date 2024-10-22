#' @title Odds Ratio and confidence intervals
#' @description a function that returns the odd ratio result and 95\% confidence intervals
#' @param coef a numeric value for probability
#' @param se a numeric value for stand error
#' @param siglevel a numeric value for significant level
#' @param roundto a numeric valuefor decimal places
#' @return a table for Odds Ratio, 95\% lower bound and upper bound
#' @author Ting Lin
#' @examples
#' coef <- c(0.2538319,0.2534135,0.5947923 )
#' se <- c(0.2538319, 0.2534135, 0.5947923 )
#' siglevel = 0.05
#' roundto = 4
#' OR_95CI(coef, se, siglevel, roundto)
#' @export
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
