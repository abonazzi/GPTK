
#' Deal 1 payout - Example
#'
#' Simple Strike, Exit and Tick value
#'
#' first implementation
#'
#' @param s simulation vector
#' @param strike strike
#' @param exit exit
#' @param tick tick value
#' @keywords linear.payout.1
#' @export
#' @examples
#' deal.1.payout(s,strike,exit,tick)
#'
#

deal.1.payout <- function (s,strike,exit,tick){


  return( pmin(exit, pmax(0,x-strike))*tick )


}
