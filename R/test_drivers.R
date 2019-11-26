
#' Test Deal - Example
#'
#' testing function
#'

#' @keywords test.deal
#' @export
#' @examples
#' test.compute.cdf()
#'
#

test.deal <- function(){

  data(example_of_timeseries)

  cdf = compute.cdf(time=example_of_timeseries$Year, index = example_of_timeseries$Index)

  sim = simulate.cdf (cdf$x,cdf$p,N=1e5)

  payout = deal.1.payout  (sim$s,strike=2,exit=10,tick=1e4)

  stats = payout.statistics (payout)

  return(stats)
}
