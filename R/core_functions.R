
#' Compute CDF from timeseries
#'
#' first implementation
#'
#' @param time Vector of timestamps, e.g. years
#' @param index Vector of Index values
#' @keywords compute.cdf
#' @export
#' @examples
#' compute.cdf(time,idex)
#'
#

compute.cdf <- function(time, index){


  srt = sort(index, index.return=T,decreasing=T)

  n = length(srt$x)
  k = 1:n

  x = c(srt$x,min(0,min(srt$x)))

  # implementing type 3 (to be extended)

  pk =  (k - 1/3) / ( n + 1/3 )

  cdf = data.frame(x=srt$x,p=pk)

  return(cdf)
}



#' Perform simulation from CDF
#'
#' first implementation
#'
#' @param x Vector of levels
#' @param pk 1 minus cdf
#' @param N number of simulations
#' @keywords simulate.cdf
#' @export
#' @examples
#' simulate.cdf(x,cdf)
#'
#

simulate.cdf <- function(x,pk,N=1e5) {

  # doing simulation

  u = runif(1e5)

  s = x[findInterval(u,pk)+1]

  t = 1:N

  out = data.frame(t=t,s=s)

  return(out)

}




#' Payout statistics
#'
#' first implementation
#'
#' @param payout payout vector
#' @export
#' @examples
#' payout.statistic(payout)
#'
#

payout.statistics <- function(payout){


    out = c(mean(payout),quantile(payout,c(0.5,0.75,0.90,0.95,0.99,0.995)))
    names(out) = c('mean', paste('p',c(0.5,0.75,0.90,0.95,0.99,0.995),sep=''))

    return(out)
}
