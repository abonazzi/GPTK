
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




#' Cat in Circle
#'
#'
#'

#' @keywords test.cat_in_circle
#' @export
#' @examples
#' test.cat_in_circle()
#'
#

test.cat_in_circle <- function(){

  # simulation length
  NSIM = 1e4

  # payout definition
  limit = 1e6
  payout = c(.2,.5,1) ; names(payout)<-c(3,4,5)

  # location details
  insured_lat=25.75
  insured_lon=-80.2
  circlesize = 50
  circleunit = 'miles' # km or miles only

  # track data
  path = "D:/Parametrix/IBTrACS.NA.list.v04r00.lines"
  shapefile_name = 'IBTrACS.NA.list.v04r00.lines'


  ibtracs <- read.shapefile (path,shapefile_name)
  ibtracs_ <-subset(ibtracs, WMO_WIND >=96)
  ibtracs_ <-subset(ibtracs_, SEASON >= 1900)
  ibtracs_ <-subset(ibtracs_, WMO_AGENCY =='hurdat_atl')


  # find tracks in circle

  out_1 <-storm.in.circle (ibtracs_,insured_lat, insured_lon,circlesize=50,circleunits='miles')

  # compute payout
  out_1$payout = payout[as.character(out_1$cat)] * limit

  out_1 <- out_1 %>% group_by(SEASON) %>% summarise(PAYOUT_YEARLY = sum(payout))

  store = data.frame(time = c(min(ibtracs_@data$SEASON):2019), stringsAsFactors = F )
  store = merge(store, out_1, by.x = 'time', by.y='SEASON', all.x = T) ; store[is.na(store)] = 0.


  # fit distribution

  par_a = fit.distribution(store$PAYOUT_YEARLY, "lognormal", zero_mass=F,DOPLOT=F)

  simulation = c( rlnorm(n=NSIM*(1-par_a[1,2]),meanlog = par_a[2,2], sdlog = par_a[3,2]) ,
     rep(0,(NSIM*par_a[1,2])))


  # compute payout statistics

  sim_stats = payout.statistics (simulation)
  bc_stats = payout.statistics(store$PAYOUT_YEARLY)

  return( rbind(sim_stats,bc_stats) )


}
