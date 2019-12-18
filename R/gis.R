#' Read Shapefile
#'
#'
#' @param path
#' @param shapefile_name
#' @keywords read.shapefile
#' @import rgdal
#' @export
#' @examples
#'
#' read.shapefile()
#'
#

read.shapefile <- function(path,shapefile_name) {


  # pay out terms  (still to add)

  shapefile <-readOGR(path,shapefile_name)

  return(shapefile)

}




#' Storm In a Circle
#'
#'
#' @param ibracs shapefile with track data
#' @param insured_lat lat centre
#' @param insured_lon lon centre
#' @param circlesize  radius of circle
#' @param circleunits radius units. It defaults to miles
#' @keywords storm.in.circle
#' @import dplyr
#' @import raster
#' @import rgeos
#' @export
#' @examples
#'
#' storm.in.circle()
#'
#


storm.in.circle <- function(ibtracs,insured_lat, insured_lon,circlesize,circleunits='miles') {


if(circleunit > 'miles'){
  circlesize = circlesize * 1609.344
} else {
  circlesize = circlesize * 1000
}


# Buffer circles by 50 and 100 miles (works on lat, lon)
insured_loc <- SpatialPoints(coords = cbind(insured_lon,insured_lat),
                             proj4string = CRS(proj4string(ibtracs)))
circle <- buffer(insured_loc, width = circlesize)



result <- rgeos::intersect(ibtracs_, circle)

outdf <- result@data %>% group_by(SID,NAME,SEASON) %>% summarise(Value = max(WMO_WIND))

outdf <- outdf %>% mutate(cat = case_when(Value >=137 ~ 5,
                           Value >=113 & Value < 137 ~ 4,
                           Value >=96 & Value < 113 ~ 3))



return(outdf)

}


#' Doughnut Join
#'
#' anti_join
#' what's in outer but not inner
#'
#'
#' @param outerdf outer df
#' @param innerdf inner df
#' @keywords circle.anti_join
#' @import rgdal
#' @export
#' @examples
#'
#' circle.anti_join()
#'
#


circle.anti_join <- function(outerdf,innerdf){


  outerdf <- anti_join(outerdf, innerdf, by = c("SID" = "SID"))

  return(outerdf)

}


