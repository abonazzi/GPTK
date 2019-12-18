

limit = 1e6


insured_lat=25.75
insured_lon=-80.2
circlesize = 50
circleunit = 'miles' # km or miles only

path = "D:/Parametrix/IBTrACS.NA.list.v04r00.lines"
shapefile_name = 'IBTrACS.NA.list.v04r00.lines'



ibtracs <- read.shapefile (path,shapefile_name)
ibtracs_ <-subset(ibtracs, WMO_WIND >=96)
ibtracs_ <-subset(ibtracs_, SEASON >= 1900)
ibtracs_ <-subset(ibtracs_, WMO_AGENCY =='hurdat_atl')

out_1 <-storm.in.circle (ibtracs_,insured_lat, insured_lon,circlesize=50,circleunits='miles')

payout = c(.2,.5,1) ; names(payout)<-c(3,4,5)

out_1$payout = payout[as.character(out_1$cat)] * limit

out_1 <- out_1 %>% group_by(SEASON) %>% summarise(PAYOUT_YEARLY = sum(payout))

store = data.frame(time = c(min(ibtracs_@data$SEASON):2019), stringsAsFactors = F )
store = merge(store, out_1, by.x = 'time', by.y='SEASON', all.x = T) ; store[is.na(store)] = 0.

payout - payout.statistics (store$PAYOUT_YEARLY)



