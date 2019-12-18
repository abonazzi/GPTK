#' Requires vectors describing a time serie to
#' provide a linearly detrended time series
#'
#'
#'
#' @param time Vector of timestamps, e.g. years
#' @param index Vector of Index values
#' @param DOPLOT Boolean, whether to plot or not. It defauts to False
#' @keywords compute.cdf
#' @export
#' @examples
#' linear.detrending(time,idex,DOPLOT=F)
#'
#

linear.detrending=function(time,index,DOPLOT=F){
  slope=cov(time,index)/var(time)
  detrended_index=index+slope*(max(time)-time)
  if(DOPLOT){
    plot(time,index,type="l")
    points(time,detrended_index,type="l",col=2)
  }
  return(detrended_index)
}


#' Provide log-normal MLE
#'
#' @param index Vector of Index values
#' @keywords estimate.lognormal
#' @export
#' @examples
#' estimate.lognormal(idex)
#'
#

estimate.lognormal=function(index){
  parameters=vector(length=2,mode="numeric")
  parameters[1]=mean(log(index))
  parameters[2]=sqrt(var(log(index)))
  return(parameters)
}



#' Provides Gamma method of moments estimates
#'
#' @param index Vector of Index values
#' @keywords estimate.gamma_mm
#' @export
#' @examples
#' estimate.gamma_mm(idex)
#'
#

estimate.gamma_mm=function(index){
  parameters=vector(length=2,mode="numeric")
  parameters[1]=((mean(index))^2)/var(index)
  beta=parameters[2]=var(index)/mean(index)
  return(parameters)
}

#' Fits selected distribution, providing a CDF graph and the vector of the parameters
#'
#' @param index Vector of Index values
#' @param distribution
#' @param zero_mass Boolean, zero mass distribution. It defaults to FALSE
#' @param DOPLOT Boolean, whether to plot
#' @keywords fit.distribution
#' @export
#' @examples
#' fit.distribution(index,distribution,zero_mass,DOPLOT)
#'
#


fit.distribution=function(index, distribution, zero_mass=F,DOPLOT=F){
  if(DOPLOT){
    plot(ecdf(index),main="Cumulative Distribution Function")
  }
  if(zero_mass==F){
    if(distribution=="lognormal"){
      parameters=estimate.lognormal(index)
      mi=parameters[1]
      s=parameters[2]
      max=qlnorm(.999,mi,s)
      min=qlnorm(.001,mi,s)
      if(DOPLOT){
        xaxis=seq(min,max,.01)
        points(xaxis,plnorm(xaxis,mi,s),type="l",col=2)
      }
      return(data.frame(row.name=c("mi","sigma"),parameters))
    }
    if(distribution=="gamma"){
      parameters=estimate.gamma_mm(index)
      alpha=parameters[1]
      beta=parameters[2]
      max=qgamma(.999,shape=alpha,scale=beta)
      min=qgamma(.001,shape=alpha,scale=beta)
      if(DOPLOT){
        xaxis=seq(min,max,.01)
        points(xaxis,pgamma(xaxis,shape=alpha,scale=beta),type="l",col=2)
      }
      return(data.frame(row.name=c("alpha","beta"),parameters))
    }
  }
  else{
    p=sum(index==0)/length(index) #Zero mass probability
    if(distribution=="lognormal"){
      positive_index=index[index>0]
      parameters=estimate.lognormal(positive_index)
      mi=parameters[1]
      s=parameters[2]
      if(DOPLOT){
        min=0
        max=qlnorm(.999,mi,s)
        xaxis=seq(min,max,.01)
        points(xaxis,p+(1-p)*plnorm(xaxis,mi,s),type="l",col=2)
      }
      return(data.frame(row.name=c("p","mi","sigma"),c(p,parameters)))
    }
    if(distribution=="gamma"){
      positive_index=index[index>0]
      parameters=estimate.gamma_mm(positive_index)
      alpha=parameters[1]
      beta=parameters[2]
      max=qgamma(.999,shape=alpha,scale=beta)
      min=qgamma(.001,shape=alpha,scale=beta)
      if(DOPLOT){
      xaxis=seq(min,max,.01)
      points(xaxis,p+(1-p)*pgamma(xaxis,shape=alpha,scale=beta),type="l",col=2)
      }
      return(data.frame(row.name=c("p","alpha","beta"),c(p,parameters)))
    }
  }
}





