#' Function to transform a pollen time series
#'
#' This function to transforms a pollen time series using one of several methods
#' @param ts a vector of pollen data
#' @param method method of transforming the time series. Options are asin.sqrt, log, logit, sqrt, or hellinger 
#' @return A transformed vector
#' @author M. Allison Stegner
#' @export

pollen_transform<-function(ts,method){
	ts1<-ts/100
	
	if (method=="asin.sqrt"){
		ts2<-cbind(ts[,1],asin(sqrt(ts1[,2])))
	} else if (method=="log"){
		ts2<-cbind(ts[,1],log(ts1[,2]))
	} else if (method=="logit"){
		ts2<-cbind(ts[,1],car::logit(ts1[,2]))
	} else if (method=="sqrt"){
		ts2<-cbind(ts[,1],sqrt(ts1[,2]))
	} else if (method=="hellinger"){
		print("hellinger is equivalent to sqrt if row sums = 1")
	} else {
		print("unrecognized method")
	}
	return(ts2)
}
