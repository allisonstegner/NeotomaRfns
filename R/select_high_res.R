#' Function to compile sampling resolution information for a set of Neotoma sites
#'
#' This function identifies total duration and number of samples/duration for a Neotoma download 
#' @param dl_obj Neotoma download object
#' @param max.grain integer; The maximum allowable number of years represented per pollen sample
#' @return A list object containing a vector of dataset ids for sites that have resolution less than max.grain and a matrix of dataset ids, durations, and resolutions
#' @author M. Allison Stegner
#' @export

select_high_res<-function(dl_obj,max.grain){ 		
	
	chron.table<-matrix(NA,nr=length(dl_obj),nc=3)
	for (i in 1:length(dl_obj)) {
		pol_ds<-dl_obj[[i]]
		n.pollen.samples<-nrow(pol_ds$counts) #number of samples for site i
		
		handlej<-pol_ds$dataset$dataset.meta$collection.handle # get site handle
		handle.path<-paste(path,"/",handlej,sep="") 
		am.file<-dir(handle.path,pattern="ages.txt") 
		am.path<-paste(handle.path,"/",am.file,sep="")
		am<-read.table(am.path,header=T)
	
		minage<-min(am,na.rm=T)
		maxage<-max(am,na.rm=T)	
		duration<-maxage-minage
		
		#fill chron.table with relevant information: dataset.id, duration of the time series, and number of sample/time
		chron.table[i,1]<-names(dl_obj[i])
		if (n.pollen.samples<2){		#if there are less than 2 sample, enter NAs. These sites will get cut later
			chron.table[i,2]<-NA
			chron.table[i,3]<-NA
		}
		chron.table[i,2]<-duration
		chron.table[i,3]<-duration/n.pollen.samples
		}
	
	colnames(chron.table)<-c("dataset.id","duration","time.per.pollen.sample")
	
	#evaluate dataset resolution: if n.sample/time is less than the pre-defined maximum grain, keep the site
	chron.table.include<-chron.table[which(as.numeric(chron.table[,3])<=max.grain),]
	
	out<-list(dataset.ids=chron.table.include[,1],chron.table=chron.table)
	return(out)
}

