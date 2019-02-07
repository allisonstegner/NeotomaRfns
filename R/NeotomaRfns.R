#select.high.res________________________________________________________
# function to select ids for datasets where number of years represented per pollen sample is less than max.grain
# tax_dec_dl is a Neotoma download object
# max.grain is the maximum allowable number of years represented per pollen sample
# returns a vector of dataset ids for sites that have fine enough resolution.
# also returns a matrix of dataset ids, durations, and resolutions

select_high_res<-function(pol_dlx,max.grain){ 		
	
	chron.table<-matrix(NA,nr=length(pol_dlx),nc=3)
	for (i in 1:length(pol_dlx)) {
		pol_ds<-pol_dlx[[i]]
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
		chron.table[i,1]<-names(pol_dlx[i])
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

