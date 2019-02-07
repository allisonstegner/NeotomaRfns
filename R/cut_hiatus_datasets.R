#' Function to identify datasets that have excessivley long hiatuses
#'
#' This function identifies Neotoma dastasets which have hiatuses of user-defined length, and returns a list of dataset ids for the remaining sites (which do not have hiatuses)
#' @param dl_obj Neotoma download object
#' @param prop.length proportionate hiatus length, as a propotion of the total dataset duration
#' @param agemodels list object of agemodels. Can be provided by user, or downloaded from Neotoma. If downloaded from Neotoma, default chronology is used.
#' @return A list of Neotoma dataset ids for sites that do not have hiatuses
#' @author M. Allison Stegner
#' @export


cut_hiatus_datasets<-function(dl_obj,prop.length.cutoff,agemodels){
	dataset.ids<-c()
	for (i in 1:length(dl_obj)){
		#print(i)
				
		# either supply a list of default/preferred chronology names that match Neotoma
		# or identify Neotoma default chronologies via the chron control table
		# accessing the chron control table is slow, so better to use a list of 
		# default chrons from the min.chron.control function
		if (is.null(agemodels)){	
			chronj<-dl_obj[[i]]$sample.meta$age
		} else {	
			chronj<-agemodels[names(agemodels) %in% names(dl_obj[i])][[1]]	
		}
		
		wait.times<-chronj[-1]-chronj[-length(chronj)]
		chronj.dur<-max(chronj,na.rm=T)-min(chronj,na.rm=T)
		define.hiatus<-chronj.dur*prop.length.cutoff
		if (sum(wait.times[complete.cases(wait.times)]>define.hiatus)==0){
			dataset.ids<-c(dataset.ids,dl_obj[[i]]$dataset$dataset.meta$dataset.id)		
		} else { next }
	}
	output<-dataset.ids
	return(output)
}
