#' Function to identify datasets with at least n samples
#'
#' This function identifies Neotoma dastasets which have at least a user-defined minimum number of samples 
#' @param dl_obj Neotoma download object
#' @param n integer; minimum number of samples
#' @return A list of Neotoma dataset ids that have at least n samples
#' @author M. Allison Stegner
#' @export

ts_min_length<-function(dl_obj,n){
	dataset.ids<-c() # this will be filled with the Neotoma dataset ids for datasets that meet the inclusion criteria
	for (i in 1:length(dl_obj)) {
		sitei<-dl_obj[[i]]
		n.pollen.samples<-nrow(sitei$counts) #number of samples for site i
		if (n.pollen.samples<n){ 
			dataset.ids[i]<-NA
		} else {
			dataset.ids[i]<-sitei$dataset$dataset.meta$dataset.id
		}
	}
		return(adequate.n=dataset.ids)
}
