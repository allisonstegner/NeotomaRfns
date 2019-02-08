#' Function to identify datasets that are of sufficient duration
#'
#' This function identifies Neotoma dastasets which have at least a minimum amount of temporal coverage
#' @param dl_obj Neotoma download object
#' @param min.time numeric; minimum duration, in years. Sites with number of samples less than min.samples are excluded
#' @param agemodels list object of agemodels. Can be provided by user, or default age model from Neotoma is used.
#' @return A list of Neotoma dataset ids for sites that have adequate duration.
#' @author M. Allison Stegner
#' @export

ts_min_duration<-function(dl_obj,min.time,agemodels){
	dataset.ids<-c() # this will be filled with the Neotoma dataset ids for datasets that meet the inclusion criteria
	for (i in 1:length(dl_obj)) {
		sitei<-dl_obj[[i]]
		
		# choose the default chronology
		if (is.null(agemodels)){
			chroncont<-get_chroncontrol(sitei)
			default.chronology<-chroncont$meta$name		
			chronj<-sitei$chronologies[[i.chrons[default.chronology]]]	
			dur<-max(chronj$age,na.rm=T)-min(chronj$age,na.rm=T)
		} else {	
			chronj<-agemodels[names(agemodels) %in% names(dl_obj[i])][[1]]
			dur<-max(chronj,na.rm=T)-min(chronj,na.rm=T)
		}		
		
		if (dur<min.time){ next
		} else {
			dataset.ids<-c(dataset.ids,sitei$dataset$dataset.meta$dataset.id)
		}
	}
		return(dataset.ids=dataset.ids)
}
