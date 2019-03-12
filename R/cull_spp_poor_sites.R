#' Function to cull datasets thathave too few taxa
#'
#' This function identifies Neotoma dastasets which have at least a user-defined minimum number of taxa 
#' @param pol_dl Neotoma download object
#' @param min.spp integer; minimum number of samples
#' @return A Neotoma download object
#' @author M. Allison Stegner
#' @export


cull_spp_poor_sites<-function(pol_dl,min.spp){
	id<-c()
	for (i in 1:length(pol_dl)){
		pol_ds<-pol_dl[[i]]
		ntaxa<-ncol(pol_ds$counts)
		if (ntaxa<min.spp){
			id[[i]]<-NA
		} else {
			id[[i]]<-names(pol_dl[i])
		}	
	}
	out<-pol_dl[id[!is.na(id)]]
	return(out)
}

