#' Function to extract coordinates from a Neotoma download object
#'
#' This function extracts coordinates from a Neotoma download object
#' @param pol_dl_obj Neotoma download object
#' @return A three column matrix containing dataset ids, longitudes, and latitudes
#' @author M. Allison Stegner
#' @export

extract_coords<-function(pol_dl_obj){
	coords<-matrix(NA,nc=3,nr=length(pol_dl_obj))
	for (i in 1:length(pol_dl_obj)){
		coords[i,]<-c(pol_dl_obj[[i]]$dataset$dataset.meta$dataset.id,pol_dl_obj[[i]]$dataset$site.data$long,pol_dl_obj[[i]]$dataset$site.data$lat)	
	}
	return(coords)
}