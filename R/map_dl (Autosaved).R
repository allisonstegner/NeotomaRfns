#' Map a Neotoma Download Obect
#'
#' Function to map location of Neotoma sites
#' @param dl_obj a Neotoma download object
#' @param X numeric; longitude
#' @param Y numeric; latitude
#' @param add logical; should points be added to an existing map?
#' @param col what color should the points be?
#' @param label.sites logical; should dataset ids be added as text labels?
#' @param return.table logical; should a matrix containing site name, dataset id, long, and lat be returned?
#' @return A four column matrix containing site name, dataset ids, longitudes, and latitudes
#' @author M. Allison Stegner
#' @export

map_dl<-function(dl_obj,X,Y,add,col=NULL,label.sites=FALSE,return.table=TRUE,cex=NULL){
	require(maps)
	lat<-c()
	long<-c()
	dataset.id<-c()
	site.name<-c()
	site.id<-c()
	for (i in 1:length(dl_obj)){
		long[i]<-dl_obj[[i]]$dataset$site.data$long
		lat[i]<-dl_obj[[i]]$dataset$site.data$lat
		dataset.id[i]<-dl_obj[[i]]$dataset$dataset.meta$dataset.id
		site.name[i]<-dl_obj[[i]]$dataset$site.data$site.name
		site.id[i]<-dl_obj[[i]]$dataset$site.data$site.id
	}
	
	if (return.table==TRUE){
		out<-as.data.frame(cbind(site.name,dataset.id,site.id,lat,long))
		return(out)
	} else {
		if (add==FALSE){
			map("world",xlim=X,ylim=Y)
			map("state",add=T)
			box()
		} 
		if (!is.null(col)) points(long,lat,pch=16,col=col,cex=cex)
		else points(long,lat,pch=1)

		if (label.sites==TRUE) text(long,lat,site.id,cex=0.5,pos=4,offset=0.2)
	}	
}
