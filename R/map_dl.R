#' Map a Neotoma Download Obect
#'
#' Function to map location of Neotoma sites
#' @param tax_dl_dl a Neotoma download object
#' @X numeric; longitude
#' @Y numeric; latitude
#' @add logical; should points be added to an existing map?
#' @color what color should the points be?
#' @label.sites logical; should dataset ids be added as text labels?
#' @return.table logical; should a matrix containing site name, dataset id, long, and lat be returned?
#' @return A four column matrix containing site name, dataset ids, longitudes, and latitudes
#' @author M. Allison Stegner
#' @export
#' map_dl()

map_dl<-function(tax_dec_dl,X,Y,add,color,label.sites,return.table){
	if (add==FALSE){
		map("world",xlim=X,ylim=Y)
	} 
	lat<-c()
	long<-c()
	dataset.id<-c()
	site.name<-c()
	site.id<-c()
	for (i in 1:length(tax_dec_dl)){
		long[i]<-tax_dec_dl[[i]]$dataset$site.data$long
		lat[i]<-tax_dec_dl[[i]]$dataset$site.data$lat
		points(long[i],lat[i],pch=16,col=color)
		dataset.id[i]<-tax_dec_dl[[i]]$dataset$dataset.meta$dataset.id
		site.name[i]<-tax_dec_dl[[i]]$dataset$site.data$site.name
		site.id[i]<-tax_dec_dl[[i]]$dataset$site.data$site.id
	}
	if (label.sites==TRUE){
		text(long,lat,site.id,cex=0.5,pos=4,offset=0.2)
	}
	
	if (return.table==TRUE){
		out<-cbind(site.name,dataset.id,site.id,lat,long)
		return(out)
	}
	
}
