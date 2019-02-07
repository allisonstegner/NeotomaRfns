#map_dl________________________________________________________
# function to map location of sites
# tax_dec_dl is a Neotoma download object
# X is a vector with 2 elements: min and max longitude
# Y is a vector with 2 elements: min and max latitude
# add: should points be added to an existing map?
# color: color to use for points
# label.sites: should site names be added as text to the map

map_dl<-function(tax_dec_dl,X,Y,add,color,label.sites){
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
	
	out<-cbind(site.name,dataset.id,site.id,lat,long)
	#return(out)
}
