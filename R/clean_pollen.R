#' Function to clean pollen datasets from Neotoma
#'
#' This function compiles pollen using Neotoma-defined pollen lists or selects user-specified ecological groups
#' And optionally limits to abundant taxa
#' Then calculates pollen percent or returns counts  
#' @param pol_ds a single site from a Neotoma download object
#' @param type If type="pct", return is proportion. Else, counts are returned.
#' @param topN integer; allows user to limit result to the topN most abundant taxa. Default (NULL) is to return all taxa.
#' @param eco.group Neotoma ecological group codes. e.g., UPHE=upland herbs
#' @param list.name Neotoma pre-defined pollen list name 
#' @return A matrix of pollen counts or proportions
#' @author M. Allison Stegner
#' @export

clean_pollen<-function(pol_ds,type,topN=NULL,eco.group,list.name){
			
			counts.subset <-compile_taxa(pol_ds,list.name)
			sitei.counts <- counts.subset$counts
			
			keep.taxa<-counts.subset$taxon.list[counts.subset$taxon.list$ecological.group %in% eco.group,"compressed"]
			
			sitei.counts<-sitei.counts[,which(colnames(sitei.counts) %in% keep.taxa)]
			
			if (sum(colnames(sitei.counts) %in% "Other")>0){
				sitei.counts<-sitei.counts[,-which(colnames(sitei.counts)=="Other")]
			}
		
			if (is.numeric(topN)==T){
				abundance.order<-order(colSums(sitei.counts),decreasing=T)
				pollen<-sitei.counts[,abundance.order[1:topN]]
			} else {
				pollen<-sitei.counts
			}
				
			if (type=="pct") pollen<-pollen/rowSums(pollen,na.rm=TRUE)
		
			rownames(pollen)<-pol_ds$sample.meta$age
			#pollen<-pollen[which(rowSums(pollen)>0),]
			#ages<-as.numeric(rownames(pollen))
		
			return(pollen)
	}

