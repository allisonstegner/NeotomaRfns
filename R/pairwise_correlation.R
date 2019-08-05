#' Function to compute pairwise correlations on a matrix
#' This function calculates pairwise spearmanns correlations on a data matrix and returns rhos and ps

#' @param datamatx is a data matrix of species (rows) abundance per site or level (cols)
#' @return rhos A matrix of pairwise rhos
#' @return ps A matrix of pairwise p values
#' @author M. Allison Stegner
#' @export


pairwise_correlation<-function(datamatx,method,pval.adjust=F){
	rhos<-matrix(NA,nr=ncol(datamatx),nc=ncol(datamatx))
	ps<-matrix(NA,nr=ncol(datamatx),nc=ncol(datamatx))
	for (k in 1:(ncol(datamatx)-1)){
		vect1<-datamatx[,k]
		for (j in (k+1):ncol(datamatx)){
			vect2<-datamatx[,j]
			cor.out<-cor.test(vect1,vect2,method=method)
			rhos[j,k]<-cor.out$estimate[[1]]
			
			if(pval.adjust==T){
				n.tests<-(ncol(datamatx)*(ncol(datamatx)+1))/2
				ps[j,k]<-p.adjust(cor.out$p.value[[1]],method="holm",n=n.tests)
			} else {
				ps[j,k]<-cor.out$p.value[[1]]
			}
		}
	}
	
	rownames(rhos)<-colnames(rhos)<-rownames(ps)<-colnames(ps)<-colnames(datamatx)
	out<-list(rhos=rhos,ps=ps)
	return(out)
}
