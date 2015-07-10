## Functions to create and process hapgen 2 files

library(qdap) #needed for the following
## Do hapgen 2 run

## Convert hapgen file to 1 column per genotype - 100,010,or 001 
geno.comb <-function(dat.sub){
  del.snp <-which(dat.sub$V4=="-") #identify snps with missing minor alleles
  dat.sub <-dat.sub[-del.snp,] # and delete them
  vec <-seq(6,ncol(dat.sub)-1,3)
  tst <-lapply(vec,function(x) colpaste2df(dat.sub,x:(x+2),sep="",keep.orig=FALSE))
  tst2 <-lapply(tst,function(x) x[,ncol(x)])
  tst3 <-do.call(cbind,tst2)
  colnames(tst3) <-c(1:ncol(tst3))
  out <-cbind.data.frame(dat.sub[,2:5],tst3,stringsAsFactors=FALSE)
  out
}  
  ## Convert geno.comb output to base pairs

cv2base <-function(genos){
  h1 <-paste(genos[3],genos[3],sep="")
  h2 <-paste(genos[4],genos[4],sep="")
  het <-paste(genos[3],genos[4],sep="")
  genos <-gsub("\\b100\\b",h1,genos)
  genos <-gsub("\\b001\\b",h2,genos)
  genos <-gsub("\\b010\\b",het,genos)
  genos
}