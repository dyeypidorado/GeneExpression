#####Define marker set######
marker = read.csv(file="GeneExpression/mark.csv", header=TRUE)

######Define gene set######
gene = read.csv(file="GeneExpression/geneexpression.csv", header=TRUE)

######Define FL478 gene set######
flgene = gene[, 1:3]

######Define IR29 gene set######
irgene = gene[, 4:6]

######Set means of the gene sets######
flmean = rowMeans(flgene)
irmean = rowMeans(irgene)

######Set midparent######
midg = (flmean+irmean)/2

######Set ADDg######
fl478 = mark[, 140]
numfl478 = sum(fl478 == 2)
addg = (flmean - midg)/numfl478

######Set Y_gc#####
ygc = matrix(,19, ncol(marker),byrow = F)
for(i in (1:ncol(marker))){
numpc = sum(marker[,i] == 2)
ygc[,i] = midg + (addg * numpc) 
}