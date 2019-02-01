### Normalize heart failure dataset and do t-test

#first, normalize the data using -log(cellvalue/rowsum)
dat = data.frame(read.csv("CleanerHeartTable.csv", header = TRUE))
normdat <- dat

for (i in 1:length(dat[,1])){
  mysum <- sum(dat[i,20:1336])
  for (j in 20:1336){
    normdat[i,j] <- -log(dat[i,j]/mysum)
  }
}

#write.csv(normdat, file="normdat.csv")


# next, perform ttest between groups A and c(B,C)
normdat <- read.csv("normdat.csv")
pframe <- normdat[1,]
colnames(pframe) <- colnames(normdat)

###I think this is wrong
# for (i in 20:1336){
#   pframe[i] <- t.test(normdat[1:21,i],normdat[22:43,i])["p.value"]
# }

for (i in 22:1337){
  pframe[i] <- t.test(unlist(normdat[i])~normdat$Status)["p.value"]
}

pframe <- pframe[,22:1337]
write.csv(pframe, file="newpvals.csv")

