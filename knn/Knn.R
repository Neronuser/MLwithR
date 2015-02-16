library(class)
library(gmodels)
setwd("/Users/roman/MLwithR/MLwithRscripts/knn")
wisc <- read.csv("wisc_bc_data.csv", stringsAsFactors=F)
wisc <- wisc[-1]
wisc$diagnosis <- factor(wisc$diagnosis,levels=c("B","M"),labels=c("Benign","Malignant"))
normalize <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}
normalize(c(1,2,3,4,5,6,7,8,2500))
wisc_norm <- as.data.frame(lapply(wisc[2:31],normalize))
wisc_train <- wisc_norm[1:469,]
wisc_test <- wisc_norm[470:569,]
wisc_train_labels <- wisc[1:469,1]
wisc_test_labels <- wisc[470:569,1]
k<-round(sqrt(length(wisc[,1])))
train_result <- knn(train=wisc_train,test=wisc_test,cl=wisc_train_labels,k)
CrossTable(x=wisc_test_labels,y=train_result,prop.chisq=F)

# experimenting with z-score normalization instead of min-max
wisc_z <- as.data.frame(scale(wisc[-1]))
wisc_train <- wisc_z[1:469,]
wisc_test <- wisc_z[470:569,]
train_result <- knn(train=wisc_train,test=wisc_test,cl=wisc_train_labels,k)
CrossTable(x=wisc_test_labels,y=train_result,prop.chisq=F)
