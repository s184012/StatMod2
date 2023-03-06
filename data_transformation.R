rm(list=ls())
library(caret)

raw = read.csv("dioxin.csv", header = TRUE, sep = ",")
print(sapply(raw, typeof))
raw <- as.data.frame(unclass(raw),stringsAsFactors = TRUE)
raw$TIME = as.factor(raw$TIME)
print(sapply(raw, typeof))
m = colMeans(raw[c(1:2,9:21)], na.rm = T)
std = sapply(raw[,c(1:2,9:21)],sd, na.rm = T)
preproc = preProcess(raw, method = "knnImpute")
data = predict(preproc, raw)
data$PRSEK[15:16] = c("L","L")
sum(is.na(data))
m = unname(m)
std = unname(std)
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
m = rep.row(m, 57)
std = rep.row(std, 57)
data[,c(1:2,9:21)] = data[,c(1:2,9:21)]*std + m

save(data, file='data.rds')
