rm(list=ls())

cloth = read.csv("clothingFullAss03.csv", header = T)
cloth$sex = as.factor(cloth$sex)
cloth$subjId = as.factor(cloth$subjId)
cloth$day = as.factor(cloth$day)
cloth$subDay = as.factor(cloth$subDay)

save(cloth, file = 'data.rds')