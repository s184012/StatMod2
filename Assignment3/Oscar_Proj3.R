library(tidyverse)
library(dplyr)
library(DHARMa)
library(car)
library(caret)
library(nlme)
library(lme4)
library(MASS)
library(SuppDists)
library(GGally)
library(bbmle)
library(lme4)
library(glmmTMB)

rm(list=ls())

cloth = read.csv("clothingFullAss03.csv", header = T)
cloth$sex = as.factor(cloth$sex)
cloth$subjId = as.factor(cloth$subjId)
cloth$day = as.factor(cloth$day)
cloth$subDay = as.factor(cloth$subDay)

attach(cloth)

cloth
str(cloth)

library(mvtnorm)

#PART 1
nll <- function(par){
  #mu=par[1],betasex=par[2],sigma_u=par[3],sigma=par[4]
  mu <- rep(par[1],803)
  mu[sex=="male"] <- mu[sex=="male"]+par[2]
  SIGMA <- diag(rep(par[4],803))
  indexmatr <- matrix(subjId, nrow=length(subjId), ncol=length(subjId), byrow=TRUE)
  SIGMA[indexmatr==t(indexmatr)] <- SIGMA[indexmatr==t(indexmatr)]+par[3]
  
  -dmvnorm(clo, mean = mu, sigma=SIGMA,log=TRUE,checkSymmetry = TRUE)
}

(op1 <- nlminb(c(0.5,1,1,1),nll)   lower=c(-1,-Inf,0,0))

library(lme4)
(fit0 <- lmer(clo~sex+(1|subjId),REML=FALSE))


#PART 2 Sigma extra covariance if sub_i=sub_j and day_i=day_j

nll <- function(par){
  #mu=par[1],betasex=par[2],sigma_u2=par[3],sigma2=par[4],sigma_v2=par[5]
  mu <- rep(par[1],803)
  mu[sex=="male"] <- mu[sex=="male"]+par[2]
  SIGMA <- diag(rep(par[4],803))
  indexmatr <- matrix(subjId, nrow=length(subjId), ncol=length(subjId), byrow=TRUE)
  SIGMA[indexmatr==t(indexmatr)] <- SIGMA[indexmatr==t(indexmatr)]+par[3]
  
  indexmatr2 <- matrix(day, nrow=length(day), ncol=length(day), byrow=TRUE)
  INDEXmat <- indexmatr==t(indexmatr) & indexmatr2==t(indexmatr2)
  SIGMA[INDEXmat] <- SIGMA[INDEXmat] + par[5]
  
  -dmvnorm(clo, mean = mu, sigma=SIGMA,log=TRUE,checkSymmetry = TRUE)
}

(op2 <- nlminb(c(0.5,1,1,1,1), nll ,lower=c(-Inf,-Inf,0,0,0)) ) 
op2

library(lme4)
(fit1 <- lmer(clo~sex+(1|subjId)+(1|subjId:day),REML=FALSE))
op2

SIGMA <- diag(rep(par[4],803))


INDEXmat[1:40,1:40]
SIGMA[indexmatr==t(indexmatr)] <- SIGMA[indexmatr==t(indexmatr)]+par[3]






#PART 3 Sigma extra covariance if sub_i=sub_j and day_i=day_j

nll <- function(par){
  #mu=par[1],betasex=par[2],sigma_u2=par[3],sigma2=par[4],sigma_v2=par[5], alphasex = par[6]
  mu <- rep(par[1],803)
  mu[sex=="male"] <- mu[sex=="male"]+par[2]
  SIGMA <- diag(rep(par[4]*(1+par[6]),803))
  indexmatr <- matrix(subjId, nrow=length(subjId), ncol=length(subjId), byrow=TRUE)
  SIGMA[indexmatr==t(indexmatr)] <- SIGMA[indexmatr==t(indexmatr)]+par[3]*(1+par[6])
  
  indexmatr2 <- matrix(day, nrow=length(day), ncol=length(day), byrow=TRUE)
  INDEXmat <- indexmatr==t(indexmatr) & indexmatr2==t(indexmatr2)
  SIGMA[INDEXmat] <- SIGMA[INDEXmat] + par[5]*(1+par[6])
  
  -dmvnorm(clo, mean = mu, sigma=SIGMA,log=TRUE,checkSymmetry = TRUE)
}

(op3 <- nlminb(c(0.5,1,1,1,1,0), nll ,lower=c(-Inf,-Inf,0,0,0,0)) ) 
op3

library(lme4)
(fit1 <- lmer(clo~sex+(1|subjId)+(1|subjId:day),REML=FALSE))
op2


