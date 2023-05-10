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


#PART 1
library(mvtnorm)
nll <- function(par){
  #mu=par[1],betasex=par[2],sigma_u=par[3],sigma=par[4]
  mu <- rep(par[1],803)
  mu[sex=="male"] <- mu[sex=="male"]+par[2]
  SIGMA <- diag(rep(par[4],803))
  subject_matrix <- matrix(
    subjId, 
    nrow=length(subjId), 
    ncol=length(subjId), 
    byrow=TRUE
    )
  subject_index <- subject_matrix == t(subject_matrix)
  SIGMA[subject_index] <- SIGMA[subject_index]+par[3]
  -dmvnorm(clo, mean = mu, sigma=SIGMA,
           log=TRUE, checkSymmetry = FALSE)
}

simon_nll <- function(par){
  beta <- par[1:2]
  Sigma <- diag(rep(exp(par[3]), nrow(cloth)))
  sigma_u <- rep(exp(par[4]), nlevels(cloth$subjId))
  Psi <- diag(sigma_u)
  
  X <- model.matrix(~1 + sex, data=cloth)
  Z <- cbind(
    model.matrix(~-1 + subjId, data=cloth)
  )
  -dmvnorm(cloth$clo, mean = X%*%beta, sigma = Z%*%Psi%*%t(Z) + Sigma, log=T)
}

op1 <- nlminb(c(0.5, -.1, .1, .1),simon_nll)
op1$obj

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

simon_nll <- function(par){
  beta <- par[1:2]
  Sigma <- diag(rep(exp(par[3]), nrow(cloth)))
  sigma_u <- rep(exp(par[4]), nlevels(cloth$subjId))
  sigma_v <- rep(exp(par[5]), nlevels(cloth$subDay))
  Psi <- diag(c(sigma_u, sigma_v))
  
  X <- model.matrix(~1 + sex, data=cloth)
  Z <- cbind(
    model.matrix(~-1 + subjId, data=cloth),
    model.matrix(~-1 + subDay, data=cloth)
  )
  -dmvnorm(cloth$clo, mean = X%*%beta, sigma = Z%*%Psi%*%t(Z) + Sigma, log=T)
}

(op2 <- optim(c(0.5,-.1,.05,.1,.1), simon_nll, method="BFGS")) 
op2$par

library(lme4)
(fit1 <- lmer(clo~sex+(1|subjId)+(1|subjId:day),REML=FALSE))
op2

SIGMA <- diag(rep(par[4],803))


INDEXmat[1:40,1:40]
SIGMA[indexmatr==t(indexmatr)] <- SIGMA[indexmatr==t(indexmatr)]+par[3]






#PART 3 Sigma extra covariance if sub_i=sub_j and day_i=day_j

nll <- function(par){
  mu <- par[1]
  beta <- par[2]
  sigma_u2 <- par[3]
  sigma2 <- par[4]
  sigma_v2 <- par[5]
  alpha <- par[6]
  
  n_subjects <- length(subjId)
  is_male <- sex == "male" 
  subj_filter <- outer(subjId, subjId, FUN="==")
  subj_day <- paste(subjId, day)
  subj_day_filter <- outer(subj_day, subj_day, FUN="==")
  
  betamale <- is_male*beta
  mu <- rep(mu, n_subjects) + betamale
  alphamale <- 1 - is_male*alpha
  sigma2 <- diag(sigma2, n_subjects)
  sigma_v2 <- subj_day_filter*sigma_v2
  sigma_u2 <- subj_filter*sigma_u2
  SIGMA <- (sigma2 + sigma_u2 + sigma_v2)*alphamale
  
  -dmvnorm(clo, mean = mu, sigma=SIGMA,log=TRUE,checkSymmetry = FALSE)
}

simon_nll <- function(par){
  beta <- par[1:2]
  alpha <- exp(par[6]) - 1 # In range (-1, inf)
  weight <- 1 + (cloth$sex=="male")*alpha
  Sigma <- diag(rep(exp(par[3]), nrow(cloth)))
  sigma_u <- rep(exp(par[4]), nlevels(cloth$subjId))
  sigma_v <- rep(exp(par[5]), nlevels(cloth$subDay))
  Psi <- diag(c(sigma_u, sigma_v))
  
  X <- model.matrix(~1 + sex, data=cloth)
  Z <- cbind(
    model.matrix(~-1 + subjId, data=cloth),
    model.matrix(~-1 + subDay, data=cloth)
  )
  -dmvnorm(cloth$clo, mean = X%*%beta, sigma = (Z%*%Psi%*%t(Z) + Sigma)*weight, log=T)
}
(op3 <- optim(c(0.5, -0.5, 1, 1,1,-.5), nll, method="BFGS"))

library(lme4)
(fit1 <- lmer(clo~sex+(1|subjId)+(1|subjId:day),REML=FALSE))
op2


