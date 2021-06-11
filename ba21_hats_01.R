

library(brms)
library(rstan)
library(raw)
library(ChainLadder)
library(nlmeODE)
library(nlme)
library(R2OpenBUGS)
library(data.table)
library(StanHeaders)
library(ggplot2)
library(lattice)
library(deSolve)
library(remotes)
library(devtools)
library(RcppArmadillo)
library(pkgbuild)
library(tidyverse)
library(readxl)



############################################## 
### DATA
##############################################


# Apple
a=read.csv("/Users/.../wkcomp_pos.csv",header=TRUE)

# Windows
a=read.csv("C:/.../wkcomp_pos.csv",header=TRUE)

grp.code=unique(a$GRCODE)

# function to get Schedule P triangle data given ins group
ins.line.data=function(g.code){
  b=subset(a,a$GRCODE==g.code)
  name=b$GRNAME
  grpcode=b$GRCODE
  ay=b$AccidentYear
  dev=b$DevelopmentLag
  
  cum_incloss=b[,6]
  cum_pdloss=b[,7]
  bulk_loss=b[,8]
  dir_premium=b[,9]
  ced_premium=b[,10]
  net_premium=b[,11]
  single=b[,12]
  posted_reserve97=b[,13]
  
  # get incremental paid losses - assume data is sorted by ay and dev
  inc_pdloss=numeric(0)
  for (i in unique(ay)){
    s=(ay==i)
    pl=c(0,cum_pdloss[s])
    ndev=length(pl)-1
    il=rep(0,ndev)
    for (j in 1:ndev){
      il[j]=pl[j+1]-pl[j]
    }
    inc_pdloss=c(inc_pdloss,il)
  }
  data.out=data.frame(name,grpcode,ay,dev,net_premium,dir_premium,ced_premium,
                      cum_pdloss,cum_incloss,bulk_loss,inc_pdloss,single,posted_reserve97)
  return(data.out)
}

# Example
comauto=ins.line.data(grp.code[2])

# upper triangle
com.insample=subset(comauto,ay+dev<=1998)

# lower triangle
com.outsample=subset(comauto,ay+dev>1998)

# --------- cumulative paid claims triangle
wk_tri2 <- as.triangle(com.insample, origin = "ay",
                       dev = "dev",
                       value = "cum_pdloss")

# ---- outstanding claims triangle 
com.insample$out_claims <- com.insample$cum_incloss - com.insample$cum_pdloss


wk_tri1 <- as.triangle(com.insample, origin = "ay",
                       dev = "dev",
                       value = "out_claims")


#### ------ Preparation for predictions

# For munich CL
wk_tri3 <- as.triangle(com.insample, origin = "ay",
                       dev = "dev",
                       value = "cum_incloss")
wk_tri3

#lower triangle - actual claims 
Incurred2 <- as.triangle(com.outsample, origin = "ay",
                         dev = "dev",
                         value = "cum_pdloss")



# ---- outstanding claims triangle 
#compute outstanding claims
com.insample$out_claims <- com.insample$cum_incloss - com.insample$cum_pdloss
com.outsample$out_claims <- com.outsample$cum_incloss - com.outsample$cum_pdloss

# Go to Appendix D and initialize the GroupedData object
wk_tri1 <- as.triangle(com.insample, origin = "ay",
                       dev = "dev",
                       value = "out_claims")
wk_tri1_new <- cbind(unique(Dose)[-2], wk_tri1)



#lower triangle - actual claims 
Incurred1 <- as.triangle(com.outsample, origin = "ay",
                         dev = "dev",
                         value = "out_claims") 

#### ------ End preparation for predictions

# ---------------------------- Appendix D
### replica manually 

#Cohort
c1 <- rep(1988, 22)
c2 <- rep(1989, 20)
c3 <- rep(1990, 18)
c4 <- rep(1991, 16)
c5 <- rep(1992, 14)
c6 <- rep(1993, 12)
c7 <- rep(1994, 10)
c8 <- rep(1995, 8)
c9 <- rep(1996, 6)
c10 <- rep(1997, 4)

Cohort <- c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)

#t
t1 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10)
t2 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9)
t3 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8)
t4 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7)
t5 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6)
t6 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
t7 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4)
t8 <- c(0, 0, 1, 1, 2, 2, 3, 3)
t9 <- c(0, 0, 1, 1, 2, 2)
t10 <- c(0, 0, 1, 1)
t <- c(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)

#Claims

cl1 <- c(0, 0, rbind(wk_tri1[1 ,], wk_tri2[1 ,]))
cl2 <- c(0, 0, rbind(wk_tri1[2 ,], wk_tri2[2 ,]))
cl3 <- c(0, 0, rbind(wk_tri1[3 ,], wk_tri2[3 ,]))
cl4 <- c(0, 0, rbind(wk_tri1[4 ,], wk_tri2[4 ,]))
cl5 <- c(0, 0, rbind(wk_tri1[5 ,], wk_tri2[5 ,]))
cl6 <- c(0, 0, rbind(wk_tri1[6 ,], wk_tri2[6 ,]))
cl7 <- c(0, 0, rbind(wk_tri1[7 ,], wk_tri2[7 ,]))
cl8 <- c(0, 0, rbind(wk_tri1[8 ,], wk_tri2[8 ,]))
cl9 <- c(0, 0, rbind(wk_tri1[9 ,], wk_tri2[9 ,]))
cl10 <- c(0, 0, rbind(wk_tri1[10 ,], wk_tri2[10 ,]))

Claims <- c(cl1, cl2, cl3, cl4, cl5, cl6, cl7, cl8, cl9, cl10)
Claims <- Claims[!is.na(Claims)]
Claims <- na.omit(Claims)

#Type
Type <- as.factor(rep(c(1, 2), 65))

#Dose
dunique <- unique(comauto$dir_premium)

d1 <- c(dunique[1], rep(0, 21))
d2 <- c(dunique[2], rep(0, 19))
d3 <- c(dunique[3], rep(0, 17))
d4 <- c(dunique[4], rep(0, 15))
d5 <- c(dunique[5], rep(0, 13))
d6 <- c(dunique[6], rep(0, 11))
d7 <- c(dunique[7], rep(0, 9))
d8 <- c(dunique[8], rep(0, 7))
d9 <- c(dunique[9], rep(0, 5))
d10 <- c(dunique[10], rep(0, 3))

Dose <- c(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10)

#Cmt
Cmt <- as.factor(rep(1, 130))


Data <- cbind(Cohort, t, Claims, Type, Dose, Cmt);Data
Data <- as.data.frame(Data)
Data <- groupedData(Claims ~t | Cohort/Type, data = Data)

######
# Data_Full
###### 

# Needed for predictions

#Cohort
c1 <- rep(1988, 22)
c2 <- rep(1989, 22)
c3 <- rep(1990, 22)
c4 <- rep(1991, 22)
c5 <- rep(1992, 22)
c6 <- rep(1993, 22)
c7 <- rep(1994, 22)
c8 <- rep(1995, 22)
c9 <- rep(1996, 22)
c10 <- rep(1997, 22)

Cohort <- c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)

#t
t1 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10)
t2 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10)
t3 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10)
t4 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10)
t5 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10)
t6 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10)
t7 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10)
t8 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10)
t9 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10)
t10 <- c(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10)
t <- c(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)

#Claims

cl1 <- c(0, 0, rbind(data_in_full_2[1 ,], data_in_full[1 ,]))
cl2 <- c(0, 0, rbind(data_in_full_2[2 ,], data_in_full[2 ,]))
cl3 <- c(0, 0, rbind(data_in_full_2[3 ,], data_in_full[3 ,]))
cl4 <- c(0, 0, rbind(data_in_full_2[4 ,], data_in_full[4 ,]))
cl5 <- c(0, 0, rbind(data_in_full_2[5 ,], data_in_full[5 ,]))
cl6 <- c(0, 0, rbind(data_in_full_2[6 ,], data_in_full[6 ,]))
cl7 <- c(0, 0, rbind(data_in_full_2[7 ,], data_in_full[7 ,]))
cl8 <- c(0, 0, rbind(data_in_full_2[8 ,], data_in_full[8 ,]))
cl9 <- c(0, 0, rbind(data_in_full_2[9 ,], data_in_full[9 ,]))
cl10 <- c(0, 0, rbind(data_in_full_2[10 ,], data_in_full[10 ,]))

Claims <- c(cl1, cl2, cl3, cl4, cl5, cl6, cl7, cl8, cl9, cl10)
Claims <- Claims[!is.na(Claims)]
Claims <- na.omit(Claims)

#Type
Type <- as.factor(rep(c(1, 2), 65))

#Dose
dunique <- unique(comauto$dir_premium)

d1 <- c(dunique[1], rep(0, 21))
d2 <- c(dunique[2], rep(0, 21))
d3 <- c(dunique[3], rep(0, 21))
d4 <- c(dunique[4], rep(0, 21))
d5 <- c(dunique[5], rep(0, 21))
d6 <- c(dunique[6], rep(0, 21))
d7 <- c(dunique[7], rep(0, 21))
d8 <- c(dunique[8], rep(0, 21))
d9 <- c(dunique[9], rep(0, 21))
d10 <- c(dunique[10], rep(0, 21))

Dose <- c(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10)

#Cmt
Cmt <- as.factor(rep(1, 130))


Data_Full <- cbind(Cohort, t, Claims, Type, Dose, Cmt);Data_Full
Data_Full <- as.data.frame(Data_Full)
Data_Full <- groupedData(Claims ~t | Cohort/Type, data = Data_Full)

# End Data_Full

# ---------------------------------------------------------------- End Data Preparation


############################################## 
### FUNCTIONS - MORRIS 2016 PAPER 
##############################################


freq_model <- function(base_mod, time_dep, rand_eff_corr, block_diag_rand_eff, Data, plot){
  
  if(base_mod == T){
    
    # BASELINE STRUCTURAL MODEL
    DEmodel <- list( 
      DiffEq=list(
        dy1dt = ~ -lker*y1,
        dy2dt = ~ lker*lRLR*y1 - lkp*y2,
        dy3dt = ~ lkp*lRRF*y2),
      ObsEq=list(
        EX = ~ 0,
        OS = ~ y2,
        PA = ~ y3),
      States=c("y1","y2","y3"),
      Parms=c("lker","lRLR","lkp","lRRF"),
      Init=list(0,0,0))
    
    
    ReservingModel <<- nlmeODE(DEmodel,Data) ### “Data” = data in Appendix D
    nlmeModel <<- nlme(Claims ~ ReservingModel(lker,lRLR,lkp,lRRF,t,Cohort,Type), # Parameters of DiffEq + right side of GroupedData
                       data = Data, # Grouped Data
                       fixed = lker+lRLR+lkp+lRRF ~ 1, ### fixed-effect parameters
                       random = pdDiag(lRLR + lRRF ~ 1), ### parameters with random-effects - should be ultimate loss ratio
                       groups = ~Cohort, ### data grouping (accident years)
                       weights = varIdent(form = ~1 | Type), ### residual error functions: OS&PD
                       start = c(lker = log(1.5), lRLR = log(1),
                                 lkp = log(0.75), lRRF = log(0.75)), ### parameter starting values
                       control=list(returnObject=TRUE,msVerbose=TRUE, msMaxIter=10000,pnlsMaxIter=10000,
                                    pnlsTol=0.4), ### tolerance for PNLS convergence
                       verbose=TRUE)
    plot_func_signal <<- 1
    if (plot == T){
      plot_func(x = nlmeModel, y = plot_func_signal)
    }
    
  }
  
  if(time_dep == T){
    
    DEmodel2 <- list(
      DiffEq=list(
        dy1dt = ~ -lBer*t*y1,
        dy2dt = ~ lBer*t*lRLR*y1 - lkp*y2,
        dy3dt = ~ lkp*lRRF*y2),
      ObsEq=list(
        EX = ~ 0,
        OS = ~ y2,
        PA = ~ y3),
      States=c("y1","y2","y3"),
      Parms=c("lBer","lRLR","lkp","lRRF"),
      Init=list(0,0,0)) 
    
    ReservingModel2 <<- nlmeODE(DEmodel2,Data)
    
    nlmeModel2 <<- nlme(Claims ~ ReservingModel2(lBer,lRLR,lkp,lRRF,t,Cohort,Type),
                        data = Data,
                        fixed = lBer+lRLR+lkp+lRRF ~ 1,
                        random = pdDiag(lRLR + lRRF ~ 1),
                        groups = ~Cohort,
                        weights = varIdent(form = ~1 | Type),
                        start=c(lBer = log(5), lRLR = log(1.03),
                                lkp = log(0.45), lRRF = log(0.67)),
                        control=list(returnObject=TRUE,msVerbose=TRUE,
                                     msMaxIter=10000,pnlsMaxIter=10000,
                                     pnlsTol=0.4),
                        verbose=TRUE)
    plot_func_signal <<- 2
    
    
    if (plot == T){
      print(plot_func_signal)
      plot_func(x = nlmeModel2, y = plot_func_signal)
    }
  }
  
  if(rand_eff_corr == T){
    
    DEmodel3 <- list(
      DiffEq=list(
        dy1dt = ~ -lBer*t*y1,
        dy2dt = ~ lBer*t*lRLR*y1 - lkp*y2,
        dy3dt = ~ lkp*lRRF*y2),
      ObsEq=list(
        EX = ~ 0,
        OS = ~ y2,
        PA = ~ y3),
      States=c("y1","y2","y3"),
      Parms=c("lBer","lRLR","lkp","lRRF"),
      Init=list(0,0,0)) 
    
    ReservingModel3 <<- nlmeODE(DEmodel3,Data)
    
    nlmeModel3 <<- nlme(Claims ~ ReservingModel3(lBer,lRLR,lkp,lRRF,t,Cohort,Type),
                        data = Data,
                        fixed = lBer+lRLR+lkp+lRRF ~ 1,
                        random=list(lRLR+lRRF~1),
                        groups = ~Cohort,
                        weights = varIdent(form = ~1 | Type),
                        start=c(lBer = log(5), lRLR = log(1.03),
                                lkp = log(0.45), lRRF = log(0.67)),
                        control=list(returnObject=TRUE,msVerbose=TRUE,
                                     msMaxIter=10000,pnlsMaxIter=10000,
                                     pnlsTol=0.4),
                        verbose=TRUE)
    plot_func_signal <<- 3
    
    if (plot == T ){
      plot_func(x = nlmeModel3, y = plot_func_signal)
    }
  }
  
  if(block_diag_rand_eff == T){
    
    DEmodel4 <- list(
      DiffEq=list(
        dy1dt = ~ -lBer*t*y1,
        dy2dt = ~ lBer*t*lRLR*y1 - lkp*y2,
        dy3dt = ~ lkp*lRRF*y2),
      ObsEq=list(
        EX = ~ 0,
        OS = ~ y2,
        PA = ~ y3),
      States=c("y1","y2","y3"),
      Parms=c("lBer","lRLR","lkp","lRRF"),
      Init=list(0,0,0)) 
    
    ReservingModel4 <<- nlmeODE(DEmodel4,Data)
    
    nlmeModel4 <<- nlme(Claims ~ ReservingModel4(lBer,lRLR,lkp,lRRF,t,Cohort,Type),
                        data = Data,
                        fixed = lBer+lRLR+lkp+lRRF ~ 1,
                        random=pdBlocked(list(lRLR + lRRF~1, lkp ~ 1)),
                        groups = ~Cohort,
                        weights = varIdent(form = ~1 | Type),
                        start=c(lBer = log(5), lRLR = log(1.03),
                                lkp = log(0.45), lRRF = log(0.67)),
                        control=list(returnObject=TRUE,msVerbose=TRUE,
                                     msMaxIter=10000,pnlsMaxIter=10000,
                                     pnlsTol=0.4),
                        verbose=TRUE)
    plot_func_signal <<- 4
    if (plot == T){
      plot_func(x = nlmeModel4, y = plot_func_signal)
    }
  }
}


# ------------ Plot Function for nlmeModel

plot_func <- function(x, y){
  model <- x
  plot_func_signal = y
  print(plot_func_signal)
  data <- model$fitted[,2]
  wk_tri1 <- cbind(rep(0, nrow(wk_tri1)), wk_tri1)
  wk_tri2 <- cbind(rep(0, nrow(wk_tri2)), wk_tri2)
  time <- seq(from = 1988, to = 1997, by = 1)
  a <- which(model$fitted == 0)
  b <- seq(from = 1, to = length(a), by = 2)
  c <- a[b]
  
  matrix_fixed <- matrix(NA, ncol=10, nrow = 11) # outstanding
  matrix_cohort <- matrix(NA, ncol=10, nrow = 11) # paid
  
  for(i in 1:length(time)){
    s <- seq(from = c[i], to = c[i+1]-1, by = 1)
    odd <- seq(from = min(s), to =max(s), by = 2)
    even <- seq(from = (min(s)+1),to = max(s), by = 2)
    matrix_fixed[1:length(odd),i] <- data[odd]
    matrix_cohort[1:length(even),i] <- data[even]
  }
  
  data_I <- matrix_fixed + matrix_cohort
  
  # Start Prediction
  IndCoef <- coef(model)
  
  if (plot_func_signal == 1){
    p <- ReservingModel(rep(IndCoef[,1], each = 2*11),
                        rep(IndCoef[,2], each = 2*11),
                        rep(IndCoef[,3], each = 2*11),
                        rep(IndCoef[,4], each = 2*11),
                        Data_Full$t, Data_Full$Cohort, Data_Full$Type)
    
  }
  
  if (plot_func_signal == 2){
    p <- ReservingModel2(rep(IndCoef[,1], each = 2*11),
                         rep(IndCoef[,2], each = 2*11),
                         rep(IndCoef[,3], each = 2*11),
                         rep(IndCoef[,4], each = 2*11),
                         Data_Full$t, Data_Full$Cohort, Data_Full$Type)
    
  }
  
  if (plot_func_signal == 3){
    p <- ReservingModel3(rep(IndCoef[,1], each = 2*11),
                         rep(IndCoef[,2], each = 2*11),
                         rep(IndCoef[,3], each = 2*11),
                         rep(IndCoef[,4], each = 2*11),
                         Data_Full$t, Data_Full$Cohort, Data_Full$Type)
    
  }
  
  if (plot_func_signal == 4){
    p <- ReservingModel4(rep(IndCoef[,1], each = 2*11),
                         rep(IndCoef[,2], each = 2*11),
                         rep(IndCoef[,3], each = 2*11),
                         rep(IndCoef[,4], each = 2*11),
                         Data_Full$t, Data_Full$Cohort, Data_Full$Type)
    
  }
  
  title_time <- seq(from = 1988, to = 1997, by = 1)
  odd <- c(1,3,5,7,9,11,13,15,17,19,21)
  even <- c(2,4,6,8,10,12,14,16,18,20,22)
  pred_data_out = matrix(NA, nrow = 11, ncol = 10) # odd
  pred_data_paid = matrix(NA, nrow = 11, ncol = 10)# even
  
  iteration = 10
  l = 22
  
  for (i in 1:iteration){ # fill data into columns
    aim = i * l
    start = (aim - l) + 1
    dat_together = p[start:aim]
    dat_odd = dat_together[odd]
    dat_even = dat_together[even]
    pred_data_out[,i] = dat_odd
    pred_data_paid[,i] = dat_even
  }
  
  pred_data_out = as.data.frame(pred_data_out) 
  pred_data_paid = as.data.frame(pred_data_paid)
  colnames(pred_data_out) = title_time
  colnames(pred_data_paid) = title_time
  
  pred_data_inc = pred_data_out + pred_data_paid
  
  # End Prediction
  
  ## Plots
  
  # Outstanding and Paid together
  par(mfrow=c(2,5))
  for(i in 1:length(time)){
    plot(matrix_fixed[,i], type = "b", ylim = c(0,70000), ylab = "OS & PD", xlab = "Dev. Year", main = as.character(time[i]), cex.main = 3, cex.lab = 1.5, lwd = 3)
    lines(matrix_cohort[,i], col = "red", lwd = 3)
  }
  
  # Just outstanding
  for(i in 1:length(time)){
    plot(wk_tri1[i,], ylim = c(0,70000), type = "b",ylab = "OS", xlab = "Dev. Year", main = as.character(time[i]),  cex.main = 3, cex.lab = 1.5, lwd = 3) # out-claims
    lines(pred_data_out[i], col = "blue", lwd = 2)
  }
  
  # Just paid
  for(i in 1:length(time)){
    plot(wk_tri2[i,], ylim = c(0,70000), type = "b",ylab = "PD", xlab = "Dev. Year", main = as.character(time[i]),  cex.main = 3, cex.lab = 1.5, lwd = 3) # cum.paid loss
    lines(pred_data_paid[i], col = "blue", lwd = 2)
  }
  
  # Incurred
  for(i in 1:length(time)){
    plot(data_I[,i], ylim = c(0,90000), type = "b", col = "black", ylab = "INC", xlab = "Dev. Year", main = as.character(time[i]),  cex.main = 3, cex.lab = 1.5, lwd = 3)
    lines(pred_data_inc[i], col = "blue", lwd = 2)
  }
  
  coefs <- model$coefficients
  RLR <- exp(coefs$random$Cohort[,1])
  RRF <- exp(coefs$random$Cohort[,2])
  par(mfrow=c(1,2))
  plot(RLR, type = "b", col = "red", ylim = c(0,1.5), ylab="RLR and RRF estimates", xlab = "Accident Year", main = "RRF and RLR estimates",  cex.main = 1.5, cex.lab = 1.5, lwd = 3)
  lines(RRF, type = "b", col  = "blue", lwd = 3)
  
  premiums <- unique(Dose); premiums <- premiums[-2]
  plot(premiums, type = 'b', main = "Premium", ylab = "Premiums",  cex.main = 3, cex.lab = 1.5, lwd = 3)
  par(mfrow=c(1,1))
}


# End plot function

# All frequentist models can be created with this function:
freq_model(base_mod = 0, time_dep = 1, rand_eff_corr=0, block_diag_rand_eff = 0, Data = Data, plot = 1)


############################################## 
### PREDICTIONS FOR TRIANGLES
##############################################

# ---- Predictions
#Actual incurred claims
IncurredTotal <- Incurred1 + Incurred2
IncurredTotal10 <- as.numeric(c(wk_tri1[1, 10] + wk_tri2[1, 10], IncurredTotal[, 9]))



#Classic mack CL
mack <- MackChainLadder(wk_tri1+wk_tri2, est.sigma="Mack")
mack10 <- as.numeric(mack$FullTriangle[, 9])



#Munich mack CL
MackMunich <- MunichChainLadder(wk_tri2, wk_tri3, # Paid # Incurred
                                est.sigmaP = "Mack", est.sigmaI = "Mack",
                                tailP=F, tailI=F)
MackMunich10 <- as.numeric(MackMunich$MCLIncurred[, 10])

cbind(unique(Cohort),IncurredTotal10, mack10, MackMunich10)

# ----- End Predictions

############################################## 
### OpenBUGS
##############################################

# OpenBUGS models is implemented with wkcomp data

model <- function() {
  for (i in 1:n.ind) {
    for (j in 1:1) {
      data_O[i, j] ~ dnorm(mean_O[i, j] , tau_O)
      data_P[i, j] ~ dnorm(mean_P[i, j] , tau_P)
      data_I[i, j] <- data_O[i, j] + data_P[i, j]
      mean_O[i, j] <- solution[i,j,2]
      mean_P[i, j] <- solution[i, j, 3]
      mean_I[i, j] <- mean_O[i, j] + mean_P[i, j]
    }
    for (j in 2:n.grid) {
      data_O[i, j] ~ dnorm(mean_O[i, j] , tau_O2)
      data_P[i, j] ~ dnorm(mean_P[i, j] , tau_P2)
      data_I[i, j] <- data_O[i, j] + data_P[i, j]
      mean_O[i, j] <- solution[i, j, 2] + rho2 * (data_O[i, j-1] - mean_O[i, j-1])
      #Calendar shock substitution
      #mean_O[i, j] <- solution[i,j,2] * (1 - C[i,j] * a[i]) + rho2 *(data_O[i, j-1] - #mean_O[i, j-1])
      mean_P[i, j] <- solution[i, j, 3] + rho3 * (data_P[i, j-1] - mean_P[i, j-1])
      mean_I[i, j] <- mean_O[i, j] + mean_P[i, j]
    }
    theta[i, 1:p] ~ dmnorm(mu[1:p], omega.inv[1:p, 1:p])
    param[i, 1] <- theta[i, 1]
    param[i, 2] <- theta[i, 2]
    param[i, 3] <- theta[i, 3]
    param[i, 4] <- theta[i, 4]
    param[i, p+1] <- prem[i]
    Ber[i] <- exp(theta[i, 1])
    RLR[i] <- exp(theta[i, 2])
    kp[i] <- exp(theta[i, 3])
    RRF[i] <- exp(theta[i, 4])
    ULR[i] <- RLR[i] * RRF[i]
    ILR10[i] <- data_I[i, 10] / prem[i]
    solution[i, 1:n.grid, 1:dim] <- ode(inits[i, 1:dim],
                                        grid[1:n.grid], D(A[i, 1:dim], t[i]), origin, tol)
    D(A[i, 1], t[i]) <- -Ber[i] * t[i] * A[i, 1]
    D(A[i, 2], t[i]) <- Ber[i] * t[i] * RLR[i] * A[i, 1] - kp[i] * A[i, 2]
    D(A[i, 3], t[i]) <- kp[i] * RRF[i] * A[i, 2]
    
    #Calendar shock substitution
    #D(A[i, 3], t[i]) <- kp[i] * RRF[i] * (1 - V[i]*a[i]) * A[i, 2]
    #V[i] <- step((i + t[i]) - 10)
    #a[i] ~ dunif(-0.99,0.99)
    
    inits[i, 1] <- prem[i] 
    inits[i, 2] <- 0 
    inits[i, 3] <- 0
  }
  mu[1:p] ~ dmnorm(mu.prior.mean[1:p], mu.prior.prec[1:p, 1:p])
  omega.inv[1:p, 1:p] ~ dwish(omega.inv.matrix[1:p, 1:p], omega.inv.dof)
  omega[1:p, 1:p] <- inverse(omega.inv[1:p, 1:p])
  ResC <- omega[2, 4] / (sqrt(omega[2, 2]) * sqrt(omega[4, 4]))
  sigma_O ~ dunif(0, 10000)
  tau_O <- pow(sigma_O, -2)
  sigma_O2 <- sigma_O * sqrt(1 - pow(rho2, 2))
  tau_O2 <- pow(sigma_O2, -2)
  sigma_P ~ dunif(0, 5000)
  tau_P <- pow(sigma_P, -2)
  sigma_P2 <- sigma_P * sqrt(1 - pow(rho3, 2))
  tau_P2 <- pow(sigma_P2, -2)
  rho2 ~ dunif(-1,1)
  rho3 ~ dunif(-1,1)
  #Standardized residuals
  for (i in 1:n.ind) {
    for (j in 1:1) {
      r_O[i,j] <- (data_O[i, j] - mean_O[i, j] ) * sqrt(tau_O)
      r_P[i,j] <- (data_P[i, j] - mean_P[i, j] ) * sqrt(tau_P)
    }
    for (j in 2:n.grid) {
      r_O[i,j] <- (data_O[i, j] - mean_O[i, j] ) * sqrt(tau_O2)
      r_P[i,j] <- (data_P[i, j] - mean_P[i, j] ) * sqrt(tau_P2)
    }
  }
}

# parameters <- c("theta", "mu","omega.inv", "sigma_O","sigma_P","rho2","rho3")
# write.model(model, con = "model.txt", digits = 5) initial model

model.file <- file.path(tempdir(), "model.txt") # new approach
write.model(model, model.file)


# Data and prior parameters

data <- list(
  p = 4, dim = 3,
  origin = 0.0,
  tol = 1.0E-6,
  n.ind = 10, n.grid = 10,
  grid = c(1,2,3,4,5,6,7,8,9,10),
  prem = c(104437, 88883, 85956, 99339, 104897, 119427, 110784, 77731, 63646, 48052),
  mu.prior.mean = c(1.7, -0.15, -0.9, -0.21),
  mu.prior.prec = structure(
    .Data = c(
      650, 0, 0, 0,
      0, 380, 0, 0,
      0, 0, 5400, 0,
      0, 0, 0, 390),
    .Dim = c(4, 4)),
  omega.inv.matrix = structure(
    .Data = c(
      1, 0, 0, 0,
      0, 1, 0, 0.8,
      0, 0, 1, 0,
      0, 0.8, 0, 1),
    .Dim = c(4, 4)),
  omega.inv.dof = 4,
  data_O = structure(.Data = c(
    53121, 41222, 32309, 24944, 17104, 13137, 9605, 6515, 1661, 1322,
    54145, 37188, 26976, 20015, 14319, 10179, 6672, 2575, 2071, NA,
    55211, 37221, 27760, 17990, 11417, 6716, 4282, 3015, NA, NA,
    60617, 42144, 25987, 14805, 9406, 5792, 3966, NA, NA, NA,
    65719, 46047, 31250, 22245, 11878, 8408, NA, NA, NA, NA,
    68133, 51102, 39934, 21824, 16955, NA, NA, NA, NA, NA,
    62434, 46661, 32248, 24140, NA, NA, NA, NA, NA, NA,
    56971, 48677, 35336, NA, NA, NA, NA, NA, NA, NA,
    56526, 41707, NA, NA, NA, NA, NA, NA, NA, NA,
    40799, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    .Dim = c(10,10)),
  data_P = structure(.Data = c(
    9558, 22778, 33298, 40348, 45146, 48048, 49782, 50623, 51812, 51939,
    7913, 19472, 29622, 36816, 40975, 43302, 44707, 45871, 46229, NA,
    8744, 24302, 35406, 43412, 48057, 50897, 52879, 53956, NA, NA,
    13301, 32950, 47201, 56394, 61650, 65039, 66566, NA, NA, NA,
    11424, 29086, 42034, 50910, 56406, 59437, NA, NA, NA, NA,
    11792, 27161, 38229, 46722, 50742, NA, NA, NA, NA, NA,
    11194, 26893, 38488, 45580, NA, NA, NA, NA, NA, NA,
    12550, 31604, 44045, NA, NA, NA, NA, NA, NA, NA,
    13194, 31474, NA, NA, NA, NA, NA, NA, NA, NA,
    9372, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    .Dim = c(10,10))
)

p <- data$p
dim <- data$dim
origin <- data$origin
tol <- data$tol
n.ind <- data$n.ind
n.grid <- data$n.grid
grid <- data$grid
prem <- data$prem
mu.prior.mean <-data$mu.prior.mean
mu.prior.prec <- data$mu.prior.prec
omega.inv.matrix <- data$omega.inv.matrix
omega.inv.dof <- data$omega.inv.dof
data_O <- data$data_O
data_P <- data$data_P


data <- list("p", "dim", "origin", "tol", "n.ind", "n.grid", "grid", "prem", "mu.prior.mean", "mu.prior.prec",
             "omega.inv.matrix", "omega.inv.dof", "data_O", "data_P")
length(data)


# Initial values (1)
inits <- function(){
  list(
    rho2 = 0.5,
    rho3= 0.5,
    sigma_O = 5000,
    sigma_P = 500,
    mu = c(1.7, -0.15, -0.9, -0.21),
    omega.inv = structure(
      .Data = c(
        10, 0, 0, 0,
        0, 10, 0, 0.8,
        0, 0, 10, 0,
        0, 0.8, 0, 10),
      .Dim = c(4, 4)),
    theta = structure(
      .Data = c(
        1.7, -0.15, -0.9, -0.21,
        1.7, -0.15, -0.9, -0.21,
        1.7, -0.15, -0.9, -0.21,
        1.7, -0.15, -0.9, -0.21,
        1.7, -0.15, -0.9, -0.21,
        1.7, -0.15, -0.9, -0.21,
        1.7, -0.15, -0.9, -0.21,
        1.7, -0.15, -0.9, -0.21,
        1.7, -0.15, -0.9, -0.21,
        1.7, -0.15, -0.9, -0.21),
      .Dim = c(10, 4)))
}

params = c("rho2", "rho3", "sigma_O", "sigma_P", "mu", "omega.inv", "theta")



out <- bugs(data, inits, params, model.file,n.chains = 3, n.iter=1000, debug = TRUE)




# Initial values (2)
inits2 <- function(){
  list(
    rho2 = 0.6,
    rho3= 0.2,
    sigma_O = 3000,
    sigma_P = 700,
    mu = c(1.4, -0.07, -0.2, -0.51),
    omega.inv = structure(
      .Data = c(
        15, 0, 0, 0,
        0, 15, 0, 0.5,
        0, 0, 15, 0,
        0, 0.5, 0, 15),
      .Dim = c(4, 4)),
    theta = structure(
      .Data = c(
        1.4, -0.07, -0.2, -0.51,
        1.4, -0.07, -0.2, -0.51,
        1.4, -0.07, -0.2, -0.51,
        1.4, -0.07, -0.2, -0.51,
        1.4, -0.07, -0.2, -0.51,
        1.4, -0.07, -0.2, -0.51,
        1.4, -0.07, -0.2, -0.51,
        1.4, -0.07, -0.2, -0.51,
        1.4, -0.07, -0.2, -0.51,
        1.4, -0.07, -0.2, -0.51),
      .Dim = c(10, 4))
  )
}


# Initial values (3)
inits3 <- function(){
  list(
    rho2 = 0.2,
    rho3= 0.6,
    sigma_O = 1500,
    sigma_P = 1000,
    mu = c(1.1, 0, 0, -0.29),
    omega.inv = structure(
      .Data = c(
        5, 0, 0, 0,
        0, 5, 0, 0.3,
        0, 0, 5, 0,
        0, 0.3, 0, 5),
      .Dim = c(4, 4)),
    theta = structure(
      .Data = c(
        1.1, 0, 0, -0.29,
        1.1, 0, 0, -0.29,
        1.1, 0, 0, -0.29,
        1.1, 0, 0, -0.29,
        1.1, 0, 0, -0.29,
        1.1, 0, 0, -0.29,
        1.1, 0, 0, -0.29,
        1.1, 0, 0, -0.29,
        1.1, 0, 0, -0.29,
        1.1, 0, 0, -0.29),
      .Dim = c(10, 4))
  )
}


############################################## 
### rstan - MORRIS 2016 PAPER
##############################################

# rstan models is implemented with wkcomp data


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


####################
### IMPORT SWISSRE DATA
####################

# VERSION 2

# Libraries

path_data <- "C:/...."

# Choose between "prop", "liab", "motor"
portfolio <- "liab"

# Choose between "Property", "Liability - Prop & Direct", "Motor - Prop & Direct Reinsura"
sheetname <- "Liability - Prop & Direct"

#### Rectangle Cumulative ####
# Incurred 
dat_rect_inc <- read_excel(path = paste(path_data, "/2019-loss-ratio-development-doc.xlsx", sep = ""), 
                           sheet = sheetname, 
                           range = anchored("H11", dim =c(9,8), col_names = FALSE))

# Paid 
dat_rect_paid<- read_excel(path = paste(path_data, "/2019-loss-ratio-development-doc.xlsx", sep = ""), 
                           sheet = sheetname, 
                           range = anchored("H30", dim =c(9,8), col_names = FALSE))

# Oustanding
dat_rect_out <- dat_rect_inc - dat_rect_paid

# Premium
prem <- read_excel(path = paste(path_data, "/2019-loss-ratio-development-doc.xlsx", sep = ""), 
                   sheet = sheetname, 
                   range = anchored("F11", dim =c(9,1), col_names = FALSE))

# Outstanding + Paid
dat_rect <- data.frame(accident_year = rep(c(2004:2011), 2),
                       rbind(dat_rect_out, dat_rect_paid),
                       type = c(rep("OS",nrow(dat_rect_out)), rep("Paid", nrow(dat_rect_paid))), 
                       premium = prem)

names(dat_rect) <- c("accident_year" , c(1:8), "type", "premium")

dat_rect_long_OS = dat_rect_long[dat_rect_long$type == "OS",]
dat_rect_long_Paid = dat_rect_long[dat_rect_long$type == "Paid",]

# Transform from wide to long
dat_rect_long <- dat_rect %>%
  pivot_longer(cols = c(2:9),
               names_to = "dev", 
               values_to = "loss_ratio") %>%
  mutate(dev = as.integer(dev))



# Visualization
dat_rect_long %>%
  ggplot(mapping = aes( x = dev, 
                        y = loss_ratio, 
                        col = factor(type), 
                        shape = testf))+
  facet_wrap(~accident_year)+
  geom_line()+
  geom_point()

####################
### MULTISTAGE MODEL WITH RUNGE-KUTTA
####################

load("C:/.../2x_1k_runs.RData")

myFuns <- "
real[] ode_lossemergence(real t, real [] y, real [] theta,
real [] x_r, int[] x_i){
real dydt[3];
real ke = theta[1];
real dr = theta[2];
real kp1 = theta[3];
real kp2 = theta[4];
dydt[1] = pow(ke, dr) * pow(t, dr - 1) * exp(-t * ke)/tgamma(dr)
- (kp1 + kp2) * y[1];
dydt[2] = kp2 * (y[1] - y[2]);
dydt[3] = (kp1 * y[1] + kp2 * y[2]);
return dydt;
}
real int_lossemergence(real t, real ke, real dr,
real kp1, real kp2){
real y0[3]; real y[1, 3]; real theta[4];
y0[1] = 0; y0[2] = 0; y0[3] = 0;
theta[1] = ke;
theta[2] = dr;
theta[3] = kp1;
theta[4] = kp2;
y = integrate_ode_rk45(ode_lossemergence,
y0, 0, rep_array(t, 1), theta,
rep_array(0.0, 0), rep_array(1, 1),
0.0001, 0.0001, 500); // tolerances, steps
return (y[1, 3]);
}
real lossemergence(real t, real devfreq, real ke, real dr,
real kp1, real kp2){
real out = int_lossemergence(t, ke, dr, kp1, kp2);
if(t > devfreq){ // paid greater dev period 1
// incremental paid
out = out - int_lossemergence(t - devfreq, ke, dr, kp1, kp2);
}
return(out);
}
"

frml <- bf(loss_ratio ~ eta, # loss ratio prediction 
           nlf(eta ~ log(ELR * lossemergence(dev, 1.0, ke, dr, kp1, kp2))), nlf(ke ~ exp(oke * 0.5)),
           nlf(dr ~ 1 + 0.1 * exp(odr * 0.5)),
           nlf(kp1 ~ 0.5 * exp(okp1 * 0.5)),
           nlf(kp2 ~ 0.1 * exp(okp2 * 0.5)),
           ELR ~ 1 + (1 | accident_year),
           oke ~ 1 + (1 | accident_year),
           odr ~ 1 + (1 | accident_year),
           okp1 ~ 1 + (1 | accident_year),
           okp2 ~ 1 + (1 | accident_year), 
           nl = TRUE)



mypriors <- c(prior(inv_gamma(4, 2), nlpar = "ELR", lb=0), prior(normal(0, 1), nlpar = "oke"),
              prior(normal(0, 1), nlpar = "odr"),
              prior(normal(0, 1), nlpar = "okp1"),
              prior(normal(0, 1), nlpar = "okp2"), 
              prior(student_t(10, 0, 0.1), class = "sd", nlpar = "ELR"), 
              prior(student_t(10, 0, 0.1), class = "sd", nlpar = "oke"), 
              prior(student_t(10, 0, 0.1), class = "sd", nlpar = "odr"), 
              prior(student_t(10, 0, 0.1), class = "sd", nlpar = "okp1"), 
              prior(student_t(10, 0, 0.1), class = "sd", nlpar = "okp2"), 
              prior(student_t(10, 0, 1), class = "sigma")) 

fit_loss <- brm(frml, prior = mypriors,
                data = dat_rect_long_Paid, 
                family = lognormal(), 
                seed = 12345, 
                stanvars = stanvar(scode = myFuns, block = "functions"),
                control = list(adapt_delta = 0.9, max_treedepth=7), iter = 1000, chains = 2)

fit_loss

x <- posterior_samples(fit_loss, "^b")
mySummary <- function(x){
  c(Estimate = mean(x), Est.Error = sd(x),
    `l-95% CI` = as.numeric(quantile(x, probs = 0.025)),
    `u-95% CI` = as.numeric(quantile(x, probs = 0.975)))
}

estims = rbind(
  ELR = mySummary(x[, 'b_ELR_Intercept']),
  ke = mySummary(exp(x[, 'b_oke_Intercept'] * 0.5)),
  dr = mySummary(1 + 0.1 * exp(x[, 'b_odr_Intercept'] * 0.5)),
  kp1 = mySummary(0.5 * exp(x[, 'b_okp1_Intercept'] * 0.5)),
  kp2 = mySummary(0.1 * exp(x[, 'b_okp2_Intercept'] * 0.5))
)
estims



expose_functions(fit_loss, vectorize = TRUE)

fit_loss_pred <- predict(fit_loss, newdata = dat_rect_long_Paid, method="predict")
fit_loss_pred_all <- cbind(dat_rect_long_Paid, fit_loss_pred)


xyplot(Q2.5 + Q97.5 + loss_ratio + Estimate ~ 
         dev | factor(accident_year), 
       data=fit_loss_pred_all, layout = c(4,2),
       ylab = "Paid Loss Ratio",
       xlab = "Devolopment year",
       par.settings = list(strip.background=list(col="#CBDDE6")),
       scales=list(alternating=1),
       as.table=TRUE, type = "l", col = c("grey","grey",2,4), lwd = c(0.5 , 0.5, 2,2),
       main = "Paid Loss Ratio"
)

################# now OS



fit_loss_OS <- brm(frml, prior = mypriors, # prediction for incr_lr
                   data = dat_rect_long_OS, 
                   family = lognormal(), 
                   seed = 12345, 
                   stanvars = stanvar(scode = myFuns, block = "functions"),
                   control = list(adapt_delta = 0.9, max_treedepth=7), iter = 1000, chains = 2)

fit_loss_OS

x <- posterior_samples(fit_loss_OS, "^b")
mySummary <- function(x){
  c(Estimate = mean(x), Est.Error = sd(x),
    `l-95% CI` = as.numeric(quantile(x, probs = 0.025)),
    `u-95% CI` = as.numeric(quantile(x, probs = 0.975)))
}

estims_OS = rbind(
  ELR = mySummary(x[, 'b_ELR_Intercept']),
  ke = mySummary(exp(x[, 'b_oke_Intercept'] * 0.5)),
  dr = mySummary(1 + 0.1 * exp(x[, 'b_odr_Intercept'] * 0.5)),
  kp1 = mySummary(0.5 * exp(x[, 'b_okp1_Intercept'] * 0.5)),
  kp2 = mySummary(0.1 * exp(x[, 'b_okp2_Intercept'] * 0.5))
)
estims_OS



expose_functions(fit_loss_OS, vectorize = TRUE)

fit_loss_pred_OS <- predict(fit_loss_OS, newdata = dat_rect_long_OS, method="predict")
fit_loss_pred_all_OS <- cbind(dat_rect_long_OS, fit_loss_pred_OS)


xyplot(Q2.5 + Q97.5 + loss_ratio + Estimate ~ 
         dev | factor(accident_year), 
       data=fit_loss_pred_all_OS, layout = c(4,2),
       ylab = "OS Loss Ratio",
       xlab = "Devolopment year",
       par.settings = list(strip.background=list(col="#CBDDE6")),
       scales=list(alternating=1),
       as.table=TRUE, type = "l", col = c("grey","grey",2,4), lwd = c(0.5 , 0.5, 2,2),
       main = "OS Loss Ratio"
)


mcmc_plot(fit_loss, type="dens_overlay")
mcmc_plot(fit_loss_OS, type="dens_overlay")

#### End multistage

####################
### ANALYTICAL SOLUTION OF TWO TANKS - CAS Data
####################

# This model is implemented with wkcomp data

CASdata <- read.csv("C:/.../wkcomp_pos.csv",header=TRUE)


CASdata <- as.data.table(CASdata)

createLossData2 <- function(CASdata, company_code){
  compData <- CASdata[GRCODE==company_code,
                      c("EarnedPremDIR_D", "AccidentYear", "DevelopmentLag", 
                        "IncurLoss_D", "CumPaidLoss_D", "BulkLoss_D")]
  setnames(compData, names(compData),
           c("premium", "accident_year", "dev",
             "incurred_loss", "paid_loss", "bulk_loss"))
  compData <- as.data.table(compData)
  compData <- compData[, `:=`(origin = accident_year - min(accident_year) + 1)]
  compData0 <- compData[dev==1]
  compData0 <- compData0[, `:=`(dev = 0, incurred_loss = 0,
                                paid_loss = 0, bulk_loss = 0)]
  compData <- rbindlist(list(compData0, compData))
  compData <- compData[, cal := origin + dev - 1][order(origin, dev)] # neue Spalte und ordnen nach origin, dann dev
  compData <- compData[, `:=`(
    paid_train = ifelse(cal <= max(origin), paid_loss, NA),
    paid_test = ifelse(cal > max(origin), paid_loss, NA),
    os_train = ifelse(cal <= max(origin), incurred_loss - paid_loss, NA),
    os_test = ifelse(cal > max(origin), incurred_loss - paid_loss, NA))]
  traintest <- rbindlist(list(compData[cal <= max(origin)],
                              compData[cal > max(origin)]))
  return(traintest)
}
lossData <- createLossData2(CASdata, company_code = 337)

########################### ------ Plot


key <- list(
  rep=FALSE, 
  lines=list(col=c("#00526D", "purple"), 
             type=c("p", "p"), 
             pch=c(19, 19)),
  text=list(lab=c("Paid observation","OS observation"))) 

xyplot(paid_loss + os_train ~  
         dev | factor(accident_year), data=lossData, t = c("b", "b"), 
       pch=c(19, 19),
       as.table=TRUE,
       xlab="Development Year",
       ylab="Loss ratio (%)",
       main="Paid & OS claims developments by accident year",
       sub=paste("Data source: CAS Loss Reserving Database,",
                 "Workers' Comp 337.",
                 337),
       scales=list(alternating=1), layout=c(5,2), key=key,
       par.settings = list(strip.background=list(col="#CBDDE6")),
       par.strip.text = list(font = 2))


lossData0 <- 
  rbindlist(
    list(lossData[, list(accident_year, dev, loss_train=os_train, 
                         loss_test=os_test, delta=0, premium=premium)],
         lossData[,list(accident_year, dev,loss_train=paid_train, 
                        loss_test=paid_test, delta=1, premium=premium)]
    ))[order(accident_year, dev)]

premind <- lossData0[accident_year==min(accident_year) & dev==min(dev) & delta==1, premium]
lossData0 <- lossData0[, `:=`(premium = premium/premind,
                              loss_train = loss_train/premind,
                              loss_test = loss_test/premind,
                              deltaf = factor(delta, labels = c("os", "paid")),
                              cal=accident_year + dev - 1)][dev>0]

### ANALYTICAL SOLUTION TWO TANKS

my.f <- function(t, premium, lk_er,  lk_p_1, lk_p_2, lRLR, lRRF, delta){
  k_er <- exp(lk_er)  
  k_p_1 <- exp(lk_p_1)
  k_p_2 <- exp(lk_p_2)
  RLR <- exp(lRLR) 
  RRF <- exp(lRRF)
  
  os_1 <- ((RLR * k_er) / (k_er - k_p_1 - k_p_2) * (exp(-(k_p_1 + k_p_2) * t) - exp(-k_er * t)))
  
  os_2 <- ((RLR * k_er * k_p_2) / (k_p_1* (k_p_2 - k_er) * (k_er - k_p_1 - k_p_2))) * (
    exp(-(k_p_1 + k_p_2)*t) * (k_er - k_p_2) - exp(-k_p_2 * t)*(k_er - k_p_1 - k_p_2) - 
      exp(-k_er * t) * k_p_1)
  
  paid <- ((RLR * RRF) / (k_p_1 * (k_p_2 - k_er) * (k_er - k_p_1 - k_p_2)) * 
             ((k_p_1 * (k_er * (k_p_1 - k_er) - k_p_2 * (k_p_1 + k_p_2)) + 2*k_er*k_p_2) +
                exp(-(k_p_1 + k_p_2)*t) * (k_er * (k_er * k_p_1 - k_er*k_p_2 + (k_p_2^2) - k_p_1*k_p_2)) +
                exp(-k_p_2 * t) * (k_er * k_p_2*(k_er - k_p_1 - k_p_2)) +
                exp(-k_er * t) * (k_p_1 * (k_p_1* k_p_2 + (k_p_2^2) - k_er*k_p_1)))
  )
  
  return(premium * ((os_1 + os_2) * (1 - delta) + paid * delta))
}

n1 <- nls(loss_train ~ my.f(dev, premium, # nls = Nonlinear Least Squares
                            lk_er=lker, lk_p_1 = lkp1,lk_p_2 = lkp2, 
                            lRLR=lRLR, lRRF=lRRF, delta=delta),
          data=lossData0[cal<=max(accident_year)],
          start = c(lker = log(1.5), lRLR = log(1),
                    lkp1 = log(0.75),lkp2 = log(0.05), lRRF = log(0.75)))
n1


n1par <- data.table(summary(n1)$coefficients)[, exp(Estimate + 0.5*`Std. Error`^2)]
names(n1par) <- c("ker", "RLR", "kp1", "kp2", "RRF")
n1par

m1 <- nlme(loss_train ~ my.f(dev, premium,
                             lk_er=lker, lk_p_1=lkp1, lk_p_2 = lkp2, 
                             lRLR=lRLR, lRRF=lRRF, delta=delta),
           data=lossData0[cal<=max(accident_year)],
           fixed = lker + lRLR + lkp1 + lkp2 + lRRF ~ 1,
           random = pdDiag(lRLR + lRRF ~ 1),
           groups = ~ accident_year,
           weights = varIdent(form = ~ 1 | deltaf),
           start = c(lker = log(1.5), lRLR = log(1),
                     lkp1 = log(0.75),lkp2 = log(0.05), lRRF = log(0.75)),
           control=list(msMaxIter=10000, pnlsMaxIter=10000,pnlsTol=0.4)) 
summary(m1)



m1_fe <- data.table(summary(m1)$tTable)[, exp(Value + 0.5*`Std.Error`^2)]
names(m1_fe) <- c("ker", "RLR", "kp1","kp2", "RRF")
m1_fe


RLRRRF <- coef(m1)[,c(2,4)]
names(RLRRRF) <- c("RLR", "RRF")
round(exp(cbind(RLRRRF, ULR=apply(RLRRRF,1,sum))),3)

(sig_delta <- sapply(split(resid(m1), lossData0[cal<=max(accident_year)]$deltaf), sd))

VarCorr(m1)


############################ BAYES

curve(dgamma(x, 4, 5), from = 0, 5, main="Gamma priors",xlab="", ylab="", bty="n", col = 1) # RLR/RRF
curve(dgamma(x, 3, 2), add = TRUE, lty=3, col = 2) #k_er
curve(dgamma(x, 3, 4), add=TRUE, lty=5, col = 3) #k_p1
curve(dgamma(x, 5, 4), add=TRUE, lty=6, col = 4) #k_p2
text(x = c(3,3,3,3), y = c(1,0.8,0.6,0.4), labels = c( "RLR,RRF", "k_er", "k_p1", "k_p2"), col = c(1,2,3,4))


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


fml2t <- 
  loss_train ~ premium * (
    (((RLR * ker) / (ker - kp1 - kp2) * (exp(-(kp1 + kp2) * dev) - exp(-ker * dev))) +
       (((RLR * ker * kp2) / (kp1* (kp2 - ker) * (ker - kp1 - kp2))) * (
         exp(-(kp1 + kp2)*dev) * (ker - kp2) - exp(-kp2 * dev)*(ker - kp1 - kp2) - 
           exp(-ker * dev) * kp1))) * (1 - delta) + 
      ((RLR * RRF) / (kp1 * (kp2 - ker) * (ker - kp1 - kp2)) * 
         ((kp1 * (ker * (kp1 - ker) - kp2 * (kp1 + kp2)) + 2*ker*kp2) +
            exp(-(kp1 + kp2)*dev) * (ker * (ker * kp1 - ker*kp2 + (kp2^2) - kp1*kp2)) +
            exp(-kp2 * dev) * (ker * kp2*(ker - kp1 - kp2)) +
            exp(-ker * dev) * (kp1 * (kp1* kp2 + (kp2^2) - ker*kp1)))) * delta
  ) 

b2 <- brm(bf(fml2t,
             RLR ~ 1 + (1 | accident_year),
             RRF ~ 1 + (1 | accident_year),
             ker ~ 1,
             kp1 ~ 1,
             kp2 ~ 1,
             sigma ~ 0 + deltaf,
             nl = TRUE),
          data = lossData0[cal <= max(accident_year)], 
          family = brmsfamily("gaussian", link_sigma = "log"),
          prior = c(prior(gamma(4, 5), nlpar = "RLR", lb=0),
                    prior(gamma(4, 5), nlpar = "RRF", lb=0),
                    prior(gamma(3, 2), nlpar = "ker", lb=0),
                    prior(gamma(3, 4), nlpar = "kp1", lb=0),
                    prior(gamma(5, 4), nlpar = "kp2", lb=0)),
          control = list(adapt_delta = 0.999, max_treedepth=15), # 15
          seed = 1234, iter = 1000) # 1000

b2
summary(b2)
b2$fit

X <- extract(b2$fit)
(sig_delta_brm <- apply(X$b_sigma, 2, function(x) exp(mean(x)+0.5*var(x))))


plot(b2, N = 4, ask = FALSE)



RLR <- sweep(X$r_1_RLR_1, 1, X$b_RLR, "+")
RRF <- sweep(X$r_2_RRF_1, 1, X$b_RRF, "+")
ULR <- t(apply(RLR * RRF, 2, quantile, c(0.025, 0.5, 0.975)))
matplot(unique(lossData0$accident_year), ULR*100, 
        t="l", ylim=c(0, 150), lty=1, col=c(1,2,4), bty="n", 
        ylab="Projected ULR (%)", xlab="Accident year")
legend("topleft", legend = c("2.5%ile","50%ile", "97.5%ile"),
       lty=1, col=c(1,2,4), bty="n")
baseULR <- X$b_RLR * X$b_RRF
abline(h=quantile(baseULR, 0.025)*100, col=1, lty=3)
abline(h=median(baseULR)*100, col=2, lty=3)
abline(h=quantile(baseULR, 0.975)*100, col=4, lty=3)

###### Plot Functions

createPlotData <- function(stanfit, data, probs=c(0.25, 0.975)){
  
  ppc_loss <- as.matrix(extract(stanfit, "ppc_loss")$ppc_loss)
  ppc_loss_summary <- cbind(
    mean=apply(ppc_loss, 2, mean),
    t(apply(ppc_loss, 2, quantile, probs=probs))
  )
  colnames(ppc_loss_summary) = c(
    "Y_pred_mean",
    paste0("Y_pred_cred", gsub("\\.", "", probs[1])),
    paste0("Y_pred_cred", gsub("\\.", "", probs[2])))
  
  return(cbind(ppc_loss_summary, data))
}

plotDevBananas <- function(x, data, company_code,
                           xlab="Development year", 
                           ylab="Reported incurred loss ($k)", 
                           main="Correlated Log-normal Chain Ladder Model"){
  key <- list(
    rep=FALSE, 
    lines=list(col=c("#00526D", "#00526D", "purple"), 
               type=c("p", "p", "l"), 
               pch=c(19, 1, NA)),
    text=list(lab=c("Observation", "Hold out observation", "Mean estimate")),
    rectangles = list(col=adjustcolor("yellow", alpha.f=0.5), border="grey"),
    text=list(lab="95% Posterior prediction interval"))
  
  xyplot(x, data=data,as.table=TRUE,xlab=xlab, ylab=ylab, main=main,
         sub=paste("Data source: CAS Loss Reserving Database,",
                   "Comm. Auto, Comp.",
                   company_code),
         scales=list(alternating=1), layout=c(5,2), key=key,
         par.settings = list(strip.background=list(col="#CBDDE6")),
         par.strip.text = list(font = 2),
         panel=function(x, y){
           n <- length(x)
           divisor <- 5
           cn <- c(1:(n/divisor))
           upper <- y[cn+n/divisor*0]
           lower <- y[cn+n/divisor*1]
           x <- x[cn]
           panel.polygon(c(x, rev(x)), c(upper, rev(lower)),
                         col = adjustcolor("yellow", alpha.f = 0.5), 
                         border = "grey")
           panel.lines(x, y[cn+n/divisor*2], col="purple")
           panel.points(x, y[cn+n/divisor*4], lwd=1, col="#00526D")
           panel.points(x, y[cn+n/divisor*3], lwd=1, pch=19, col="#00526D")
         })
}

predClaimsPred <- predict(b2, newdata = lossData0, method="predict")
plotDevBananas(`Q2.5`/premium*100 + `Q97.5`/premium*100 + 
                 Estimate/premium*100 + loss_train/premium*100 + 
                 loss_test/premium*100 ~ 
                 dev | factor(accident_year), company_code = 337,
               data=cbind(lossData0, predClaimsPred)[delta==0], 
               main="Outstanding claims developments", 
               ylab="Outstanding loss ratio (%)")



plotDevBananas(`Q2.5`/premium*100 + `Q97.5`/premium*100 + 
                 Estimate/premium*100 + loss_train/premium*100 + 
                 loss_test/premium*100 ~  
                 dev | factor(accident_year),company_code = 337,
               data=cbind(lossData0, predClaimsPred)[delta==1], 
               main="Paid claims developments", 
               ylab="Paid loss ratio (%)")

# Calculation time
# > 4888/60
# [1] 81.46667 Minutes


####################
### TWO TANKS ODE NUMERICAL
####################


myFuns <- "
real[] ode_claimsprocess(real t, real [] y,  real [] theta, 
                         real [] x_r, int[] x_i){
  real dydt[4];
  
  dydt[1] = - theta[1] * y[1]; // Exposure
  dydt[2] = theta[1] * theta[2] * y[1] - (theta[3] + theta[5]) * y[2]; // OS1
  dydt[3] = theta[5] * (y[2] - y[3]); // OS2
  dydt[4] = theta[4] * (theta[3]*y[2] + theta[5]*y[3]); // Paid
  
  return dydt;
}
real claimsprocess(real t, real premium, real Ber, real kp1, 
                   real RLR, real RRF, real kp2, real delta){
  real y0[4];
  real y[1, 4];
  real theta[5];
  theta[1] = Ber;
  theta[2] = RLR;
  theta[3] = kp1;
  theta[4] = RRF;
  theta[5] = kp2;
  y0[1] = premium;
  y0[2] = 0;
  y0[3] = 0;
  y0[4] = 0;
  y = integrate_ode_rk45(ode_claimsprocess, 
                        y0, 0, rep_array(t, 1), theta,
                        rep_array(0.0, 0), rep_array(1, 1),
                        0.001, 0.001, 100); // tolerances, steps
      return (y[1, 3] * (1 - delta) + y[1, 4] * delta);
}
"

frml <- bf(loss_ratio ~ log(claimsprocess(dev, premium, Ber, kp1, RLR, RRF, kp2, delta)),
           RLR ~ 1 + (1 | p | accident_year), # 'p' allow for correlation with RRF 
           RRF ~ 1 + (1 | p | accident_year), # 'p' allow for correlation with RLR
           Ber ~ 1 + (1 | accident_year),
           kp1 ~ 1 + (1 | accident_year),
           kp2 ~1 + (1 | accident_year), 
           sigma ~ 0 + deltaf, # different sigma for OS and paid
           nl = TRUE)

## prior 1
mypriors <- c(prior(gamma(1, 1), nlpar = "RLR", lb=0),
              prior(gamma(1, 1), nlpar = "RRF", lb=0),
              prior(gamma(1, 1), nlpar = "Ber", lb=0),
              prior(gamma(1, 1), nlpar = "kp1", lb=0),
              prior(gamma(1, 1), nlpar = "kp2", lb=0),
              set_prior("lkj(2)", class = "cor"))

### prior 2
mypriors <- c(prior(normal(0, 1), nlpar = "RLR"),
              prior(normal(0, 1), nlpar = "RRF"),
              prior(normal(0, 1), nlpar = "Ber"),
              prior(normal(0, 1), nlpar = "kp1"),
              prior(normal(0, 1), nlpar = "kp2"),
              prior(student_t(10, 0, 0.1), class = "sd", nlpar = "RLR"), 
              prior(student_t(10, 0, 0.1), class = "sd", nlpar = "RRF"), 
              prior(student_t(10, 0, 0.1), class = "sd", nlpar = "Ber"), 
              prior(student_t(10, 0, 0.1), class = "sd", nlpar = "kp1"), 
              prior(student_t(10, 0, 0.1), class = "sd", nlpar = "kp2"))



b4 <- brm(
  frml, stanvars = stanvar(scode = myFuns, block = "functions"),
  data = dat_rect_long, backend = 'rstan',
  family = brmsfamily("lognormal", link_sigma = "log"),
  prior = mypriors, chains = 2,
  control = list(adapt_delta = 0.9, max_treedepth=7),
  seed = 1, iter = 100)

expose_functions(b4, vectorize = TRUE)

fit_loss_pred <- predict(b4, newdata = dat_rect_long, method="predict")
fit_loss_pred_all <- cbind(dat_rect_long, fit_loss_pred)

xyplot(Q2.5 + Q97.5 + loss_ratio + Estimate ~
         dev | factor(accident_year),
       data=fit_loss_pred_all, layout = c(4,2),
       ylab = "OS Loss Ratio",
       xlab = "Devolopment year",
       par.settings = list(strip.background=list(col="#CBDDE6")),
       scales=list(alternating=1),
       as.table=TRUE, type = "l", col = c("grey","grey",2,4), lwd = c(0.5 , 0.5, 2,2),
       main = "OS Loss Ratio"
)






