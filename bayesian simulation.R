rm(list=ls())



#Operating hours           #Date              Time

#Peak electricity      Dec 15-Dec 31      5am-9am (4hr) 
#                      Jan 01-Feb 28      5am-9am and 5pm-8pm (7hr) 
#                      Mar 01-Mar 15      5am-9am (4hr) 
#                      Jun 01-Sept 15     1pm-6pm (5hr)

#Sub-peak electricity  Dec 15-Dec 31      9am-8pm (11hr) 
#                      Jan 01-Feb 28      9am-5pm (8hr) 
#                      Mar 01-Mar 15      9am-8pm (11hr)
#                      Mar 15-May 31      6am-8pm (14hr) 
#                      Jun 01-Sept 15     8am-1pm (5hr)

#Base electricity      Dec 15-May 31      8pm-12am and 4am (5hr) 
#                      Jun 01-Sept 15     6pm-12am and 4am-8am (10hr)


############## ELECTRICITY PRICES ##############

set.seed(12345)

library(dplyr)

data10 <- read.csv("Ercot_10_12.csv")
data13 <- read.csv("Ercot_13_15.csv")
data16 <- read.csv("Ercot_16_18.csv")
data19 <- read.csv("Ercot_19_21.csv")

data19 <- filter(data19, Year<2021)

data <- rbind(data10, data13, data16, data19)
data$ID <- paste(data$Hour, data$Month, data$Day, data$Year)

data <- filter(data, Settlement_Point=="HB_WEST")
data$moy <- ifelse(data$Month==9, 0, 1)

anc_data <- read.csv("Ancilliary.csv")
anc_data$ID <- paste(anc_data$Hour, anc_data$Month, anc_data$Day, anc_data$Year)
data <- merge(x = data, y = anc_data[ , c("ID", "Total")], by = "ID")
data$price <- data$Settlement_Point_Price+data$Total

rm(data10, data13, data16, data19, anc_data)

#winter

data_dec <- filter(data, Month==12 & Day >= 15 & Day <= 31 & Hour >= 6 & Hour <= 20) #5am to 8pm
summary(data_dec)

data_janmay <- filter(data, Month>=1 & Month <= 5 & Hour >= 6 & Hour <= 20) #5am to 8pm
summary(data_janmay)

data_winter <- rbind(data_dec, data_janmay)
summary(data_winter)

data_winter %>% group_by(Hour)  %>%
  summarise(price_tot = mean(Settlement_Point_Price, na.rm=TRUE))

#winter peak

data_dec <- filter(data_dec, Month==12 & Day >= 15 & Day <= 31 & Hour >= 6 & Hour <= 9) #5am to 9am
summary(data_dec)

data_janfeb <- filter(data_janmay, Month>=1 & Month <= 2 & Hour >= 6 & Hour <= 9 | Month>=1 & Month <= 2 & 
                        Hour >= 18 & Hour <= 20) #5am to 9am and 5pm to 8pm
summary(data_janfeb)

data_mar <- filter(data_janmay, Month==3 & Day >= 1 & Day <= 15 & Hour >= 6 & Hour <= 9) #5am to 9am
summary(data_mar)

data_winter_peak <- rbind(data_dec, data_janfeb, data_mar)
summary(data_winter_peak)

data_winter_peak %>% group_by(Hour)  %>%
  summarise(price_tot = mean(Settlement_Point_Price, na.rm=TRUE))

#winter upper base

data_winter_ubase <- data_winter %>% filter(!ID %in% data_winter_peak$ID) #9am to 8pm

data_winter_ubase %>% group_by(Hour)  %>%
  summarise(price_tot = mean(Settlement_Point_Price, na.rm=TRUE))

#winter lower base

data_dec <- filter(data, Month==12 & Day >= 15 & Day <= 31 & Hour >= 21 & Hour <= 24 | Month==12 & Day >= 15 & Day <= 31 & Hour == 5) #8pm to 12am and 4am
summary(data_dec)

data_janmay <- filter(data, Month>=1 & Month <= 5 & Hour >= 21 & Hour <= 24 | Month>=1 & Month <= 5 & Hour == 5) #8pm to 12am and 4am
summary(data_janmay)

data_winter_lbase <- rbind(data_dec, data_janmay)
summary(data_winter_lbase)

data_winter_lbase %>% group_by(Hour)  %>%
  summarise(price_tot = mean(Settlement_Point_Price, na.rm=TRUE))

#summer

data_junaug <- filter(data, Month>=6 & Month<=8 & Hour >= 9 & Hour <= 18) #8am to 6pm
summary(data_junaug)

data_sep <- filter(data, Month==9 & Day <= 15 & Hour >= 9 & Hour <= 18) #8am to 6pm
summary(data_sep)

data_summer <- rbind(data_junaug, data_sep)
summary(data_summer)

data_summer %>% group_by(Hour)  %>%
  summarise(price_tot = mean(Settlement_Point_Price, na.rm=TRUE))

#summer peak

data_junaug <- filter(data_junaug, Month>=6 & Month<=8 & Hour >= 14 & Hour <= 18) #1pm to 6pm
summary(data_junaug)

data_sep <- filter(data_sep, Month==9 & Day <= 15 & Hour >= 14 & Hour <= 18) #1pm to 6pm
summary(data_sep)

data_summer_peak <- rbind(data_junaug, data_sep)
summary(data_summer_peak)

data_summer_peak %>% group_by(Hour)  %>%
  summarise(price_tot = mean(Settlement_Point_Price, na.rm=TRUE))

#summer upper base

data_summer_ubase <- data_summer %>% filter(!ID %in% data_summer_peak$ID) #8am to 1pm

data_summer_ubase %>% group_by(Hour)  %>%
  summarise(price_tot = mean(Settlement_Point_Price, na.rm=TRUE))

#summer lower base

data_junaug <- filter(data, Month>=6 & Month<=8 & Hour >= 19 & Hour <= 24 | Month>=6 & Month<=8 & Hour >= 5 & Hour <= 8) #6pm to 12am and 4am to 8am
summary(data_junaug)

data_sep <- filter(data, Month==9 & Day <= 15 & Hour >= 19 & Hour <= 24 | Month==9 & Day <= 15 & Hour >= 5 & Hour <= 8) #6pm to 12am and 4am to 8am
summary(data_sep)

data_summer_lbase <- rbind(data_junaug, data_sep)
summary(data_summer_lbase)

data_summer_lbase %>% group_by(Hour)  %>%
  summarise(price_tot = mean(Settlement_Point_Price, na.rm=TRUE))

rm(data_dec, data_janfeb, data_janmay, data_junaug, data_mar, data_sep)

############## TEMPERATURE ##############

#source: https://mesonet.agron.iastate.edu/request/download.phtml?network=TX_ASOS#
#ASOS-AWOS-METAR Data Download

temp <- read.csv("Temperature.csv")

temp$Hour <- ifelse(temp$Hour==0,24, temp$Hour)

temp$ID <- paste(temp$Hour, temp$Month, temp$Day, temp$Year)
temp <- filter(temp, tmpf!="M")
temp$tmpf <- as.numeric(temp$tmpf)

temp %>% group_by(ID, Hour)  %>%
  summarise(temp = mean(tmpf, na.rm=TRUE),
            Month = mean(Month, na.rm=TRUE),
            Day = mean(Day, na.rm=TRUE),
            Year = mean(Year, na.rm=TRUE),
            Hour = mean(Hour, na.rm=TRUE),
            dow = mean(dow, na.rm=TRUE),
            weekday = mean(weekday, na.rm=TRUE)) -> temp

#merge

data_winter_peak <- merge(x = data_winter_peak, y = temp[ , c("ID", "temp")], by = "ID")
data_winter_ubase <- merge(x = data_winter_ubase, y = temp[ , c("ID", "temp")], by = "ID")
data_winter_lbase <- merge(x = data_winter_lbase, y = temp[ , c("ID", "temp")], by = "ID")
data_summer_peak <- merge(x = data_summer_peak, y = temp[ , c("ID", "temp")], by = "ID")
data_summer_ubase <- merge(x = data_summer_ubase, y = temp[ , c("ID", "temp")], by = "ID")
data_summer_lbase <- merge(x = data_summer_lbase, y = temp[ , c("ID", "temp")], by = "ID")

peak <- rbind(data_winter_peak,data_summer_peak)
ubase <- rbind(data_winter_ubase,data_summer_ubase)
lbase <- rbind(data_winter_lbase,data_summer_lbase)

#check if all months exist

unique(peak$Month)
unique(ubase$Month)
unique(lbase$Month)

#getting month-wise prices and temperature

peak_jan <- subset(peak, Month==1)
peak_feb <- subset(peak, Month==2)
peak_mar <- subset(peak, Month==3)
peak_jun <- subset(peak, Month==6)
peak_jul <- subset(peak, Month==7)
peak_aug <- subset(peak, Month==8)
peak_sep <- subset(peak, Month==9)
peak_dec <- subset(peak, Month==12)

ubase_jan <- subset(ubase, Month==1)
ubase_feb <- subset(ubase, Month==2)
ubase_mar <- subset(ubase, Month==3)
ubase_apr <- subset(ubase, Month==4)
ubase_may <- subset(ubase, Month==5)
ubase_jun <- subset(ubase, Month==6)
ubase_jul <- subset(ubase, Month==7)
ubase_aug <- subset(ubase, Month==8)
ubase_sep <- subset(ubase, Month==9)
ubase_dec <- subset(ubase, Month==12)

lbase_jan <- subset(lbase, Month==1)
lbase_feb <- subset(lbase, Month==2)
lbase_mar <- subset(lbase, Month==3)
lbase_apr <- subset(lbase, Month==4)
lbase_may <- subset(lbase, Month==5)
lbase_jun <- subset(lbase, Month==6)
lbase_jul <- subset(lbase, Month==7)
lbase_aug <- subset(lbase, Month==8)
lbase_sep <- subset(lbase, Month==9)
lbase_dec <- subset(lbase, Month==12)

summary(lm(price ~ temp+I(temp*temp)+weekday, data=peak_feb))

############## BAYESIAN SIMULATION ##############

bayesian_elec <- function(df) {
  
  k=4
  n=nrow(df)
  x1=df[, "temp"]
  x2=df[, "temp"]*df[, "temp"]
  x3=df[, "weekday"]
  y=df[, "price"]
  x0=rep(1, each=n)
  X=cbind(x0,x1,x2,x3)
  beta <- solve(crossprod(X)) %*% crossprod(X, y)
  
  sigma=0.5
  e=rnorm(n)*sigma
  
  #select prior values
  
  v0=1
  s02=0.01 #0.001
  prior_betamean=beta
  prior_beta=n*diag(k)
  prior_betacov=solve(prior_beta)
  
  #start gibbs sampling and setup initial storage and number of draws
  
  iter=10050
  burn=50
  betas_final=matrix(0, nrow=iter-burn, ncol=k)
  beta=matrix(0,nrow=k, ncol=1)
  sigma_final=matrix(0, nrow=iter-burn, ncol=1)
  log_like=matrix(0, nrow=iter, ncol=1)
  
  #start sampling
  
  for (i in 1:iter){
    
    e=y-X%*%beta
    d1=t(e)%*%e + v0*s02
    chi=rchisq(1,n+v0)
    sigma=sqrt(d1/chi)
    
    #draw for beta
    betacov=solve((t(X)%*%X)*as.numeric(sigma^-2)+prior_betacov)
    betamean=betacov%*%(t(X)%*%y*as.numeric(sigma^-2) + prior_betacov%*%prior_betamean)
    
    znorm=rnorm(k)
    beta=betamean+t(chol(betacov))%*%znorm
    
    if(i>burn){
      betas_final[i-burn,]=t(beta)
      sigma_final[i-burn,]=sigma
    }
  }
  
  mean_sigma=mean(sigma_final)
  mean_beta=colMeans(betas_final)
  
  Y <- matrix(0,nrow=iter-burn,ncol=1)
  
  list_estimate<-list()
  
  for (i in 1:nrow(X)){
    
    Y <- t(t(as.matrix(X[i,]))%*%t(betas_final)+e[i])
    
    list_estimate[[i]] <- Y
  }
  
  Y <- matrix(0,nrow=iter-burn,ncol=1)
  
  set.seed(123)
  
  for (i in 1:nrow(Y)){
    
    list_num <- sample(1:length(list_estimate), 1)
    Y[i] <- list_estimate[[list_num]][sample(nrow(list_estimate[[list_num]]),size=1,replace=FALSE),]
  }
  
  return(Y)
  
}

df <- lbase_dec #specify month

#simple bootstrap for comparison
lbase_dec_boot <- sample(df[,"price"], 10000, replace=TRUE) #rename

#bayesian simulation
price <- bayesian_elec(df=df)

plot(ecdf(x = price), main = "Empirical cumulative distribution function (base prices)")
lines(ecdf(x = df[,"price"]), col = 2)
plot(density(price,500), lwd = 2, main = "Kernel density plot (base prices)")
lines(density(df[,"price"],500), col=2)
lines(density(lbase_dec_boot, 500), col=3)

summary(price); summary(df[,"price"]); summary(lbase_dec_boot) #adjust name

dec_lbase <- price #adjust name

peak_price <- data.frame(jan_peak=jan_peak[,1], jan_boot=peak_jan_boot, feb_peak=feb_peak[,1], feb_boot=peak_feb_boot,
                         mar_peak=mar_peak[,1], mar_boot=peak_mar_boot, jun_peak=jun_peak[,1], jun_boot=peak_jun_boot,
                         jul_peak=jul_peak[,1], jul_boot=peak_jul_boot, aug_peak=aug_peak[,1], aug_boot=peak_aug_boot,
                         sep_peak=sep_peak[,1], sep_boot=peak_sep_boot, dec_peak=dec_peak[,1], dec_boot=peak_dec_boot)

write.csv(peak_price, file="peak_price.csv")

ubase_price <- data.frame(jan_ubase=jan_ubase[,1], jan_boot=ubase_jan_boot, feb_ubase=feb_ubase[,1], feb_boot=ubase_feb_boot,
                          mar_ubase=mar_ubase[,1], mar_boot=ubase_mar_boot, apr_ubase=apr_ubase[,1], apr_boot=ubase_apr_boot,
                          may_ubase=may_ubase[,1], may_boot=ubase_may_boot, jun_ubase=jun_ubase[,1], jun_boot=ubase_jun_boot,
                          jul_ubase=jul_ubase[,1], jul_boot=ubase_jul_boot, aug_ubase=aug_ubase[,1], aug_boot=ubase_aug_boot,
                          sep_ubase=sep_ubase[,1], sep_boot=ubase_sep_boot, dec_ubase=dec_ubase[,1], dec_boot=ubase_dec_boot)

write.csv(ubase_price, file="ubase_price.csv")

lbase_price <- data.frame(jan_lbase=jan_lbase[,1], jan_boot=lbase_jan_boot, feb_lbase=feb_lbase[,1], feb_boot=lbase_feb_boot,
                          mar_lbase=mar_lbase[,1], mar_boot=lbase_mar_boot, apr_lbase=apr_lbase[,1], apr_boot=lbase_apr_boot,
                          may_lbase=may_lbase[,1], may_boot=lbase_may_boot, jun_lbase=jun_lbase[,1], jun_boot=lbase_jun_boot,
                          jul_lbase=jul_lbase[,1], jul_boot=lbase_jul_boot, aug_lbase=aug_lbase[,1], aug_boot=lbase_aug_boot,
                          sep_lbase=sep_lbase[,1], sep_boot=lbase_sep_boot, dec_lbase=dec_lbase[,1], dec_boot=lbase_dec_boot)

write.csv(lbase_price, file="lbase_price.csv")

############## CROPS AND FERTILIZER PRICE FOR AMMONIA SIMULATION ##############

fert <- read.csv("Fertilizer prices.csv")
fert$ID <- paste(fert$Month, fert$Year)

barley <- read.csv("Barley.csv")
barley$ID <- paste(barley$Month, barley$Year)

corn <- read.csv("Corn.csv")
corn$ID <- paste(corn$Month, corn$Year)
corn %>% group_by(ID)  %>%
  summarise(Corn = mean(Corn, na.rm=TRUE),
            Corn_sq = mean(Corn_sq, na.rm=TRUE),
            Month = mean(Month, na.rm=TRUE),
            Year = mean(Year, na.rm=TRUE)) -> corn

cotton <- read.csv("Cotton.csv")
cotton$ID <- paste(cotton$Month, cotton$Year)
cotton %>% group_by(ID)  %>%
  summarise(Cotton = mean(Cotton, na.rm=TRUE),
            Cotton_sq = mean(Cotton_sq, na.rm=TRUE),
            Month = mean(Month, na.rm=TRUE),
            Year = mean(Year, na.rm=TRUE)) -> cotton

oats <- read.csv("Oats.csv")
oats$ID <- paste(oats$Month, oats$Year)
oats %>% group_by(ID)  %>%
  summarise(Oats = mean(Oats, na.rm=TRUE),
            Oats_sq = mean(Oats_sq, na.rm=TRUE),
            Month = mean(Month, na.rm=TRUE),
            Year = mean(Year, na.rm=TRUE)) -> oats

soy <- read.csv("Soybean.csv")
soy$ID <- paste(soy$Month, soy$Year)
soy %>% group_by(ID)  %>%
  summarise(Soybean = mean(Soybean, na.rm=TRUE),
            Soybean_sq = mean(Soybean_sq, na.rm=TRUE),
            Month = mean(Month, na.rm=TRUE),
            Year = mean(Year, na.rm=TRUE)) -> soy
soy <- soy[-c(137),]

wheat <- read.csv("Wheat.csv")
wheat$ID <- paste(wheat$Month, wheat$Year)
wheat %>% group_by(ID)  %>%
  summarise(Wheat = mean(Wheat, na.rm=TRUE),
            Wheat_sq = mean(Wheat_sq, na.rm=TRUE),
            Month = mean(Month, na.rm=TRUE),
            Year = mean(Year, na.rm=TRUE)) -> wheat

wti <- read.csv("WTI.csv")
wti$ID <- paste(wti$Month, wti$Year)
wti %>% group_by(ID)  %>%
  summarise(WTI = mean(WTI, na.rm=TRUE),
            WTI_sq = mean(WTI_sq, na.rm=TRUE),
            Month = mean(Month, na.rm=TRUE),
            Year = mean(Year, na.rm=TRUE)) -> wti

crop <- merge(x = fert, y = barley[ , c("ID", "Barley", "Barley_sq")], by = "ID")
crop <- merge(x = crop, y = corn[ , c("ID", "Corn", "Corn_sq")], by = "ID")
crop <- merge(x = crop, y = cotton[ , c("ID", "Cotton", "Cotton_sq")], by = "ID")
crop <- merge(x = crop, y = oats[ , c("ID", "Oats", "Oats_sq")], by = "ID")
crop <- merge(x = crop, y = soy[ , c("ID", "Soybean", "Soybean_sq")], by = "ID")
crop <- merge(x = crop, y = wheat[ , c("ID", "Wheat", "Wheat_sq")], by = "ID")
crop <- merge(x = crop, y = wti[ , c("ID", "WTI", "WTI_sq")], by = "ID")

crop <- arrange(crop,Year,Month)

#season-wise ammonia and crop prices
crop_winter <- subset(crop, Month==12 | Month==1 | Month==2)
crop_summer <- subset(crop, Month==3 | Month==4 | Month==5)
crop_roy <- subset(crop, Month==6 | Month==7 | Month==8 | Month==9)

summary(lm(ANHYD ~ Corn + Corn_sq + Cotton + Cotton_sq + Oats + Oats_sq + 
             lag(WTI, 9), data=crop))

############## BAYESIAN SIMULATION ##############

bayesian_amm <- function(df){
  
  n=nrow(df)
  k=8
  x1=df[, "Corn"]
  x2=df[, "Corn_sq"]
  x3=df[, "Cotton"]
  x4=df[, "Cotton_sq"]
  x5=df[, "Oats"]
  x6=df[, "Oats_sq"]
  x7=lag(df[, "WTI"],9)
  y=df[, "ANHYD"]
  x0=rep(1, each=n)
  X=cbind(x0,x1,x2,x3,x4,x5,x6,x7)
  
  X <- X[-c(1:9),]
  y <- y[-c(1:9)]
  
  beta <- solve(crossprod(X)) %*% crossprod(X, y)
  
  sigma=0.5
  e=rnorm(n)*sigma
  
  #select prior values
  
  v0=1
  s02=0.01 #0.001
  prior_betamean=beta
  prior_beta=n*diag(k)
  prior_betacov=solve(prior_beta)
  
  #start gibbs sampling and setup initial storage and number of draws
  
  iter=10050
  burn=50
  betas_final=matrix(0, nrow=iter-burn, ncol=k)
  beta=matrix(0,nrow=k, ncol=1)
  sigma_final=matrix(0, nrow=iter-burn, ncol=1)
  log_like=matrix(0, nrow=iter, ncol=1)
  n=length(y)
  
  #start sampling
  
  for (i in 1:iter){
    
    e=y-X%*%beta
    d1=t(e)%*%e + v0*s02
    chi=rchisq(1,n+v0)
    sigma=sqrt(d1/chi)
    
    #draw for beta
    betacov=solve((t(X)%*%X)*as.numeric(sigma^-2)+prior_betacov)
    betamean=betacov%*%(t(X)%*%y*as.numeric(sigma^-2) + prior_betacov%*%prior_betamean)
    
    znorm=rnorm(k)
    beta=betamean+t(chol(betacov))%*%znorm
    
    if(i>burn){
      betas_final[i-burn,]=t(beta)
      sigma_final[i-burn,]=sigma
    }
  }
  
  mean_sigma=mean(sigma_final)
  mean_beta=colMeans(betas_final)
  
  Y <- matrix(0,nrow=10000,ncol=1)
  
  list_estimate<-list()
  
  for (i in 1:nrow(X)){
    
    Y <- t(t(as.matrix(X[i,]))%*%t(betas_final)+e[i])
    
    list_estimate[[i]] <- Y
  }
  
  Y <- matrix(0,nrow=10000,ncol=1)
  
  set.seed(123)
  
  for (i in 1:nrow(Y)){
    list_num <- sample(1:length(list_estimate), 1)
    Y[i] <- list_estimate[[list_num]][sample(nrow(list_estimate[[list_num]]),size=1,replace=FALSE),]
  }
  
  return(Y)
  
}

df <- crop_roy #specify season

#simple bootstrap for comparison
crop_roy_boot <- sample(df[,"ANHYD"], 10000, replace=TRUE) #rename

#bayesian simulation
price <- bayesian_amm(df=df)

plot(ecdf(x = price), main = "Empirical cumulative distribution function (base prices)")
lines(ecdf(x = df[,"ANHYD"]), col = 2)

plot(density(price,500), lwd = 2, main = "Kernel density plot (base prices)")
lines(density(df[,"ANHYD"],500), col=2)
lines(density(crop_roy_boot, 500), col=3)

summary(price); summary(df[,"ANHYD"]); summary(crop_roy_boot) #adjust name

roy_crop <- price #adjust name

amm_price <- data.frame(winter_crop=winter_crop[,1], winter_boot=crop_winter_boot,
                        summer_crop=summer_crop[,1], summer_boot=crop_summer_boot,
                        roy_crop=roy_crop[,1], roy_boot=crop_roy_boot)

write.csv(amm_price, file="amm_price.csv")

############## GIN TRASH BAYESIAN SIMULATION ##############

gin <- read.csv("Ropes Coop Gin Trash 2004-2018.csv")

summary(lm(gin~ppt+I(ppt*ppt), data=gin)) 

n=nrow(gin)
k=3
x1=gin$ppt
x2=gin$ppt*gin$ppt
y=(gin$gin)
x0=rep(1, each=n)
X=cbind(x0,x1,x2)
beta <- solve(crossprod(X)) %*% crossprod(X, y)

sigma=0.5
e=rnorm(n)*sigma

#select prior values

v0=1
s02=0.01 #0.001
prior_betamean=beta
prior_beta=n*diag(k)
prior_betacov=solve(prior_beta)

#start gibbs sampling and setup initial storage and number of draws

iter=10050
burn=50
betas_final=matrix(0, nrow=iter-burn, ncol=k)
beta=matrix(0,nrow=k, ncol=1)
sigma_final=matrix(0, nrow=iter-burn, ncol=1)
log_like=matrix(0, nrow=iter, ncol=1)
n=length(y)

#start sampling

for (i in 1:iter){
  
  e=y-X%*%beta
  d1=t(e)%*%e + v0*s02
  chi=rchisq(1,n+v0)
  sigma=sqrt(d1/chi)
  
  #draw for beta
  betacov=solve((t(X)%*%X)*as.numeric(sigma^-2)+prior_betacov)
  betamean=betacov%*%(t(X)%*%y*as.numeric(sigma^-2) + prior_betacov%*%prior_betamean)
  
  znorm=rnorm(k)
  beta=betamean+t(chol(betacov))%*%znorm
  
  if(i>burn){
    
    betas_final[i-burn,]=t(beta)
    sigma_final[i-burn,]=sigma
  }
}

mean_sigma=mean(sigma_final)
mean_beta=colMeans(betas_final)

Y <- matrix(0,nrow=10000,ncol=1)

list_estimate<-list()

for (i in 1:nrow(X)){
  
  Y <- t(t(as.matrix(X[i,]))%*%t(betas_final)+e[i])
  list_estimate[[i]] <- Y
  
}

set.seed(123)

Y1 <- list_estimate[[1]][sample(nrow(list_estimate[[1]]),size=667,replace=FALSE),]
Y2 <- list_estimate[[2]][sample(nrow(list_estimate[[2]]),size=667,replace=FALSE),]
Y3 <- list_estimate[[3]][sample(nrow(list_estimate[[3]]),size=666,replace=FALSE),]
Y4 <- list_estimate[[4]][sample(nrow(list_estimate[[4]]),size=667,replace=FALSE),]
Y5 <- list_estimate[[5]][sample(nrow(list_estimate[[5]]),size=667,replace=FALSE),]
Y6 <- list_estimate[[6]][sample(nrow(list_estimate[[6]]),size=666,replace=FALSE),]
Y7 <- list_estimate[[7]][sample(nrow(list_estimate[[7]]),size=667,replace=FALSE),]
Y8 <- list_estimate[[8]][sample(nrow(list_estimate[[8]]),size=667,replace=FALSE),]
Y9 <- list_estimate[[9]][sample(nrow(list_estimate[[9]]),size=666,replace=FALSE),]
Y10 <- list_estimate[[10]][sample(nrow(list_estimate[[10]]),size=667,replace=FALSE),]
Y11 <- list_estimate[[11]][sample(nrow(list_estimate[[11]]),size=667,replace=FALSE),]
Y12 <- list_estimate[[12]][sample(nrow(list_estimate[[12]]),size=666,replace=FALSE),]
Y13 <- list_estimate[[13]][sample(nrow(list_estimate[[13]]),size=667,replace=FALSE),]
Y14 <- list_estimate[[14]][sample(nrow(list_estimate[[14]]),size=667,replace=FALSE),]
Y15 <- list_estimate[[15]][sample(nrow(list_estimate[[15]]),size=666,replace=FALSE),]

Y <- c(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,Y12,Y13,Y14,Y15)

#simple bootstrap for comparison
gin_boot <- sample(gin$gin, 10000, replace=TRUE)

summary(Y); summary(gin$gin); summary(gin_boot)

gin_quant <- data.frame(gin=Y, gin_boot=gin_boot)

write.csv(gin_quant, file="gin_quant.csv")

plot(density(Y), lwd = 2, main = "Default kernel density plot")
lines(density((gin$gin)))
lines(density(gin_boot), col="red")

plot(ecdf(x = Y), main = "ECDF of x and y")
lines(ecdf(x = gin$gin/9), col = 2)
lines(density(crop_roy_boot, 500), col=3)


