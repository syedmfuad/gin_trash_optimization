#summary: monthly optimization of gin trash to generate electricity and/or ammonia

#I use the lp function from lpSolve package for optimization
#There is another package called linprog (same arguments as linprog from matlab)
#but lpSolve is written in C which is a compiled language as opposed to R, which is an interpreted language
#One consequence of this is that lpsolve is much faster than linprog
#More details: http://www.noamross.net/archives/2014-04-16-vectorization-in-r-why/

#The optimization here is a monthly iteration with annual gin trash data (9 iteration in a year)
#So each iteration is dependent on the results of previous iterations
#I also have 10,000 rows of data, meaning each sequence of a full iteration is run 10,000 times, which I do using a loop
#I can use a nested loop for the iterations; but, the conditions in each iteration (obj function, constraints) are different
#So writing a nested loop may make the codes cleaner but at the expense of my readability

rm(list=ls())

data <- read.csv("gin.csv")

#sensitivity analysis: 
#(i) change eff=2/3; 
#(ii) reduce lower base to 25; 
#(iii) reduce ammonia marginal cost to 85 (change 17.35 to 13.23); 
#(iv) change extra_fc=75000;

n=nrow(data)
U <- data$gin_small

c=5 #electricity plant capacity
m=2 #no. of ammonia plant

#only for sensitivity analysis
eff=1
extra_fc=0

XC=matrix(NA, nrow=n, ncol=55)

XC <- assign(paste("XC", c, m, sep = ""), matrix(NA, nrow=n, ncol=55))

#profvis
system.time(
  for (i in seq_along(XC[,1])){
    
    f.con <- matrix (c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), nrow = 6, byrow = TRUE)
    f.dir <- c("<=", "<=", "<=", "<=", "<=", "<=")
    
    f.con_sum <- matrix (c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 5, byrow = TRUE)
    f.dir_sum <- c("<=", "<=", "<=", "<=", "<=")
    
    #for december
    
    f.obj <- c((data$dec_peak[i]-5.5), (data$dec_ubase[i]-5.5), (data$dec_lbase[i]-5.5), ((data$winter_crop[i]/11-17.35)))
    f.rhs <- c(340*c*eff, U[i]*eff, 68*c*eff, 187*c*eff, 85*c*eff, 369*m*eff)
    
    results_dec <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_dec$solution[1]+results_dec$solution[2]+results_dec$solution[3]+results_dec$solution[4])*1/eff
    
    XC[i,1] <- results_dec$solution[1]
    XC[i,12] <- results_dec$solution[2]
    XC[i,23] <- results_dec$solution[3]
    XC[i,34] <- results_dec$solution[4]
    XC[i,45] <- as.numeric(format(round(U[i]-total, 3), nsmall=3))
    gin <- XC[i,45]
    
    #for january
    
    f.obj <- c((data$jan_peak[i]-5.5), (data$jan_ubase[i]-5.5), (data$jan_lbase[i]-5.5), ((data$winter_crop[i]/11-17.35)))
    f.rhs <- c(620*c*eff, gin*eff, 217*c*eff, 248*c*eff, 155*c*eff, 672*m*eff)
    
    results_jan <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_jan$solution[1]+results_jan$solution[2]+results_jan$solution[3]+results_jan$solution[4])*1/eff
    
    XC[i,2] <- results_jan$solution[1]
    XC[i,13] <- results_jan$solution[2]
    XC[i,24] <- results_jan$solution[3]
    XC[i,35] <- results_jan$solution[4]
    XC[i,46] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,46]
    
    #for february
    
    f.obj <- c((data$feb_peak[i]-5.5), (data$feb_ubase[i]-5.5), (data$feb_lbase[i]-5.5), ((data$winter_crop[i]/11-17.35)))
    f.rhs <- c(560*c*eff, gin*eff, 196*c*eff, 224*c*eff, 140*c*eff, 640*m*eff)
    
    results_feb <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_feb$solution[1]+results_feb$solution[2]+results_feb$solution[3]+results_feb$solution[4])*1/eff
    
    XC[i,3] <- results_feb$solution[1]
    XC[i,14] <- results_feb$solution[2]
    XC[i,25] <- results_feb$solution[3]
    XC[i,36] <- results_feb$solution[4]
    XC[i,47] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,47]
    
    #march divided into two parts: first part has peak electricity and second part does not
    #for march first half with peak electricity 
    
    f.obj <- c((data$mar_peak[i]-5.5), (data$mar_ubase[i]-5.5), (data$mar_lbase[i]-5.5), ((data$summer_crop[i]/11-17.35)))
    f.rhs <- c(300*c*eff, gin*eff, 60*c*eff, 165*c*eff, 75*c*eff, 325*m*eff)
    
    results_mar1 <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_mar1$solution[1]+results_mar1$solution[2]+results_mar1$solution[3]+results_mar1$solution[4])*1/eff
    
    XC[i,4] <- results_mar1$solution[1]
    XC[i,15] <- results_mar1$solution[2]
    XC[i,26] <- results_mar1$solution[3]
    XC[i,37] <- results_mar1$solution[4]
    XC[i,48] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,48]
    
    #for march second half without peak electricity
    
    f.obj <- c((data$mar_ubase[i]-5.5), (data$mar_lbase[i]-5.5), ((data$summer_crop[i]/11-17.35)))
    f.rhs <- c(304*c*eff, gin*eff, 224*c*eff, 80*c*eff, 347*m*eff)
    
    results_mar2 <- lp("max", f.obj, f.con_sum, f.dir_sum, f.rhs)
    
    total <- as.numeric(results_mar2$solution[1]+results_mar2$solution[2]+results_mar2$solution[3])*1/eff
    
    XC[i,5] <- 0
    XC[i,16] <- results_mar2$solution[1]
    XC[i,27] <- results_mar2$solution[2]
    XC[i,38] <- results_mar2$solution[3]
    XC[i,49] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,49]
    
    #for april
    
    f.obj <- c((data$apr_ubase[i]-5.5), (data$apr_lbase[i]-5.5), ((data$summer_crop[i]/11-17.35)))
    f.rhs <- c(570*c*eff, gin*eff, 420*c*eff, 150*c*eff, 672*m*eff)
    
    results_apr <- lp("max", f.obj, f.con_sum, f.dir_sum, f.rhs)
    
    total <- as.numeric(results_apr$solution[1]+results_apr$solution[2]+results_apr$solution[3])*1/eff
    
    XC[i,6] <- 0
    XC[i,17] <- results_apr$solution[1]
    XC[i,28] <- results_apr$solution[2]
    XC[i,39] <- results_apr$solution[3]
    XC[i,50] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,50]
    
    #for may
    
    f.obj <- c((data$may_ubase[i]-5.5), (data$may_lbase[i]-5.5), ((data$summer_crop[i]/11-17.35)))
    f.rhs <- c(589*c*eff, gin*eff, 434*c*eff, 155*c*eff, 672*m*eff)
    
    results_may <- lp("max", f.obj, f.con_sum, f.dir_sum, f.rhs)
    
    total <- as.numeric(results_may$solution[1]+results_may$solution[2]+results_may$solution[3])*1/eff
    
    XC[i,7] <- 0
    XC[i,18] <- results_may$solution[1]
    XC[i,29] <- results_may$solution[2]
    XC[i,40] <- results_may$solution[3]
    XC[i,51] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,51]
    
    #for june
    
    f.obj <- c((data$jun_peak[i]-5.5), (data$jun_ubase[i]-5.5), (data$jun_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35)))
    f.rhs <- c(600*c*eff, gin*eff, 150*c*eff, 150*c*eff, 300*c*eff, 672*m*eff)
    
    results_jun <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_jun$solution[1]+results_jun$solution[2]+results_jun$solution[3]+results_jun$solution[4])*1/eff
    
    XC[i,8] <- results_jun$solution[1]
    XC[i,19] <- results_jun$solution[2]
    XC[i,30] <- results_jun$solution[3]
    XC[i,41] <- results_jun$solution[4]
    XC[i,52] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,52]
    
    #for july
    
    f.obj <- c((data$jul_peak[i]-5.5), (data$jul_ubase[i]-5.5), (data$jul_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35)))
    f.rhs <- c(620*c*eff, gin*eff, 155*c*eff, 155*c*eff, 310*c*eff, 672*m*eff)
    
    results_jul <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_jul$solution[1]+results_jul$solution[2]+results_jul$solution[3]+results_jul$solution[4])*1/eff
    
    XC[i,9] <- results_jul$solution[1]
    XC[i,20] <- results_jul$solution[2]
    XC[i,31] <- results_jul$solution[3]
    XC[i,42] <- results_jul$solution[4]
    XC[i,53] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,53]
    
    #for august
    
    f.obj <- c((data$aug_peak[i]-5.5), (data$aug_ubase[i]-5.5), (data$aug_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35)))
    f.rhs <- c(610*c*eff, gin*eff, 150*c*eff, 150*c*eff, 300*c*eff, 672*m*eff)
    
    results_aug <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_aug$solution[1]+results_aug$solution[2]+results_aug$solution[3]+results_aug$solution[4])*1/eff
    
    XC[i,10] <- results_aug$solution[1]
    XC[i,21] <- results_aug$solution[2]
    XC[i,32] <- results_aug$solution[3]
    XC[i,43] <- results_aug$solution[4]
    XC[i,54] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,54]
    
    #for september
    
    f.obj <- c((data$sep_peak[i]-5.5), (data$sep_ubase[i]-5.5), (data$sep_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35)))
    f.rhs <- c(300*c*eff, gin*eff, 75*c*eff, 75*c*eff, 150*c*eff, 336*m*eff)
    
    results_sep <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_sep$solution[1]+results_sep$solution[2]+results_sep$solution[3]+results_sep$solution[4])*1/eff
    
    XC[i,11] <- results_sep$solution[1]
    XC[i,22] <- results_sep$solution[2]
    XC[i,33] <- results_sep$solution[3]
    XC[i,44] <- results_sep$solution[4]
    XC[i,55] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,55]
    
  }
)

XC <- as.data.frame(XC)

summary(XC)

XC$extra_profit <- XC$V55*10

XC$V48 <- XC$V48-XC$V55
XC$V49 <- XC$V49-XC$V55
XC$V50 <- XC$V50-XC$V55
XC$V51 <- XC$V51-XC$V55
XC$V52 <- XC$V52-XC$V55
XC$V53 <- XC$V53-XC$V55
XC$V54 <- XC$V54-XC$V55
XC$V55 <- XC$V55-XC$V55

XC$cost <- XC$V45+XC$V46+XC$V47+XC$V48+XC$V49+XC$V50+XC$V51+XC$V52+XC$V53+XC$V54+XC$V55

price_mat <- matrix(c(c(data$dec_peak-5.5), c(data$jan_peak-5.5), c(data$feb_peak-5.5), c(data$mar_peak-5.5), c(rep(0, n)), 
                      c(rep(0, n)), c(rep(0, n)), c(data$jun_peak-5.5), c(data$jul_peak-5.5), c(data$aug_peak-5.5), c(data$sep_peak-5.5), 
                      
                      c(data$dec_ubase-5.5), c(data$jan_ubase-5.5), c(data$feb_ubase-5.5), c(data$mar_ubase-5.5), c(data$mar_ubase-5.5),
                      c(data$apr_ubase-5.5), c(data$may_ubase-5.5), c(data$jun_ubase-5.5), c(data$jul_ubase-5.5), c(data$aug_ubase-5.5), 
                      c(data$sep_ubase-5.5),
                      
                      c(data$dec_lbase-5.5), c(data$jan_lbase-5.5), c(data$feb_lbase-5.5), c(data$mar_lbase-5.5), c(data$mar_lbase-5.5),
                      c(data$apr_lbase-5.5), c(data$may_lbase-5.5), c(data$jun_lbase-5.5), c(data$jul_lbase-5.5), c(data$aug_lbase-5.5), 
                      c(data$sep_lbase-5.5),
                      
                      c(data$winter_crop/11-17.35), c(data$winter_crop/11-17.35), c(data$winter_crop/11-17.35), c(data$summer_crop/11-17.35), 
                      c(data$summer_crop/11-17.35), c(data$summer_crop/11-17.35), c(data$summer_crop/11-17.35), c(data$roy_crop/11-17.35), 
                      c(data$roy_crop/11-17.35), c(data$roy_crop/11-17.35), c(data$roy_crop/11-17.35)), nrow=n, ncol=44, byrow=FALSE)

prod_mat <- as.matrix(XC[, -c(45:ncol(XC))])

XC$profit <- rowSums(price_mat*prod_mat)-37645*m-0.100385*(640000+(4000000/(1.2*c+5)))*c+XC$extra_profit-XC$cost-extra_fc

XC$electricity <- ifelse(c==0, 0, ifelse(c==1, 1285161, ifelse(c==2, 2300255, ifelse(c==3, 3233510, ifelse(c==4, 4108624,
                                                                                                           ifelse(c==5, 4960041, ifelse(c==6, 5770701, ifelse(c==7, 6559135, ifelse(c==8, 7336343, 8104855)))))))))

XC$ammonia <- ifelse(m==0, 0, ifelse(m==1, 350000, ifelse(m==2, 700000, ifelse(m==3, 1050000, ifelse(m==4, 1400000, 1750000)))))

XC$roic <- XC$profit/((XC$electricity+XC$ammonia)*0.25)
XC$loss <- ifelse(XC$profit<0, 1, 0)
XC$roic100 <- ifelse(XC$roic>=1, 1, 0)

assign(paste("XC", c, m, sep = ""), XC)

XC %>% filter(between(profit, quantile(profit, 0.00), quantile(profit, 0.95))) -> XC_new

#profit table reporting average annual profit and ROIC
profit_table <- data.frame(Model=paste0("C=", c, ", M=", m), MWe=c, Ammonia=m, Ave_ROIC=mean(XC$roic)*100,
                           Prob_loss=mean(XC$loss)*100, Prob_ROIC=mean(XC$roic100)*100, Mean_profit=mean(XC$profit),
                           SD_profit=sd(XC$profit), Extra=mean(XC$extra_profit), Cost=mean(XC$cost),
                           Ave_ROIC_95=mean(XC_new$roic)*100, Prob_loss_95=mean(XC_new$loss)*100, 
                           Prob_ROIC_95=mean(XC_new$roic100)*100, Mean_profit_95=mean(XC_new$profit), SD_profit_95=sd(XC_new$profit))
profit_table


#production table reporting average annual electricity and ammonia production
prod_table <- data.frame(dec=mean(colMeans(XC[, c(1,12,23)])), dec=mean(XC[, c(34)])/11,
                         jan=mean(colMeans(XC[, c(2,13,24)])), jan=mean(XC[, c(35)])/11,
                         feb=mean(colMeans(XC[, c(3,14,25)])), feb=mean(XC[, c(36)])/11,
                         mar1=mean(colMeans(XC[, c(4,15,26)])), mar1=mean(XC[, c(37)])/11,
                         mar2=mean(colMeans(XC[, c(5,16,27)])), mar2=mean(XC[, c(38)])/11,
                         apr=mean(colMeans(XC[, c(6,17,28)])), apr=mean(XC[, c(39)])/11,
                         may=mean(colMeans(XC[, c(7,18,29)])), may=mean(XC[, c(40)])/11,
                         jun=mean(colMeans(XC[, c(8,19,30)])), jun=mean(XC[, c(41)])/11,
                         jul=mean(colMeans(XC[, c(9,20,31)])), jul=mean(XC[, c(42)])/11,
                         aug=mean(colMeans(XC[, c(10,21,32)])), aug=mean(XC[, c(43)])/11,
                         sep=mean(colMeans(XC[, c(11,22,33)])), sep=mean(XC[, c(44)])/11)
prod_table


year <- rep(1:833, each=12)
year <- append(year, c(2,6,9,12))
set.seed(12345)
year <- sample(year)

XC$year <- year

XC_group = XC %>% group_by(year) %>%
  summarise(profit = sum(profit)*0.3757,
            #roic = sum(profit)*0.3757/(XC$electricity[1]*0.25+XC$ammonia[1]*0.25),
            n = n())

XC_group$roic <- 100*XC_group$profit/(XC$electricity[1]*0.25+XC$ammonia[1]*0.25)

#cumulative profit table reporting average profit and ROIC over a 12-year period
agg_table <- data.frame(Model=paste0("C=", c, ", M=", m), Ave_Profit=mean(XC_group$profit), SD_Profit=sd(XC_group$profit),
                        Ave_ROIC=mean(XC_group$roic), ROIC0=nrow(subset(XC_group, roic<0)), ROIC100=nrow(subset(XC_group, roic>=0 & roic<250)), 
                        ROIC200=nrow(subset(XC_group, roic>=250 & roic<500)), ROIC300=nrow(subset(XC_group, roic>=500)))
agg_table