rm(list=ls())

data <- read.csv("gin.csv")

#there is another package called linprog (just like matlab), but that lpsolve is written in C which is a compiled language as opposed to R,
#which is an interpreted language. One consequence is that lpsolve is much faster
#http://www.noamross.net/archives/2014-04-16-vectorization-in-r-why/

n=nrow(data)
U <- data$gin_medium

c=4
m=0

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
    
    f.obj <- c((data$dec_peak[i]-5.5), (data$dec_ubase[i]-5.5), (data$dec_lbase[i]-5.5), ((data$winter_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(305*c, U[i], 60*c, 139*c, 106*c, 306*m) #612*m
    
    results_dec <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_dec$solution[1]+results_dec$solution[2]+results_dec$solution[3]+results_dec$solution[4])
    
    XC[i,1] <- results_dec$solution[1]
    XC[i,12] <- results_dec$solution[2]
    XC[i,23] <- results_dec$solution[3]
    XC[i,34] <- results_dec$solution[4]
    XC[i,45] <- as.numeric(format(round(U[i]-total, 3), nsmall=3))
    gin <- XC[i,45]
    
    #for january
    
    f.obj <- c((data$jan_peak[i]-5.5), (data$jan_ubase[i]-5.5), (data$jan_lbase[i]-5.5), ((data$winter_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c, gin, 120*c, 278*c, 212*c, 612*m) #612*m
    
    results_jan <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_jan$solution[1]+results_jan$solution[2]+results_jan$solution[3]+results_jan$solution[4])
    
    XC[i,2] <- results_jan$solution[1]
    XC[i,13] <- results_jan$solution[2]
    XC[i,24] <- results_jan$solution[3]
    XC[i,35] <- results_jan$solution[4]
    XC[i,46] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,46]
    
    #for february
    
    f.obj <- c((data$feb_peak[i]-5.5), (data$feb_ubase[i]-5.5), (data$feb_lbase[i]-5.5), ((data$winter_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c, gin, 120*c, 278*c, 212*c, 612*m) #612*m
    
    results_feb <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_feb$solution[1]+results_feb$solution[2]+results_feb$solution[3]+results_feb$solution[4])
    
    XC[i,3] <- results_feb$solution[1]
    XC[i,14] <- results_feb$solution[2]
    XC[i,25] <- results_feb$solution[3]
    XC[i,36] <- results_feb$solution[4]
    XC[i,47] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,47]
    
    #for march first half
    
    f.obj <- c((data$mar_peak[i]-5.5), (data$mar_ubase[i]-5.5), (data$mar_lbase[i]-5.5), ((data$summer_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(305*c, gin, 60*c, 139*c, 106*c, 306*m) #612*m
    
    results_mar1 <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_mar1$solution[1]+results_mar1$solution[2]+results_mar1$solution[3]+results_mar1$solution[4])
    
    XC[i,4] <- results_mar1$solution[1]
    XC[i,15] <- results_mar1$solution[2]
    XC[i,26] <- results_mar1$solution[3]
    XC[i,37] <- results_mar1$solution[4]
    XC[i,48] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,48]
    
    #for march second half
    
    f.obj <- c((data$mar_ubase[i]-5.5), (data$mar_lbase[i]-5.5), ((data$summer_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(305*c, gin, 139*c, 106*c, 306*m) #612*m
    
    results_mar2 <- lp("max", f.obj, f.con_sum, f.dir_sum, f.rhs)
    
    total <- as.numeric(results_mar2$solution[1]+results_mar2$solution[2]+results_mar2$solution[3])
    
    XC[i,5] <- 0
    XC[i,16] <- results_mar2$solution[1]
    XC[i,27] <- results_mar2$solution[2]
    XC[i,38] <- results_mar2$solution[3]
    XC[i,49] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,49]
    
    #for april
    
    f.obj <- c((data$apr_ubase[i]-5.5), (data$apr_lbase[i]-5.5), ((data$summer_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c, gin, 278*c, 212*c, 612*m) #612*m
    
    results_apr <- lp("max", f.obj, f.con_sum, f.dir_sum, f.rhs)
    
    total <- as.numeric(results_apr$solution[1]+results_apr$solution[2]+results_apr$solution[3])
    
    XC[i,6] <- 0
    XC[i,17] <- results_apr$solution[1]
    XC[i,28] <- results_apr$solution[2]
    XC[i,39] <- results_apr$solution[3]
    XC[i,50] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,50]
    
    #for may
    
    f.obj <- c((data$may_ubase[i]-5.5), (data$may_lbase[i]-5.5), ((data$summer_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c, gin, 278*c, 212*c, 612*m) #612*m
    
    results_may <- lp("max", f.obj, f.con_sum, f.dir_sum, f.rhs)
    
    total <- as.numeric(results_may$solution[1]+results_may$solution[2]+results_may$solution[3])
    
    XC[i,7] <- 0
    XC[i,18] <- results_may$solution[1]
    XC[i,29] <- results_may$solution[2]
    XC[i,40] <- results_may$solution[3]
    XC[i,51] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,51]
    
    #for june
    
    f.obj <- c((data$jun_peak[i]-5.5), (data$jun_ubase[i]-5.5), (data$jun_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c, gin, 120*c, 278*c, 212*c, 612*m) #612*m
    
    results_jun <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_jun$solution[1]+results_jun$solution[2]+results_jun$solution[3]+results_jun$solution[4])
    
    XC[i,8] <- results_jun$solution[1]
    XC[i,19] <- results_jun$solution[2]
    XC[i,30] <- results_jun$solution[3]
    XC[i,41] <- results_jun$solution[4]
    XC[i,52] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,52]
    
    #for july
    
    f.obj <- c((data$jul_peak[i]-5.5), (data$jul_ubase[i]-5.5), (data$jul_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c, gin, 120*c, 278*c, 212*c, 612*m) #612*m
    
    results_jul <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_jul$solution[1]+results_jul$solution[2]+results_jul$solution[3]+results_jul$solution[4])
    
    XC[i,9] <- results_jul$solution[1]
    XC[i,20] <- results_jul$solution[2]
    XC[i,31] <- results_jul$solution[3]
    XC[i,42] <- results_jul$solution[4]
    XC[i,53] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,53]
    
    #for august
    
    f.obj <- c((data$aug_peak[i]-5.5), (data$aug_ubase[i]-5.5), (data$aug_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c, gin, 120*c, 278*c, 212*c, 612*m) #612*m
    
    results_aug <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_aug$solution[1]+results_aug$solution[2]+results_aug$solution[3]+results_aug$solution[4])
    
    XC[i,10] <- results_aug$solution[1]
    XC[i,21] <- results_aug$solution[2]
    XC[i,32] <- results_aug$solution[3]
    XC[i,43] <- results_aug$solution[4]
    XC[i,54] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,54]
    
    #for september
    
    f.obj <- c((data$sep_peak[i]-5.5), (data$sep_ubase[i]-5.5), (data$sep_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(305*c, gin, 60*c, 139*c, 106*c, 306*m) #612*m
    
    results_sep <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_sep$solution[1]+results_sep$solution[2]+results_sep$solution[3]+results_sep$solution[4])
    
    XC[i,11] <- results_sep$solution[1]
    XC[i,22] <- results_sep$solution[2]
    XC[i,33] <- results_sep$solution[3]
    XC[i,44] <- results_sep$solution[4]
    XC[i,55] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,55]
    
  }
)

store <- XC

#XC <- store
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

XC$profit <- rowSums(price_mat*prod_mat)-37645*m-0.100385*(640000+(4000000/(1.2*c+5)))*c+XC$extra_profit-XC$cost

XC$electricity <- ifelse(c==0, 0, ifelse(c==1, 1285161, ifelse(c==2, 2300255, ifelse(c==3, 3233510, ifelse(c==4, 4108624,
                                                                                                           ifelse(c==5, 4960041, ifelse(c==6, 5770701, ifelse(c==7, 6559135, ifelse(c==8, 7336343, 8104855)))))))))

XC$ammonia <- ifelse(m==0, 0, ifelse(m==1, 350000, ifelse(m==2, 700000, ifelse(m==3, 1050000, ifelse(m==4, 1400000, 1750000)))))

XC$roic <- XC$profit/((XC$electricity+XC$ammonia)*0.25)
XC$loss <- ifelse(XC$profit<0, 1, 0)
XC$roic100 <- ifelse(XC$roic>=1, 1, 0)

assign(paste("XC", c, m, sep = ""), XC)

XC %>% filter(between(profit, quantile(profit, 0.00), quantile(profit, 0.95))) -> XC_new

profit_table <- data.frame(Model=paste0("C=", c, ", M=", m), MWe=c, Ammonia=m, Ave_ROIC=mean(XC$roic)*100,
                           Prob_loss=mean(XC$loss)*100, Prob_ROIC=mean(XC$roic100)*100, Mean_profit=mean(XC$profit),
                           SD_profit=sd(XC$profit), Ave_ROIC_95=mean(XC_new$roic)*100, Prob_loss_95=mean(XC_new$loss)*100, 
                           Prob_ROIC_95=mean(XC_new$roic100)*100, Mean_profit_95=mean(XC_new$profit), SD_profit_95=sd(XC_new$profit))
profit_table



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
            roic = sum(profit)*0.3757/(XC$electricity[1]*0.25+XC$ammonia[1]*0.25),
            n = n())

agg_table <- data.frame(Model=paste0("C=", c, ", M=", m), Ave_Profit=mean(XC_group$profit), SD_Profit=sd(XC_group$profit),
                        Ave_ROIC=mean(XC_group$roic), SD_ROIC=sd(XC_group$roic), ROIC0=nrow(subset(XC_group, roic<0)),
                        ROIC100=nrow(subset(XC_group, roic>=0 & roic<1)), ROIC200=nrow(subset(XC_group, roic>=1 & roic<2)), 
                        ROIC300=nrow(subset(XC_group, roic>=2)))
agg_table


#plot

library(readxl)
data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(1:16),]

plot_sub <- subset(data_plot, Group=="C=0, M=0" | Group=="C=1, M=0" | Group=="C=2, M=0")

ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=Group)) +
  geom_point() + geom_text(hjust=0, vjust=0) + coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                                                               xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+50000)) + 
  geom_line(data = plot_sub) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))




df <- data.frame(c1m0=XC10$profit, c2m0=XC20$profit)

plot(data$ammonia, data$c1m0,
     xlab = "Price of Ammonia", ylab = "Profit",
     pch = 19, frame = FALSE)
abline(lm(c1m0 ~ ammonia, data = data), col = "blue")

legend("topright",legend=paste("R2 =", format(summary(lm(c1m0 ~ ammonia, data = data))$r.squared,digits=3)), cex=1, bty = "n")


plot <- read.csv("Summary.csv")
plot <- subset(plot, Class=="Medium_gin")
plot_sub <- subset(plot, Group=="C=0, M=0" | Group=="C=1, M=0" | Group=="C=2, M=0")
plot_sub <- subset(plot, Group=="C=0, M=0" | Group=="C=2, M=0" | Group=="C=3, M=0" | Group=="C=4, M=0" | Group=="C=5, M=0" | 
                     Group=="C=6, M=0" | Group=="C=7, M=0")

ggplot(data = plot, aes(x = SD, y = Avgprofit, label=Group)) +
  geom_point() + geom_text(hjust=0, vjust=0) + coord_cartesian(ylim = c(min(plot$Avgprofit)-50000, max(plot$Avgprofit)+50000), 
                                                               xlim = c(min(plot$SD)-50000, max(plot$SD)+50000)) + 
  geom_line(data = plot_sub) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))

plot <- read.csv("Summary.csv")
plot <- subset(plot, Class=="Ammonia" | No==1 | No==7 | No==8 | No==9 | No==10 | No==11)
plot_sub1 <- subset(plot, No==1 | No==45 | No==46)
plot_sub2 <- subset(plot, No==1 | No==51)
plot_sub3 <- subset(plot, No==1 | No==7 | No==8)
ggplot(data = plot, aes(x = SD, y = Avgprofit, label=Group)) +
  geom_point() + geom_text(hjust=0, vjust=0) + coord_cartesian(ylim = c(min(plot$Avgprofit)-50000, max(plot$Avgprofit)+50000), 
                                                               xlim = c(min(plot$SD)-50000, max(plot$SD)+110000)) + 
  geom_line(data = plot_sub1, linetype = "dashed") + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  geom_line(data = plot_sub2, linetype = "dotted") + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_line(data = plot_sub3) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))

plot <- read.csv("Summary.csv")
plot <- subset(plot, Class=="Two-thirds" | No==1 | No==2 | No==3 | No==4 | No==5 | No==6)
plot_sub1 <- subset(plot, No==1 | No==2 | No==3)
plot_sub2 <- subset(plot, No==1 | No==55)
ggplot(data = plot, aes(x = SD, y = Avgprofit, label=Group)) +
  geom_point() + geom_text(hjust=0, vjust=0) + coord_cartesian(ylim = c(min(plot$Avgprofit)-50000, max(plot$Avgprofit)+50000), 
                                                               xlim = c(min(plot$SD)-50000, max(plot$SD)+110000)) + 
  geom_line(data = plot_sub1, linetype = "dashed") + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  geom_line(data = plot_sub2, linetype = "dotted") + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))




y <- c(1285161.29, 2300255.064, 3315348.837, 4166765.33, 5018181.818, 10154226.8)
x1 <- c(1,2,3,4,5,12)
x2 <- c(1,4,9,16,25,144)

df <- data.frame (y <- c(1285161.29, 2300255.064, 3315348.837, 4166765.33, 5018181.818, 10154226.8),
                  x1 <- c(1,2,3,4,5,12), x2 <- c(1,4,9,16,25,144))

summary(lm(y~x1+x2, data=df))

y6 <- 278256+1048928*7-18848*49

#plot

hist(profit)
abline(v = mean(profit, na.rm=TRUE), col="red", lwd=3, lty=2)
abline(v = median(profit, na.rm=TRUE), col="blue", lwd=3, lty=2)

quants<-quantile(profit, probs = c(0.05, 0.95), na.rm=TRUE)
profit_df <- profit[profit < quants[2]]

hist(profit_df, main=paste0("Distribution of profit (C=", c, ", M=", m, ")"),
     xlab="Profit in $")
abline(v = mean(profit_df, na.rm=TRUE), col="red", lwd=3, lty=2)
abline(v = median(profit_df, na.rm=TRUE), col="blue", lwd=3, lty=2)





