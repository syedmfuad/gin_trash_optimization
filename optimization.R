rm(list=ls())

data <- read.csv("gin.csv")

#there is another package called linprog (just like matlab), but that lpsolve is written in C which is a compiled language as opposed to R,
#which is an interpreted language. One consequence is that lpsolve is much faster
#http://www.noamross.net/archives/2014-04-16-vectorization-in-r-why/

n=nrow(data)
U <- data$gin_small

c=1
m=1

XC=matrix(NA, nrow=n, ncol=50)

XC <- assign(paste("XC", c, m, sep = ""), matrix(NA, nrow=n, ncol=50))

#profvis for time
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
    XC[i,11] <- results_dec$solution[2]
    XC[i,21] <- results_dec$solution[3]
    XC[i,31] <- results_dec$solution[4]
    XC[i,41] <- as.numeric(format(round(U[i]-total, 3), nsmall=3))
    gin <- XC[i,41]
    
    #for january
    
    f.obj <- c((data$jan_peak[i]-5.5), (data$jan_ubase[i]-5.5), (data$jan_lbase[i]-5.5), ((data$winter_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c, gin, 120*c, 278*c, 212*c, 612*m) #612*m
    
    results_jan <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_jan$solution[1]+results_jan$solution[2]+results_jan$solution[3]+results_jan$solution[4])
    
    XC[i,2] <- results_jan$solution[1]
    XC[i,12] <- results_jan$solution[2]
    XC[i,22] <- results_jan$solution[3]
    XC[i,32] <- results_jan$solution[4]
    XC[i,42] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,42]
    
    #for february
    
    f.obj <- c((data$feb_peak[i]-5.5), (data$feb_ubase[i]-5.5), (data$feb_lbase[i]-5.5), ((data$winter_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c, gin, 120*c, 278*c, 212*c, 612*m) #612*m
    
    results_feb <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_feb$solution[1]+results_feb$solution[2]+results_feb$solution[3]+results_feb$solution[4])
    
    XC[i,3] <- results_feb$solution[1]
    XC[i,13] <- results_feb$solution[2]
    XC[i,23] <- results_feb$solution[3]
    XC[i,33] <- results_feb$solution[4]
    XC[i,43] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,43]
    
    #for march
    
    f.obj <- c((data$mar_peak[i]-5.5), (data$mar_ubase[i]-5.5), (data$mar_lbase[i]-5.5), ((data$summer_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c, gin, 120*c, 278*c, 212*c, 612*m) #612*m
    
    results_mar <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_mar$solution[1]+results_mar$solution[2]+results_mar$solution[3]+results_mar$solution[4])
    
    XC[i,4] <- results_mar$solution[1]
    XC[i,14] <- results_mar$solution[2]
    XC[i,24] <- results_mar$solution[3]
    XC[i,34] <- results_mar$solution[4]
    XC[i,44] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,44]
    
    #for april
    
    f.obj <- c((data$apr_ubase[i]-5.5), (data$apr_lbase[i]-5.5), ((data$summer_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c, gin, 278*c, 212*c, 612*m) #612*m
    
    results_apr <- lp("max", f.obj, f.con_sum, f.dir_sum, f.rhs)
    
    total <- as.numeric(results_apr$solution[1]+results_apr$solution[2]+results_apr$solution[3])
    
    XC[i,5] <- 0
    XC[i,15] <- results_apr$solution[1]
    XC[i,25] <- results_apr$solution[2]
    XC[i,35] <- results_apr$solution[3]
    XC[i,45] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,45]
    
    #for may
    
    f.obj <- c((data$may_ubase[i]-5.5), (data$may_lbase[i]-5.5), ((data$summer_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c, gin, 278*c, 212*c, 612*m) #612*m
    
    results_may <- lp("max", f.obj, f.con_sum, f.dir_sum, f.rhs)
    
    total <- as.numeric(results_may$solution[1]+results_may$solution[2]+results_may$solution[3])
    
    XC[i,6] <- 0
    XC[i,16] <- results_may$solution[1]
    XC[i,26] <- results_may$solution[2]
    XC[i,36] <- results_may$solution[3]
    XC[i,46] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,46]
    
    #for june
    
    f.obj <- c((data$jun_peak[i]-5.5), (data$jun_ubase[i]-5.5), (data$jun_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c, gin, 120*c, 278*c, 212*c, 612*m) #612*m
    
    results_jun <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_jun$solution[1]+results_jun$solution[2]+results_jun$solution[3]+results_jun$solution[4])
    
    XC[i,7] <- results_jun$solution[1]
    XC[i,17] <- results_jun$solution[2]
    XC[i,27] <- results_jun$solution[3]
    XC[i,37] <- results_jun$solution[4]
    XC[i,47] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,47]
    
    #for july
    
    f.obj <- c((data$jul_peak[i]-5.5), (data$jul_ubase[i]-5.5), (data$jul_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c, gin, 120*c, 278*c, 212*c, 612*m) #612*m
    
    results_jul <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_jul$solution[1]+results_jul$solution[2]+results_jul$solution[3]+results_jul$solution[4])
    
    XC[i,8] <- results_jul$solution[1]
    XC[i,18] <- results_jul$solution[2]
    XC[i,28] <- results_jul$solution[3]
    XC[i,38] <- results_jul$solution[4]
    XC[i,48] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,48]
    
    #for august
    
    f.obj <- c((data$aug_peak[i]-5.5), (data$aug_ubase[i]-5.5), (data$aug_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c, gin, 120*c, 278*c, 212*c, 612*m) #612*m
    
    results_aug <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_aug$solution[1]+results_aug$solution[2]+results_aug$solution[3]+results_aug$solution[4])
    
    XC[i,9] <- results_aug$solution[1]
    XC[i,19] <- results_aug$solution[2]
    XC[i,29] <- results_aug$solution[3]
    XC[i,39] <- results_aug$solution[4]
    XC[i,49] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,49]
    
    #for september
    
    f.obj <- c((data$sep_peak[i]-5.5), (data$sep_ubase[i]-5.5), (data$sep_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(305*c, gin, 60*c, 139*c, 106*c, 306*m) #612*m
    
    results_aug <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_aug$solution[1]+results_aug$solution[2]+results_aug$solution[3]+results_aug$solution[4])
    
    XC[i,10] <- results_aug$solution[1]
    XC[i,20] <- results_aug$solution[2]
    XC[i,30] <- results_aug$solution[3]
    XC[i,40] <- results_aug$solution[4]
    XC[i,50] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,50]
    
  }
)

store <- XC

#XC <- store
XC <- as.data.frame(XC)

summary(XC)

XC$extra_profit <- XC$V50*10

XC$V44 <- XC$V44-XC$V50
XC$V45 <- XC$V45-XC$V50
XC$V46 <- XC$V46-XC$V50
XC$V47 <- XC$V47-XC$V50
XC$V48 <- XC$V48-XC$V50
XC$V49 <- XC$V49-XC$V50
XC$V50 <- XC$V50-XC$V50

XC$cost <- XC$V41+XC$V42+XC$V43+XC$V44+XC$V45+XC$V46+XC$V47+XC$V48+XC$V49+XC$V50

price_mat <- matrix(c(c(data$dec_peak-5.5), c(data$jan_peak-5.5), c(data$feb_peak-5.5), c(data$mar_peak-5.5), 
                      c(rep(0, n)), c(rep(0, n)), c(data$jun_peak-5.5), c(data$jul_peak-5.5), c(data$aug_peak-5.5), c(data$sep_peak-5.5), 
                      
                      c(data$dec_ubase-5.5), c(data$jan_ubase-5.5), c(data$feb_ubase-5.5), c(data$mar_ubase-5.5), c(data$apr_ubase-5.5), 
                      c(data$may_ubase-5.5), c(data$jun_ubase-5.5), c(data$jul_ubase-5.5), c(data$aug_ubase-5.5), c(data$sep_ubase-5.5),
                      
                      c(data$dec_lbase-5.5), c(data$jan_lbase-5.5), c(data$feb_lbase-5.5), c(data$mar_lbase-5.5), c(data$apr_lbase-5.5), 
                      c(data$may_lbase-5.5), c(data$jun_lbase-5.5), c(data$jul_lbase-5.5), c(data$aug_lbase-5.5), c(data$sep_lbase-5.5),
                      
                      c(data$winter_crop/11-17.35), c(data$winter_crop/11-17.35), c(data$winter_crop/11-17.35), c(data$summer_crop/11-17.35), 
                      c(data$summer_crop/11-17.35), c(data$summer_crop/11-17.35), c(data$roy_crop/11-17.35), c(data$roy_crop/11-17.35), 
                      c(data$roy_crop/11-17.35), c(data$roy_crop/11-17.35)), nrow=n, ncol=40, byrow=FALSE)

prod_mat <- as.matrix(XC[, -c(41:ncol(XC))])

XC$profit <- rowSums(price_mat*prod_mat)-37645*m-0.100385*(640000+(4000000/(1.2*c+5)))*c+XC$extra_profit-XC$cost

XC$electricity <- ifelse(c==0, 0, ifelse(c==1, 1285161, ifelse(c==2, 2300255, ifelse(c==3, 3233510, ifelse(c==4, 4108624,
                                                                                                           ifelse(c==5, 4960041, ifelse(c==6, 5770701, ifelse(c==7, 6559135, ifelse(c==8, 7336343, 8104855)))))))))

XC$ammonia <- ifelse(m==0, 0, ifelse(m==1, 350000, ifelse(m==2, 700000, ifelse(m==3, 1050000, ifelse(m==4, 1400000, 1750000)))))

XC$roic <- XC$profit/((XC$electricity+XC$ammonia)*0.25)
XC$loss <- ifelse(XC$profit<0, 1, 0)
XC$roic100 <- ifelse(XC$roic>=1, 1, 0)

assign(paste("XC", c, m, sep = ""), XC)

XC %>% filter(between(profit, quantile(profit, 0.00), quantile(profit, 0.95))) -> XC_new

table <- data.frame(Model=paste0("C=", c, ", M=", m), MWe=c, Ammonia=m, Ave_ROI=mean(XC_new$roi)*100,
                    Prob_loss=mean(XC_new$loss)*100, Prob_ROI=mean(XC_new$roi25)*100, Mean_profit=mean(XC_new$profit),
                    SD_profit=sd(XC_new$profit))

peak<-c(rowSums(XC[,c(1:10)]))
ubase<-c(rowSums(XC[,c(11:20)]))
lbase<-c(rowSums(XC[,c(21:30)]))
ammonia<-c(rowSums(XC[,c(31:40)]))

extra <- data.frame(Model=paste0("C=", c, ", M=", m), Revenue=mean(revenue), Revenue_SD=sd(revenue), Peak=mean(peak), Ubase=mean(ubase),
                    Lbase=mean(lbase), Ammonia=mean(ammonia), Total=mean(prod))


year <- rep(1:833, each=12)
year <- append(year, c(2,6,9,12))
year <- sample(year)

XC$year <- year

XC_group = XC %>% group_by(year) %>%
  summarise(profit = sum(profit),
            roic = sum(roic),
            n = n())

#plot


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





