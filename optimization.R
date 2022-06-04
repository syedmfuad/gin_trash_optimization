rm(list=ls())

data <- read.csv("gin.csv")

#there is another package called linprog (just like matlab), but that lpsolve is written in C which is a compiled language as opposed to R,
#which is an interpreted language. One consequence is that lpsolve is much faster
#http://www.noamross.net/archives/2014-04-16-vectorization-in-r-why/

#plot_sub <- subset(data_plot, Group=="C=0, M=0" | Group=="C=1, M=0" | Group=="C=2, M=0")
#plot_sub <- subset(data_plot, Group=="C=0, M=0" | Group=="C=1, M=0" | Group=="C=2, M=0" | Group=="C=2, M=1" | Group=="C=3, M=0" | Group=="C=3, M=1" |
#Group=="C=4, M=1" | Group=="C=5, M=1" | Group=="C=5, M=0" | Group=="C=6, M=0")

#things to change: (i) constraints in every month consistent to hours of operation; (ii) capacity of ammonia plant 

#sensitivity analysis: (i) change eff=2/3; (ii) reduce lower base to 25; (iii) reduce ammonia marginal cost to 85

n=nrow(data)
U <- data$gin_medium

c=7
m=1
eff=1

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
    
    f.obj <- c((data$dec_peak[i]-5.5), (data$dec_ubase[i]-5.5), (data$dec_lbase[i]-5.5), ((data$winter_crop[i]/11-17.35))) #13.23 #17.35
    f.rhs <- c(340*c*eff, U[i]*eff, 68*c*eff, 187*c*eff, 85*c*eff, 369*m*eff) #612*m
    
    results_dec <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_dec$solution[1]+results_dec$solution[2]+results_dec$solution[3]+results_dec$solution[4])*1/eff
    
    XC[i,1] <- results_dec$solution[1]
    XC[i,12] <- results_dec$solution[2]
    XC[i,23] <- results_dec$solution[3]
    XC[i,34] <- results_dec$solution[4]
    XC[i,45] <- as.numeric(format(round(U[i]-total, 3), nsmall=3))
    gin <- XC[i,45]
    
    #for january
    
    f.obj <- c((data$jan_peak[i]-5.5), (data$jan_ubase[i]-5.5), (data$jan_lbase[i]-5.5), ((data$winter_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(620*c*eff, gin*eff, 217*c*eff, 248*c*eff, 155*c*eff, 672*m*eff) #612*m
    
    results_jan <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_jan$solution[1]+results_jan$solution[2]+results_jan$solution[3]+results_jan$solution[4])*1/eff
    
    XC[i,2] <- results_jan$solution[1]
    XC[i,13] <- results_jan$solution[2]
    XC[i,24] <- results_jan$solution[3]
    XC[i,35] <- results_jan$solution[4]
    XC[i,46] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,46]
    
    #for february
    
    f.obj <- c((data$feb_peak[i]-5.5), (data$feb_ubase[i]-5.5), (data$feb_lbase[i]-5.5), ((data$winter_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(560*c*eff, gin*eff, 196*c*eff, 224*c*eff, 140*c*eff, 640*m*eff) #612*m
    
    results_feb <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_feb$solution[1]+results_feb$solution[2]+results_feb$solution[3]+results_feb$solution[4])*1/eff
    
    XC[i,3] <- results_feb$solution[1]
    XC[i,14] <- results_feb$solution[2]
    XC[i,25] <- results_feb$solution[3]
    XC[i,36] <- results_feb$solution[4]
    XC[i,47] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,47]
    
    #for march first half
    
    f.obj <- c((data$mar_peak[i]-5.5), (data$mar_ubase[i]-5.5), (data$mar_lbase[i]-5.5), ((data$summer_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(300*c*eff, gin*eff, 60*c*eff, 165*c*eff, 75*c*eff, 325*m*eff) #612*m
    
    results_mar1 <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_mar1$solution[1]+results_mar1$solution[2]+results_mar1$solution[3]+results_mar1$solution[4])*1/eff
    
    XC[i,4] <- results_mar1$solution[1]
    XC[i,15] <- results_mar1$solution[2]
    XC[i,26] <- results_mar1$solution[3]
    XC[i,37] <- results_mar1$solution[4]
    XC[i,48] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,48]
    
    #for march second half
    
    f.obj <- c((data$mar_ubase[i]-5.5), (data$mar_lbase[i]-5.5), ((data$summer_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(304*c*eff, gin*eff, 224*c*eff, 80*c*eff, 347*m*eff) #612*m
    
    results_mar2 <- lp("max", f.obj, f.con_sum, f.dir_sum, f.rhs)
    
    total <- as.numeric(results_mar2$solution[1]+results_mar2$solution[2]+results_mar2$solution[3])*1/eff
    
    XC[i,5] <- 0
    XC[i,16] <- results_mar2$solution[1]
    XC[i,27] <- results_mar2$solution[2]
    XC[i,38] <- results_mar2$solution[3]
    XC[i,49] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,49]
    
    #for april
    
    f.obj <- c((data$apr_ubase[i]-5.5), (data$apr_lbase[i]-5.5), ((data$summer_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(570*c*eff, gin*eff, 420*c*eff, 150*c*eff, 672*m*eff) #612*m
    
    results_apr <- lp("max", f.obj, f.con_sum, f.dir_sum, f.rhs)
    
    total <- as.numeric(results_apr$solution[1]+results_apr$solution[2]+results_apr$solution[3])*1/eff
    
    XC[i,6] <- 0
    XC[i,17] <- results_apr$solution[1]
    XC[i,28] <- results_apr$solution[2]
    XC[i,39] <- results_apr$solution[3]
    XC[i,50] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,50]
    
    #for may
    
    f.obj <- c((data$may_ubase[i]-5.5), (data$may_lbase[i]-5.5), ((data$summer_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(589*c*eff, gin*eff, 434*c*eff, 155*c*eff, 672*m*eff) #612*m
    
    results_may <- lp("max", f.obj, f.con_sum, f.dir_sum, f.rhs)
    
    total <- as.numeric(results_may$solution[1]+results_may$solution[2]+results_may$solution[3])*1/eff
    
    XC[i,7] <- 0
    XC[i,18] <- results_may$solution[1]
    XC[i,29] <- results_may$solution[2]
    XC[i,40] <- results_may$solution[3]
    XC[i,51] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,51]
    
    #for june
    
    f.obj <- c((data$jun_peak[i]-5.5), (data$jun_ubase[i]-5.5), (data$jun_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(600*c*eff, gin*eff, 150*c*eff, 150*c*eff, 300*c*eff, 672*m*eff) #612*m
    
    results_jun <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_jun$solution[1]+results_jun$solution[2]+results_jun$solution[3]+results_jun$solution[4])*1/eff
    
    XC[i,8] <- results_jun$solution[1]
    XC[i,19] <- results_jun$solution[2]
    XC[i,30] <- results_jun$solution[3]
    XC[i,41] <- results_jun$solution[4]
    XC[i,52] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,52]
    
    #for july
    
    f.obj <- c((data$jul_peak[i]-5.5), (data$jul_ubase[i]-5.5), (data$jul_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(620*c*eff, gin*eff, 155*c*eff, 155*c*eff, 310*c*eff, 672*m*eff) #612*m
    
    results_jul <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_jul$solution[1]+results_jul$solution[2]+results_jul$solution[3]+results_jul$solution[4])*1/eff
    
    XC[i,9] <- results_jul$solution[1]
    XC[i,20] <- results_jul$solution[2]
    XC[i,31] <- results_jul$solution[3]
    XC[i,42] <- results_jul$solution[4]
    XC[i,53] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,53]
    
    #for august
    
    f.obj <- c((data$aug_peak[i]-5.5), (data$aug_ubase[i]-5.5), (data$aug_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(610*c*eff, gin*eff, 150*c*eff, 150*c*eff, 300*c*eff, 672*m*eff) #612*m
    
    results_aug <- lp("max", f.obj, f.con, f.dir, f.rhs)
    
    total <- as.numeric(results_aug$solution[1]+results_aug$solution[2]+results_aug$solution[3]+results_aug$solution[4])*1/eff
    
    XC[i,10] <- results_aug$solution[1]
    XC[i,21] <- results_aug$solution[2]
    XC[i,32] <- results_aug$solution[3]
    XC[i,43] <- results_aug$solution[4]
    XC[i,54] <- as.numeric(format(round(gin-total, 3), nsmall=3))
    gin <- XC[i,54]
    
    #for september
    
    f.obj <- c((data$sep_peak[i]-5.5), (data$sep_ubase[i]-5.5), (data$sep_lbase[i]-5.5), ((data$roy_crop[i]/11-17.35))) #13.68 #18.51
    f.rhs <- c(300*c*eff, gin*eff, 75*c*eff, 75*c*eff, 150*c*eff, 336*m*eff) #612*m
    
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
                           SD_profit=sd(XC$profit), Extra=mean(XC$extra_profit), Cost=mean(XC$cost),
                           Ave_ROIC_95=mean(XC_new$roic)*100, Prob_loss_95=mean(XC_new$loss)*100, 
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
            #roic = sum(profit)*0.3757/(XC$electricity[1]*0.25+XC$ammonia[1]*0.25),
            n = n())

XC_group$roic <- 100*XC_group$profit/(XC$electricity[1]*0.25+XC$ammonia[1]*0.25)

agg_table <- data.frame(Model=paste0("C=", c, ", M=", m), Ave_Profit=mean(XC_group$profit), SD_Profit=sd(XC_group$profit),
                        Ave_ROIC=mean(XC_group$roic), ROIC0=nrow(subset(XC_group, roic<0)), ROIC100=nrow(subset(XC_group, roic>=0 & roic<250)), 
                        ROIC200=nrow(subset(XC_group, roic>=250 & roic<500)), ROIC300=nrow(subset(XC_group, roic>=500)))
agg_table



data$profit <- data$gin_medium*10
data %>% filter(between(profit, quantile(profit, 0.00), quantile(profit, 0.95))) -> data_new

year <- rep(1:833, each=12)
year <- append(year, c(2,6,9,12))
set.seed(12345)
year <- sample(year)

data$year <- year

data_group = data %>% group_by(year) %>%
  summarise(profit = sum(profit)*0.3757,
            n = n())

agg_table <- data.frame(Ave_Profit=mean(data_group$profit), SD_Profit=sd(data_group$profit))
agg_table


#plot

library(readxl)
data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(1:16),] #1:16
data_plot <- data_plot[c(17:44),] #1:16

plot_sub <- subset(data_plot, Group1=="C=0, M=0" | Group1=="C=1, M=0" | Group1=="C=2, M=0")
plot_sub <- subset(data_plot, Group1=="C=0, M=0" | Group1=="C=1, M=0" | Group1=="C=2, M=0" | Group1=="C=3, M=0" | 
                     Group1=="C=5, M=0" | Group1=="C=6, M=0")

ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=Group1, alpha=Opaque)) +
  geom_point() + geom_text(hjust=0, vjust=0) + coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                                                               xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+100000)) + 
  geom_line(data = plot_sub) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  #geom_text(position=position_jitter(width=1,height=1)) +
  theme(axis.text=element_text(size=50), axis.title=element_text(size=50), legend.position="none") + theme_minimal()

#sensitivity analysis

data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(1:3, 45, 46, 64, 65),] #1:16

plot_sub <- data_plot[c(1:3),]
plot_sub2 <- data_plot[c(1,4,5),]
plot_sub3 <- subset(data_plot, Group=="C=0, M=0" | Group=="C=1, M=0" & Group2 == "SG_25" | Group=="C=2, M=0" & Group2 == "SG_25")


ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 (Base model & \n $25 base electricity)",
                                                             "C=1, M=0", "C=2, M=0 (Lower conversion rate)", " ", " "))) +
  geom_point() + geom_text(hjust=0, vjust=0) + coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                                                               xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+130000)) + 
  geom_line(data = plot_sub, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 (Base case + \n $25 base electricity)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_line(data = plot_sub2, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 (Base case + \n $25 base electricity)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  geom_line(data = plot_sub3, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 (Base case + \n $25 base electricity)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  theme(axis.text=element_text(size=75), axis.title=element_text(size=75)) + theme_minimal()



data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(1, 7, 8, 54, 55),]

plot_sub <- data_plot[c(1, 2, 3),]
plot_sub2 <- data_plot[c(1,4,5),]


ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=1", "C=2, M=1 (Base model & \n lower marginal cost)", " ",
                                                             " "))) +
  geom_point() + geom_text(hjust=0, vjust=0) + coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                                                               xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+100000)) + 
  geom_line(data = plot_sub, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=1", "C=2, M=1 (Base model & \n lower marginal cost)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_line(data = plot_sub2, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=1", "C=2, M=1 (Base model & \n lower marginal cost)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  theme(axis.text=element_text(size=50), axis.title=element_text(size=50)) + theme_minimal()




data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(1:3, 7, 8, 45, 46, 54, 55, 64, 65),] #1:16

plot_sub <- data_plot[c(1:3),]
plot_sub2 <- data_plot[c(1,6,7),]
plot_sub3 <- data_plot[c(1,8,9),]
plot_sub4 <- data_plot[c(1,10,11),]
plot_sub5 <- data_plot[c(1,4,5),]

ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 (Base model & \n $25 base electricity)",
                                                             "C=1, M=1", "C=2, M=1 (Base model & \n lower marginal cost)",
                                                             "C=1, M=0", "C=2, M=0 (Lower conversion rate)", " ", " ", " ", " "))) +
  geom_point() + geom_text(hjust=0, vjust=0) + coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                                                               xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+130000)) + 
  geom_line(data = plot_sub, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 (Base case + \n $25 base electricity)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_line(data = plot_sub2, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 (Base case + \n $25 base electricity)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  geom_line(data = plot_sub3, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 (Base case + \n $25 base electricity)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  geom_line(data = plot_sub4, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 (Base case + \n $25 base electricity)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  geom_line(data = plot_sub5, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 (Base case + \n $25 base electricity)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  theme(axis.text=element_text(size=75), axis.title=element_text(size=75)) + theme_minimal()


#medium gin

data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(17:23, 49:52, 67:72),] #1:16

plot_sub <- data_plot[c(1:7),] #base
plot_sub2 <- data_plot[c(1, 8:11),] #low conversion
plot_sub3 <- data_plot[c(1, 12:17),] #base price 25

ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0", "C=3, M=0", "C=4, M=0", "C=5, M=0", "C=6, M=0 (Base model & \n $25 base electricity)",
                                                             "C=2, M=0", "C=3, M=0", "C=4, M=0", "C=5, M=0 (Lower conversion rate)", 
                                                             " ", " ", " ", " ", " ", " "))) +
  geom_point() + geom_text(hjust=0, vjust=0) + coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                                                               xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+160000)) + 
  geom_line(data = plot_sub, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0", "C=3, M=0", "C=4, M=0", "C=5, M=0", "C=6, M=0 (Base model & \n $25 base electricity)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_line(data = plot_sub2, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=2, M=0", "C=3, M=0", "C=4, M=0", "C=5, M=0 (Lower conversion rate)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  geom_line(data = plot_sub3, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c(" ", " ", " ", " ", " ", " ", " "))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  theme(axis.text=element_text(size=100), axis.title=element_text(size=100)) + theme_minimal()



data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(1, 27:31, 57:61),] #1:16

plot_sub <- data_plot[c(1:6),] #base
plot_sub2 <- data_plot[c(1, 7:11),] #low conversion

ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=1", "C=2, M=1", "C=3, M=1", "C=4, M=1", "C=5, M=1 (Base model & \n lower marginal cost)",
                                                             " ", " ", " ", " ", " "))) +
  geom_point() + geom_text(hjust=0, vjust=0) + coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                                                               xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+175000)) + 
  geom_line(data = plot_sub, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=1", "C=2, M=1", "C=3, M=1", "C=4, M=1", "C=5, M=1 (Base model & \n lower marginal cost)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_line(data = plot_sub2, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c(" ", " ", " ", " ", " ", " "))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  theme(axis.text=element_text(size=75), axis.title=element_text(size=75)) + theme_minimal()




data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(17:23, 27:31, 49:52, 57:61, 67:72),] #1:16

plot_sub <- data_plot[c(1:7),] #base
plot_sub2 <- data_plot[c(1, 8:12),] #base w/ m=1
plot_sub3 <- data_plot[c(1, 13:16),] #low conversion
plot_sub4 <- data_plot[c(1, 17:21),] #marginal cost w/ m=1
plot_sub5 <- data_plot[c(1, 22:27),] #base price 25

ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0", "C=3, M=0", "C=4, M=0", "C=5, M=0", "C=6, M=0 (Base model & \n $25 base electricity)",
                                                             "C=1, M=1", "C=2, M=1", "C=3, M=1", "C=4, M=1", "C=5, M=1 (Base model & \n lower marginal cost)",
                                                             "C=2, M=0", "C=3, M=0", "C=4, M=0", "C=5, M=0 (Lower conversion rate)", 
                                                             " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "))) +
  geom_point() + geom_text(hjust=0, vjust=0) + coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                                                               xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+130000)) + 
  geom_line(data = plot_sub, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0", "C=3, M=0", "C=4, M=0", "C=5, M=0", "C=6, M=0 (Base model & \n $25 base electricity)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_line(data = plot_sub2, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=1", "C=2, M=1", "C=3, M=1", "C=4, M=1", "C=5, M=1"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  geom_line(data = plot_sub3, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=2, M=0", "C=3, M=0", "C=4, M=0", "C=5, M=0 (Lower conversion rate)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  geom_line(data = plot_sub4, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c(" ", " ", " ", " ", " ", " "))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  geom_line(data = plot_sub5, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 (Base case + \n $25 base electricity)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  theme(axis.text=element_text(size=75), axis.title=element_text(size=75)) + theme_minimal()






data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(17:27, 48, 49, 50, 51, 52, 53),]

plot_sub <- subset(data_plot, Group=="C=0, M=0" | Group=="C=1, M=0" & Group2 =="Medium gin" | Group=="C=2, M=0" & Group2 == "Medium gin" |
                     Group=="C=2, M=1" & Group2 =="Medium gin" | Group=="C=3, M=0" & Group2 == "Medium gin" |  Group=="C=3, M=1" & Group2 =="Medium gin" | 
                     Group=="C=4, M=1" & Group2 == "Medium gin" | Group=="C=5, M=1" & Group2 == "Medium gin" | Group=="C=5, M=0" & Group2 == "Medium gin" |
                     Group=="C=6, M=0" & Group2 == "Medium gin")

plot_sub2 <- subset(data_plot, Group=="C=0, M=0" | Group=="C=2, M=0" & Group2 == "MG_Low" |
                      Group=="C=3, M=0" & Group2 =="MG_Low" | Group=="C=4, M=0" & Group2 == "MG_Low" |  Group=="C=5, M=0" & Group2 =="MG_Low")


ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=Group)) +
  geom_point() + geom_text(hjust=0, vjust=0) + coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                                                               xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+50000)) + 
  geom_line(data = plot_sub) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_line(data = plot_sub2, linetype = "dotted") + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
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


#plots

rm(list=ls())

data <- read.csv("gin.csv")

peak <- read.csv("peak_price.csv")
ubase <- read.csv("ubase_price.csv")
lbase <- read.csv("lbase_price.csv")

#peak price plot

peak1 = data %>% select(jan_peak, feb_peak, mar_peak, jun_peak, jul_peak, aug_peak, sep_peak, dec_peak)
peak2 <- rbind(peak_jan, peak_feb, peak_mar, peak_jun, peak_jul, peak_aug, peak_sep, peak_dec)

peak1 <- cbind(row = rownames(peak1), stack(peak1))

peak1 %>% filter(between(values, quantile(values, 0.05), quantile(values, 0.95))) -> peak1
peak2 %>% filter(between(price, quantile(price, 0.05), quantile(price, 0.95))) -> peak2

hist(peak1$values, probability=TRUE, col="gray", border="white", main="Distribution of peak electricity price", xlab="Peak electricity price ($)", 
     ylim=c(0,0.0132), cex.lab=1.5)
d <- density(peak2$price)
lines(d, col="red", lwd = 3)

abline(v = median(peak1$values), col = "black", lwd = 3, lty = 2)
text(median(peak1$values), max(density(peak1$values)[[2]])+0.0005,  paste("Simulated median =", 72.49),  pos = 4, srt = 0, cex = 1.5, col = "black") 
abline(v = median(peak2$price), col = "black", lwd = 3)
text(median(peak2$price), max(density(peak2$price)[[2]])-0.0005,  paste("Observed median =", 78.39),  pos = 4, srt = 0, cex = 1.5, col = "black")

#ubase price plot

ubase1 = data %>% select(jan_ubase, feb_ubase, mar_ubase, apr_ubase, may_ubase, jun_ubase, jul_ubase, aug_ubase, sep_ubase, dec_ubase)
ubase2 <- rbind(ubase_jan, ubase_feb, ubase_mar, ubase_apr, ubase_may, ubase_jun, ubase_jul, ubase_aug, ubase_sep, ubase_dec)

ubase1 <- cbind(row = rownames(ubase1), stack(ubase1))

ubase1 %>% filter(between(values, quantile(values, 0.025), quantile(values, 0.975))) -> ubase1
ubase2 %>% filter(between(price, quantile(price, 0.05), quantile(price, 0.95))) -> ubase2

hist(ubase1$values, probability=TRUE, col="gray", border="white", main="Distribution of sub-peak electricity price", xlab="Sub-peak electricity price ($)", 
     xlim=c(min(ubase2$price-10), max(ubase2$price+10)), cex.lab=1.5)
d <- density(ubase2$price)
lines(d, col="red", lwd = 3)

abline(v = median(ubase1$values), col = "black", lwd = 3, lty = 2)
text(median(ubase1$values), max(density(ubase1$values)[[2]])-0.00425,  paste("Simulated median =", 44.30),  pos = 4, srt = 0, cex = 1.5, col = "black") 
abline(v = median(ubase2$price), col = "black", lwd = 3)
text(median(ubase2$price), max(density(ubase2$price)[[2]]),  paste("Observed median =", 46.85),  pos = 4, srt = 0, cex = 1.5, col = "black") 

#lbase price plot

lbase1 = data %>% select(jan_lbase, feb_lbase, mar_lbase, apr_lbase, may_lbase, jun_lbase, jul_lbase, aug_lbase, sep_lbase, dec_lbase)
lbase2 <- rbind(lbase_jan, lbase_feb, lbase_mar, lbase_apr, lbase_may, lbase_jun, lbase_jul, lbase_aug, lbase_sep, lbase_dec)

lbase1 <- cbind(row = rownames(lbase1), stack(lbase1))

lbase1 %>% filter(between(values, quantile(values, 0.05), quantile(values, 0.95))) -> lbase1
lbase2 %>% filter(between(price, quantile(price, 0.05), quantile(price, 0.95))) -> lbase2

hist(lbase1$values, probability=TRUE, col="gray", border="white", main="Distribution of base electricity price", xlab="Base electricity price ($)",
     xlim=c(min(lbase2$price-10), max(lbase2$price+10)), ylim=c(0, 0.033), cex.lab=1.5)
d <- density(lbase2$price)
lines(d, col="red", lwd = 3)

abline(v = median(lbase1$values), col = "black", lwd = 3, lty = 2)
text(median(lbase1$values), max(density(lbase1$values)[[2]])-0.00425,  paste("Simulated median =", 42.05),  pos = 4, srt = 0, cex = 1.5, col = "black") 
abline(v = median(lbase2$price), col = "black", lwd = 3)
text(median(lbase2$price), max(density(lbase2$price)[[2]]),  paste("Observed median =", 41.04),  pos = 4, srt = 0, cex = 1.5, col = "black") 

#gin trash

gin1 <- read.csv("Ropes Coop Gin Trash 2004-2018.csv")
gin2 <- read.csv("gin_quant.csv")
hist(gin1$gin, breaks=10, probability=TRUE, col="gray", border="white", main="Distribution of Gin Trash", xlab="Gin Trash (tons)", xlim=c(0, max(gin2$gin)+4000), 
     ylim=c(0,0.000275), cex.lab=1.5)
d <- density(gin2$gin)
lines(d, col="red", lwd = 3)
abline(v = median(gin1$gin), col = "black", lwd = 3)
text(median(gin1$gin), max(density(gin1$gin)[[2]])+0.00003,  paste("Observed median =", 8740),  pos = 4, srt = 0, cex = 1.50, col = "black") 
abline(v = median(gin2$gin), col = "black", lwd = 3, lty = 2)
text(median(gin2$gin), max(density(gin2$gin)[[2]]),  paste("Simulated median =", 9763),  pos = 4, srt = 0, cex = 1.50, col = "black") 


#ammonia price

amm1 = data %>% select(winter_crop, summer_crop, roy_crop)
amm2 <- rbind(crop_winter, crop_summer, crop_roy)

amm1 <- cbind(row = rownames(amm1), stack(amm1))

amm1 %>% filter(between(values, quantile(values, 0.05), quantile(values, 0.95))) -> amm1
amm2 %>% filter(between(ANHYD, quantile(ANHYD, 0.05), quantile(ANHYD, 0.95))) -> amm2

hist(amm1$values, breaks=10, probability=TRUE, col="gray", border="white", main="Distribution of Ammonia price", xlab="Ammonia price ($)", 
     cex.lab=1.5, ylim=c(0, 0.010), xlim=c(min(amm2$ANHYD-50), max(amm2$ANHYD+50)))
d <- density(amm2$ANHYD)
lines(d, col="red", lwd = 3)
abline(v = median(amm1$values), col = "black", lwd = 3, lty = 2)
#text(median(amm1$values), max(density(amm1$values)[[2]]),  paste("Simulated median =", 507),  pos = 4, srt = 0, cex = 1.50, col = "black") 

text(median(amm1$values), max(density(amm1$values)[[2]])-0.00425,  paste("Simulated median =", 507),  pos = 4, srt = 0, cex = 1.5, col = "black")

abline(v = median(amm2$ANHYD), col = "black", lwd = 3)
text(median(amm2$ANHYD), max(density(amm2$ANHYD)[[2]]),  paste("Observed median =", 537),  pos = 4, srt = 0, cex = 1.50, col = "black")


