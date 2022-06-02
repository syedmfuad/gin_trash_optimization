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