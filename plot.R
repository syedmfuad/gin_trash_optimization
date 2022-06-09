#plot

library(readxl)
data_plot <- read_excel("Summary.xlsx")

############## BASE MODELS ##############

#plot EV frontier for small gin

data_plot <- data_plot[c(1:16),] 

plot_sub <- subset(data_plot, Group1=="C=0, M=0" | Group1=="C=1, M=0" | Group1=="C=2, M=0")

ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=Group1, alpha=Opaque)) +
  geom_point() + geom_text(hjust=0, vjust=0, position = position_nudge(x = 10000, y=-5000)) +
  coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                  xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+100000)) + 
  geom_line(data = plot_sub) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_segment(aes(x= 494065 ,xend=702822,y=369700,yend=369700), linetype = "dotted") +
  theme(axis.text=element_text(size=50), axis.title=element_text(size=50), legend.position="none") + theme_minimal()

#plot EV frontier for medium gin

data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(17:44),] #1:16
plot_sub <- subset(data_plot, Group1=="C=0, M=0" | Group1=="C=1, M=0" | Group1=="C=2, M=0" | Group1=="C=3, M=0" | Group1=="C=4, M=0" | 
                     Group1=="C=5, M=0" | Group1=="C=6, M=0")

ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=Group1, alpha=Opaque)) +
  geom_point() + geom_text(hjust=0, vjust=0, position = position_nudge(x = ifelse(data_plot$SD ==   1391770 , -300000, 20000), y=-10000)) + 
  coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                  xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+100000)) + 
  geom_line(data = plot_sub) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_segment(aes(x= 1482193 ,xend=2000000,y= 1237195 ,yend=1237195), linetype = "dotted") +
  theme(axis.text=element_text(size=50), axis.title=element_text(size=50), legend.position="none") + theme_minimal()

#plot EV frontier with fixed cost for operator

data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(1, 74:88),]

plot_sub <- subset(data_plot, Group1=="C=0, M=0" | Group1=="C=1, M=0" | Group1=="C=2, M=0")

ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=Group1, alpha=Opaque)) +
  geom_point() + geom_text(hjust=0, vjust=0, position = position_nudge(x = 10000, y=-5000)) +
  coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                  xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+100000)) + 
  geom_line(data = plot_sub) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_segment(aes(x= 494065 ,xend=702822,y=294700.1,yend=294700.1), linetype = "dotted") +
  theme(axis.text=element_text(size=50), axis.title=element_text(size=50), legend.position="none") + theme_minimal()

############## SENSITIVITY ANALYSIS ##############

#plot EV frontier for small gin (base case and lower biomass conversion rate)

data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(1:3, 45, 46),]

plot_sub <- data_plot[c(1:3),]
plot_sub2 <- data_plot[c(1,4,5),]

ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 \n (Base model)",
                                                             "C=1, M=0", "C=2, M=0 (Lower conversion rate)"))) +
  geom_point() + geom_text(hjust=0, vjust=0, position = position_nudge(x = 10000, y=-5000)) + 
  coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                  xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+130000)) + 
  geom_line(data = plot_sub, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 (Base case)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_line(data = plot_sub2, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 (Base case)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  theme(axis.text=element_text(size=75), axis.title=element_text(size=75)) + theme_minimal()

#plot EV frontier for small gin (base case and lower base electricity price)

data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(1:3, 64, 65),]

plot_sub <- data_plot[c(1:3),]
plot_sub2 <- data_plot[c(1,4,5),]

ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 \n (Base model & \n $25 base electricity)",
                                                             " ", " "))) +
  geom_point() + geom_text(hjust=0, vjust=0, position = position_nudge(x = 10000, y=-5000)) + 
  coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                  xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+130000)) + 
  geom_line(data = plot_sub, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 (Base case)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_line(data = plot_sub2, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0 (Base case)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  theme(axis.text=element_text(size=75), axis.title=element_text(size=75)) + theme_minimal()

#plot EV frontier for small gin (base case and lower marginal cost of ammonia production)

data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(1, 7, 8, 54, 55),]

plot_sub <- data_plot[c(1, 2, 3),]
plot_sub2 <- data_plot[c(1,4,5),]

ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=1", "C=2, M=1 \n (Base model & \n lower marginal cost)", " ", " "))) +
  geom_point() + geom_text(hjust=0, vjust=0, position = position_nudge(x = 10000, y=-5000))  + 
  coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                  xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+100000)) + 
  geom_line(data = plot_sub, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=1", "C=2, M=1 (Base model & \n lower marginal cost)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_line(data = plot_sub2, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=1", "C=2, M=1 (Base model & \n lower marginal cost)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  theme(axis.text=element_text(size=50), axis.title=element_text(size=50)) + theme_minimal()

#plot EV frontier for small gin (base case and all sensitivity analyses)

data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(1:3, 7, 8, 45, 46, 54, 55, 64, 65),]

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

#plot EV frontier for medium gin (base case and lower biomass conversion rate)

data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(17:23, 49:52),]

plot_sub <- data_plot[c(1:7),] #base
plot_sub2 <- data_plot[c(1, 8:11),] #low conversion

ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0", "C=3, M=0", "C=4, M=0", "C=5, M=0", "C=6, M=0 \n (Base model)",
                                                             "C=2, M=0", "C=3, M=0", "C=4, M=0", "C=5, M=0 (Lower conversion rate)"))) +
  geom_point() + geom_text(hjust=0, vjust=0, position = position_nudge(x = ifelse(data_plot$SD ==   1391770 , -225000, 20000), y=-10000)) + 
  coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+50000), 
                  xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+160000)) + 
  geom_line(data = plot_sub, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0", "C=3, M=0", "C=4, M=0", "C=5, M=0", "C=6, M=0 (Base model & \n $25 base electricity)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_line(data = plot_sub2, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=2, M=0", "C=3, M=0", "C=4, M=0", "C=5, M=0 (Lower conversion rate)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  theme(axis.text=element_text(size=100), axis.title=element_text(size=100)) + theme_minimal()

#plot EV frontier for medium gin (base case and lower base electricity price)

data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(17:23, 67:72),] #1:16

plot_sub <- data_plot[c(1:7),] #base
plot_sub2 <- data_plot[c(1, 8:13),] #base price 25

ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0", "C=3, M=0", "C=4, M=0", "C=5, M=0", "C=6, M=0 \n (Base model & \n $25 base electricity)",
                                                             " ", " ", " ", " ", " ", " "))) +
  geom_point() + geom_text(hjust=0, vjust=0, position = position_nudge(x = ifelse(data_plot$SD ==   1391770 , -225000, 20000), y=-10000)) + 
  coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+75000), 
                  xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+250000)) + 
  geom_line(data = plot_sub, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0", "C=3, M=0", "C=4, M=0", "C=5, M=0", "C=6, M=0 \n (Base model & \n $25 base electricity)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_line(data = plot_sub2, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=0", "C=2, M=0", "C=3, M=0", "C=4, M=0 (Lower conversion rate)", " ", " "))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  theme(axis.text=element_text(size=100), axis.title=element_text(size=100)) + theme_minimal()

#plot EV frontier for medium gin (base case and lower marginal cost of ammonia production)

data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(17:23, 57:62),]

plot_sub <- data_plot[c(1:7),] #base
plot_sub2 <- data_plot[c(1, 8:13),] #low marginal cost

ggplot(data = data_plot, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=1", "C=2, M=1", "C=3, M=1", "C=4, M=1", "C=5, M=1", "C=6, M=1 \n (Base model & \n lower marginal cost)",
                                                             " ", " ", " ", " ", " ", " "))) +
  geom_point() + geom_text(hjust=0, vjust=0, position = position_nudge(x = ifelse(data_plot$SD ==   1391770 , -225000, 20000), y=-10000)) + 
  coord_cartesian(ylim = c(min(data_plot$Avg_profit)-50000, max(data_plot$Avg_profit)+75000), 
                  xlim = c(min(data_plot$SD)-50000, max(data_plot$SD)+250000)) + 
  geom_line(data = plot_sub, aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=1", "C=2, M=1", "C=3, M=1", "C=4, M=1", "C=5, M=1", "C=6, M=1 \n (Base model & \n $25 base electricity)"))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") + 
  geom_line(data = plot_sub2, linetype = "dotted", aes(x = SD, y = Avg_profit, label=c("C=0, M=0", "C=1, M=1", "C=2, M=1", "C=3, M=1", "C=4, M=1 (Lower conversion rate)", " ", " "))) + xlab("Standard deviation of profit") + ylab("Mean annualized profit ($)") +
  theme(axis.text=element_text(size=100), axis.title=element_text(size=100)) + theme_minimal()

#plot EV frontier for medium gin (base case and all sensitivity analyses)

data_plot <- read_excel("Summary.xlsx")
data_plot <- data_plot[c(17:23, 27:31, 49:52, 57:61, 67:72),]

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

############## COMPARISON OF DISTRIBUTIONS ##############

rm(list=ls())

data <- read.csv("gin.csv")

#peak price distribution

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

#sub-peak price distribution

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

#base price distribution

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

#gin trash distribution

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

#ammonia price distribution

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

