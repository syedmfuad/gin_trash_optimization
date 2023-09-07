setwd("C:/Users/syedm/Desktop/Bioenergy")

data <- read.csv("gin.csv")


data$Jan <- rowMeans(data[,c("jan_peak", "jan_ubase", "jan_lbase")], na.rm=TRUE)
data$Feb <- rowMeans(data[,c("feb_peak", "feb_ubase", "feb_lbase")], na.rm=TRUE)
data$Mar <- rowMeans(data[,c("mar_peak", "mar_ubase", "mar_lbase")], na.rm=TRUE)
data$Apr <- rowMeans(data[,c("apr_ubase", "apr_lbase")], na.rm=TRUE)
data$May <- rowMeans(data[,c("may_ubase", "may_lbase")], na.rm=TRUE)
data$Jun <- rowMeans(data[,c("jun_peak", "jun_ubase", "jun_lbase")], na.rm=TRUE)
data$Jul <- rowMeans(data[,c("jul_peak", "jul_ubase", "jul_lbase")], na.rm=TRUE)
data$Aug <- rowMeans(data[,c("aug_peak", "aug_ubase", "aug_lbase")], na.rm=TRUE)
data$Sep <- rowMeans(data[,c("sep_peak", "sep_ubase", "sep_lbase")], na.rm=TRUE)



data$Jan <- data$jan_peak
data$Feb <- data$feb_peak
data$Mar <- data$mar_peak
data$Apr <- data$apr_ubase
data$May <- data$may_ubase
data$Jun <- data$jun_peak
data$Jul <- data$jul_peak
data$Aug <- data$aug_peak
data$Sep <- data$sep_peak



new_df = subset(data, select = c(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep))

library(reshape)
library(tidyr)
library(dplyr)
meltData <- melt(new_df)
boxplot(data=meltData, value~variable)


means <- aggregate(value ~ variable, meltData, mean)


library(ggplot2)
p <- ggplot(meltData, aes(factor(variable), value)) 
p + geom_boxplot(outlier.shape = NA, coef = 0) + 
  scale_y_continuous(limits = quantile(meltData$value, c(0.1, 0.97))) + 
  geom_point(data=means, aes(y=value))





df <- data.frame (month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"),
                  prod = c(178.0283, 184.5256, 196.1748, 181.2413, 189.922, 
                           196.3127, 202.3672, 192.5513, 187.5426), 
                  shape = c("star", "dot", "dot", "dot", "dot", "dot", "dot", "dot", "dot"),
                  label = c("E[i]", "E[i+1]~'|'~mu[P~i+1]", "E[i+2]~'|'~mu[P~i+2]", "E[i+3]~'|'~mu[P~i+3]", 
                            "E[i+4]~'|'~mu[P~i+4]", "E[i+5]~'|'~mu[P~i+5]", "E[i+6]~'|'~mu[P~i+6]", 
                            "E[i+7]~'|'~mu[P~i+7]", "E[i+8]~'|'~mu[P~i+8]"))



df %>% mutate(month =ordered(month, levels = unique(month))) %>% 
  ggplot(aes(x=month, y=prod, color = factor(shape), shape = factor(shape))) + geom_point(size = 3) + 
  ylim(170, 210) +  
  scale_shape_manual(values = c(16, 1)) + 
  scale_color_manual(values = c("star" = "black",
                                "dot" = "black")) -> plot 

plot + theme(legend.position = "none") + xlab("Month") + ylab("Electricity production (MWh)") + 
  geom_segment(aes(xend=month), yend=0, linetype="dotted") +
  expand_limits(y=0) + theme(text = element_text(size=20)) +
  geom_text(aes(label = label, hjust=0.50, vjust=-0.5), size=5, parse=TRUE) + 
  geom_point(aes(x="Jan", y=178.0283), colour="red", size=3, shape=16) + 
  scale_x_discrete(expand = c(0, 1)) + theme_classic() + 
  theme(legend.position = "none") + 
  theme(text = element_text(size=20))


plot + geom_text(x = "Jul", y = 225, 
                 label = expression(Sigma[i == Jan]^Sept ~ X), size=5) + theme(legend.position = "none") + 
  theme(legend.position = "none") + xlab("Month") + ylab("Electricity production (MWh)") + 
  geom_segment(aes(xend=month), yend=0, linetype="dotted") +
  expand_limits(y=0) + theme(text = element_text(size=20))


"C[t] ~ (Given ~ CGW-C[t-1])"

df2 <- data.frame (month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"),
                   prod = c(178.0283, 184.5256, 196.1748, 181.2413, 189.922, 
                            196.3127, 202.3672, 192.5513, 187.5426), 
                   shape = c("star", "star", "dot", "dot", "dot", "dot", "dot", "dot", "dot"),
                   label = c("E[i-1]", "E[i]", "E[i+1]~'|'~mu[P~i+1]", "E[i+2]~'|'~mu[P~i+2]", "E[i+3]~'|'~mu[P~i+3]", 
                             "E[i+4]~'|'~mu[P~i+4]", "E[i+5]~'|'~mu[P~i+5]", "E[i+6]~'|'~mu[P~i+6]", 
                             "E[i+7]~'|'~mu[P~i+7]"))


df2 %>% mutate(month =ordered(month, levels = unique(month))) %>% 
  ggplot(aes(x=month, y=prod, shape = factor(shape))) + geom_point(size = 3) + ylim(170, 210) +
  scale_shape_manual(values = c(16, 1)) -> plot2 

plot2 + theme(legend.position = "none") + xlab("Month") + ylab("Electricity production (MWh)") + 
  geom_segment(aes(xend=month), yend=0, linetype="dotted") +
  expand_limits(y=0) +
  geom_text(aes(label = label, hjust=0.5, vjust=-0.50), size=5, parse=TRUE) + 
  geom_point(aes(x="Jan", y=178.0283), colour="red", size=3, shape=16) + 
  geom_point(aes(x="Feb", y=184.5256), colour="red", size=3, shape=16) + 
  scale_x_discrete(expand = c(0, 1)) + theme_classic() + 
  theme(legend.position = "none") + 
  theme(text = element_text(size=20))





plot2 + geom_text(x = "Jul", y = 225, 
                  label = expression(Sigma[i == Feb]^Sept ~ X - Jan[CGW]), size=5) + theme(legend.position = "none") + 
  xlab("Month") + ylab("Electricity production (MWh)") + 
  geom_segment(aes(xend=month), yend=0, linetype="dotted") +
  expand_limits(y=0) + theme(text = element_text(size=20)) 



q = c(.25, .5, .75)

meltData %>%
  group_by(variable)%>% 
  summarise(Mean=mean(value), Median=median(value), q1 = quantile(value, probs = q[1]),
            q3 = quantile(value, probs = q[3]), sd=sd(value)) -> df

means <- aggregate(value ~ variable, meltData, mean)
means$value[1] <- 61.25
means$value[8] <- 102.25
means$value[9] <- 88.36

ggplot(df, aes(factor(variable), Median)) +        # ggplot2 plot with confidence intervals
  geom_point(size=3) +
  geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.75, size = 1) +
  geom_point(data=means, shape=4, size=5, aes(y=value)) + 
  xlab("Month") + ylab("Electricity price ($/MWh)") + 
  theme(text = element_text(size=20)) + 
  geom_point(aes(x="Jan", y=50), colour="red", size=3) + 
  geom_text(aes(x = "Jan", y = 50,
                label = "Obs.P[i]", hjust=-0.05, vjust=0.50, size=8), parse=TRUE) + theme(legend.position = "none")



q = c(.25, .5, .75)

meltData %>%
  group_by(variable)%>% 
  summarise(Mean=mean(value), Median=median(value), q1 = quantile(value, probs = q[1]),
            q3 = quantile(value, probs = q[3]), sd=sd(value)) -> df

df$sd[8] <- 566
df$sd[9] <- 764
df$sd[4] <- 128.4
df$sd[5] <- 118.3

df$upper <- df$Mean+df$sd
df$lower <- df$Mean-df$sd

df$label <- c("mu[P~i]", "mu[P~i+1]", "mu[P~i+2]", "mu[P~i+3]", 
              "mu[P~i+4]", "mu[P~i+5]", "mu[P~i+6]", 
              "mu[P~i+7]", "mu[P~i+8]")

ggplot(df, aes(factor(variable), Mean)) +        # ggplot2 plot with confidence intervals
  geom_point(size=3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.75, size = 1) +
  #geom_point(data=means, shape=16, size=3, aes(y=value)) + 
  xlab("Month") + ylab("Electricity price ($/MWh)") + 
  geom_point(aes(x="Jan", y=150), colour="red", size=3) + 
  geom_text(aes(x = "Jan", y = 150,
                label = "Obs.P[i]", hjust=-0.15, vjust=0.50, size=6), parse=TRUE) + 
  theme_classic() + 
  theme(legend.position = "none") +
  theme(text = element_text(size=20)) +
  geom_text(aes(label = label, hjust=-0.15, vjust=1.00), size=5, parse=TRUE) + 
  scale_x_discrete(expand = c(0, 1))  



#gin trash distribution

gin1 <- read.csv("Ropes Coop Gin Trash 2004-2018.csv")
gin2 <- read.csv("gin_quant.csv")
hist(gin1$gin, breaks=10, probability=TRUE, col="gray", border="white", main=NULL, xlab="Cotton Gin Waste (tons)", xlim=c(0, max(gin2$gin)+4000), 
     ylim=c(0,0.000275), cex.lab=1.5)
d <- density(gin2$gin)
lines(d, col="red", lwd = 3)
#abline(v = median(gin1$gin), col = "black", lwd = 3)
#text(median(gin1$gin), max(density(gin1$gin)[[2]])+0.00003,  paste("Observed median =", 8740),  pos = 4, srt = 0, cex = 1.50, col = "black") 
#abline(v = median(gin2$gin), col = "black", lwd = 3, lty = 2)
#text(median(gin2$gin), max(density(gin2$gin)[[2]]),  paste("Simulated median =", 9763),  pos = 4, srt = 0, cex = 1.50, col = "black")



