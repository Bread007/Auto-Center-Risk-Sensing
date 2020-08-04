
##install.packages("ggplot2")
##library(ggplot2)
setwd("C:/Users/t0d00dh/01_Projects/ACC Risk Sensing")
df1 = read.csv("Claim_Veh_Cha.csv", header=T, as.is=T)
df2 = read.csv("Legal_Veh_Cha.csv", header=T, as.is=T) 


### Plot 1
clm_mil = as.numeric(df1$Mileage[which(df1$Mileage !="Unknown")])
clm_mil = clm_mil[which(clm_mil <=200000)]
clm_mil = clm_mil[which(clm_mil >=1000)]

mean_clm_mil = mean(clm_mil)
std_clm_mil = sd(clm_mil)

hist(clm_mil,,breaks=20,freq = FALSE,ann = FALSE)
title(main = "Density of Vehicle Mileage(Claims)",
      xlab = "Mileage",
      ylab = "Probability")
lines(density(clm_mil),lwd = 3,col = "blue")

abline(v = mean_clm_mil, col="red", lwd=2, lty=2)
text(mean_clm_mil,0.0000055, "111k mile", col = 2, adj = c(-.1, -.1))
abline(v = mean_clm_mil+std_clm_mil, col="red", lwd=2, lty=2)
text(mean_clm_mil+std_clm_mil,0.000006, "161k mile", col = 2, adj = c(-.1, -.1))
abline(v = mean_clm_mil-std_clm_mil, col="red", lwd=2, lty=2)
text(mean_clm_mil-std_clm_mil,0.000006, "60.7k mile", col = 2, adj = c(-.1, -.1))


### Plot 2
clm_age = as.numeric(df1$Car.Age[which(df1$Car.Age !="Unknown")])

clm_age_mean = mean(clm_age)
clm_age_sd  = sd(clm_age)
hist(clm_age,,breaks=20,freq = FALSE,ann = FALSE)
title(main = "Density of Vehicle Age(Claims)",
      xlab = "Age",
      ylab = "Probability")
lines(density(clm_age),lwd = 3,col = "blue")


abline(v = clm_age_mean, col="red", lwd=2, lty=2)
text(clm_age_mean,0.06, "11.5 Year" ,col = 2, adj = c(-.1, -.1))

abline(v = clm_age_mean + clm_age_sd, col="red", lwd=2, lty=2)
text(clm_age_mean + clm_age_sd,0.05, "15.6 Year", col = 2, adj = c(-.1, -.1))

abline(v = clm_age_mean - clm_age_sd, col="red", lwd=2, lty=2)
text(clm_age_mean - clm_age_sd,0.05, "4.2 Year", col = 2, adj = c(-.1, -.1))

#### Plot3 
leg_mil = as.numeric(df2$Mileage[which(df2$Mileage!="Unknown")])
leg_mil_mean = mean(leg_mil)
leg_mil_sd = sd(leg_mil)
hist(leg_mil,breaks=20,freq = FALSE,ann = FALSE)
title(main = "Density of Vehicle Mileage(Lawsuits)",
      xlab = "Mileage",
      ylab = "Probability")
lines(density(leg_mil),lwd = 2,col = "blue")


abline(v = leg_mil_mean, col="red", lwd=2, lty=2)
text(leg_mil_mean,0.000008, "133k miles(Avg)" ,col = 2, adj = c(-.1, -.1))
abline(v = leg_mil_mean - leg_mil_sd, col="red", lwd=2, lty=2) 
text(50000,0.000005, "71.2k miles", col = 2, adj = c(-.1, -.1))
abline(v = leg_mil_mean + leg_mil_sd, col="red", lwd=2, lty=2) 
text(leg_mil_mean + leg_mil_sd,0.000005, "194.7k miles", col = 2, adj = c(-.1, -.1))

 

#### Plot4
leg_age = as.numeric(df2$Veh_Age[which(df2$Veh_Age!="Unknown")])
leg_age_mean = mean(leg_age)
leg_age_sd = sd(leg_age)


hist(leg_age,breaks=20,freq = FALSE,ann = FALSE)
title(main = "Density of Vehicle Age(Lawsuits)",
      xlab = "Age",
      ylab = "Probability")
lines(density(leg_age),lwd = 3,col = "blue")

abline(v = leg_age_mean, col="red", lwd=2, lty=2)
text(leg_age_mean,0.07, "11.5 Year" ,col = 2, adj = c(-.1, -.1))
abline(v = leg_age_mean - leg_age_sd, col="red", lwd=2, lty=2) 
text(leg_age_mean - leg_age_sd,0.05, "6.3 Year", col = 2, adj = c(-.1, -.1))
abline(v = leg_age_mean + leg_age_sd, col="red", lwd=2, lty=2) 
text(leg_age_mean + leg_age_sd,0.05, "16.8 Year", col = 2, adj = c(-.1, -.1))




"""
df01 = as.data.frame(clm_mil)
ggplot(data = df01,aes(df01$clm_mil))+
  geom_histogram(aes(y=..density..),
                 breaks  = seq(1000,200000,by = 5000),
                 col = "black",
                 alpha = 0.5)+
  geom_density(col = 4)"""
