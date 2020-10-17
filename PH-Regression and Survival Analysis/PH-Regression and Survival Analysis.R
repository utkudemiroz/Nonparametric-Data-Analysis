rm(list=ls())

library(boot)
library(survival)
library(ggplot2)
library(survminer)
library(rlang)
library(km.ci)

data('bladder')
names=c('placebo','thiotepa')
barplot(prop.table(table(bladder$rx)), names.arg = names)

plot(bladder$size,bladder$stop,
     xlab='Size (cm) of largest initial tumour',
     ylab='Recurrence or Censoring Time',
     main='Effect of the largest initial tumour on Recurrence Time')


#i) Bootstrap
B = 3000
median <- c()
beta <- c()
for (n in 1:B) {
  #Sampling with replacement
    current_data = bladder[sample(nrow(bladder),50, replace = TRUE),]
  
    
  KM_fit <- survfit(Surv(stop) ~ 1, data = current_data)
  current_median <- unname(quantile(KM_fit,probs = 0.5, names = FALSE)$quantile)
  median <- c(median,current_median)
  #PH-Regression
  
  cox1 <- coxph(Surv(stop) ~ rx + number + size + enum, data = current_data)
  #cox1 is the regression formed by bootstrapping
  PHsummary <- summary(cox1)
  beta = c(beta,PHsummary$coefficients[1])
}

fit=survfit(Surv(stop)~ rx, data=bladder, conf.int=TRUE)
plot(fit,col=c(1:2), xlab="Recurrence or Censoring Time",
     ylab="Proportion Surviving",
     main="Survival Function of Placebo vs Thiotepa Patients")
legend("bottomleft", legend=names, cex=0.6, lty=1, col=1:2, text.col=1:2)


#Cox1 is build as the survival analysis
plot(survfit(cox1),xlab='Recurrence or Censoring Time',ylab='Survival Rate',
     conf.int=TRUE,
     main="Recurrence Time vs Survival Rate")

#Cox 2 model is used to split the data into two for analysis and will result in 
#two survival curves
cox2 = coxph(Surv(stop) ~ strata(rx) + number + size + enum, data = bladder)
summary(cox2)
plot(survfit(cox2), xlab='Recurrence or Censoring Time', 
     ylab='Survival Rate', 
     conf.int=TRUE, 
     col=1:2,
     main="Recurrence Time vs Survival Rate for Placebo and Thiotepa")
legend("bottomleft", legend=names, cex=0.6, lty=1, col=1:2, text.col=1:2)

cox.zph(cox1)
cox.zph(cox2)


#median of km
res.km <- survfit(Surv(stop) ~ 1, data = bladder)
kmCI <- km.ci(res.km, conf.level=0.9,tl=NA, tu=NA, method="rothman")
kmCI

res.ph <- coxph(Surv(stop) ~ rx, data = bladder)
summary(res.ph, conf.int=0.9)
beta_lower <- summary(res.ph, conf.int=0.9)$conf.int[3]
beta_higher <- summary(res.ph, conf.int=0.9)$conf.int[4]
beta_lower <- log(beta_lower)
beta_higher <- log(beta_higher)

#Median Analysis
#histogram
hist(median)
#normality
goftest::ad.test(median)
qqnorm(median)
qqline(median, col = "red")

#Beta Analysis
#histogram
hist(beta)
#normality
goftest::ad.test(beta)
qqnorm(beta)
qqline(beta, col = "red")

