rm(list = ls())
require(magrittr)
require(ggplot2)
df=read.csv(".../Walmart_Store_sales.csv", header=TRUE)
summary(df)
df$Store <- as.factor(df$Store)
df$Holiday_Flag <- as.factor(df$Holiday_Flag)

wkly_sales.str1 <- df[df$Store %in% 1,]$Weekly_Sales
wkly_sales.str2 <- df[df$Store %in% 2,]$Weekly_Sales

selected=c(1,2)
data2=df[df$Store %in% selected,]

summary(wkly_sales.str1)
summary(wkly_sales.str2)

mean(wkly_sales.str1)
mean(wkly_sales.str2)
sd(wkly_sales.str1)
sd(wkly_sales.str2)

#Histogram For Store 1 and 2 Weekly Sales
hist(wkly_sales.str1, main="Weekly Sales for Store 1", xlab="Weekly Sales")
hist(wkly_sales.str2, main="Weekly Sales for Store 2", xlab="Weekly Sales")

#QQ-Norm Plots for Store 1
qqnorm(wkly_sales.str1, pch = 1, frame = FALSE, main="QQ Plot for Store 1")
qqline(wkly_sales.str1, lwd = 2)

#QQ-Norm Plots for Store 2
qqnorm(wkly_sales.str2, pch = 1,frame = FALSE, main="QQ Plot for Store 2")
qqline(wkly_sales.str2, lwd = 2)

#Comparison between Store 1 and Store 2
plot(wkly_sales.str1, wkly_sales.str2, main="Weekly Sales", 
     xlab="Store 1", ylab="Store 2", pch=19)

#Correlation measures
pearson = cor(wkly_sales.str1, wkly_sales.str2, method = c('pearson'))
spearman = cor(wkly_sales.str1, wkly_sales.str2, method = c('spearman')) 
kendall = cor(wkly_sales.str1, wkly_sales.str2, method = c('kendall'))

#Bootstrapping
sim=1000
size=250
boot_result <- lapply(1:sim, function(i, X=wkly_sales.str1, Y=wkly_sales.str2, size) 
  {
  #Sampling with replacement
  index <- sample(1:length(X), size = size, replace = T)
  X_s <- X[index]
  Y_s <- Y[index]
  
  #Pearson Correlation
  p <- cor.test(X_s, Y_s, method = "pearson",
                 alternative = "two.sided", conf.level = 0.95)
  #Spearman Correlation
  s <- cor.test(X_s, Y_s, method = "spearman",
                 alternative = "two.sided", conf.level = 0.95)
  #Kendall's Tau
  k <- cor.test(X_s, Y_s, method = "kendall",
                 alternative = "two.sided", conf.level = 0.95)
  return(c(p$estimate, s$estimate, k$estimate,
           P_pv = p$p.value, S_pv = s$p.value, K_pv = k$p.value))
}, wkly_sales.str1, wkly_sales.str2, size)

boot_result <- do.call("rbind", boot_result)

estimate_mean <- apply(boot_result[,c(1:3)], 2, mean)
estimate_sd <- apply(boot_result[,c(1:3)], 2, sd)

#Wilcoxon Signed Rank and Sum Rank Test
wilcox.test(wkly_sales.str1, wkly_sales.str2, paired = TRUE)

hist(wkly_sales.str1, col=rgb(0,0,1,0.2), main = 'Comparison of Histograms', 
     xlab = 'Weekly Sales')
hist(wkly_sales.str2, col=rgb(1,0,0,0.2), add=T)
legend("topright", c("Store 1", "Store 2"),
       col=c("blue", "red"), lwd=1, cex = 0.6)

data2 %>%
  ggplot(aes(Weekly_Sales)) +
  geom_density(aes(fill=Store), alpha=0.5)

wilcox.test(wkly_sales.str1, wkly_sales.str2, paired = FALSE)

#Fligner - Killeen test of homogeneity of variances
fligner.test(list(wkly_sales.str1,wkly_sales.str2))


