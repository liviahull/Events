rm(list = ls())

# Upload libraries -------------------------------------------------------

library(dplyr)
library(ggplot2)
library(readr)
library(reshape2)
library(rcompanion)
library(binr)
library(tibble)
library(glm2)
library(rpart)
library(MASS)
library(leaps)
library(DAAG)
library(sandwich)
library(msm)
library(car)

# Read in data ------------------------------------------------------------
mydata <- read_csv("Events.csv") %>% 
  mutate(failure_rate = (Events/Length)/3) %>%
  mutate(
    Region  = case_when(
      Region == "Area 1" ~ "Area 01",
      Region == "Area 2" ~ "Area 02",
      Region == "Area 3" ~ "Area 03",
      Region == "Area 4" ~ "Area 04",
      Region == "Area 5" ~ "Area 05",
      Region == "Area 6" ~ "Area 06",
      Region == "Area 7" ~ "Area 07",
      Region == "Area 8" ~ "Area 08",
      Region == "Area 9" ~ "Area 09",
      TRUE               ~ Region
    )
  )


# Checks: histograms all variables ----------------------------------------------------------

## Failure_rate -- > target variable (Continuous)

range(mydata$failure_rate) # range is 0 to 62.9 but with outliers

group_by(mydata, failure_rate) %>% # 61% are zero
  summarise(counts = n()) %>%
  mutate(perc_zero = counts/nrow(mydata))

ggplot(mydata, aes(failure_rate)) + 
  geom_histogram(binwidth = 0.1) +
  coord_cartesian(xlim = c(0,20)) +
  theme_bw()

ggplot(mydata, aes(y=failure_rate, x = Region))+
  geom_boxplot() 

### Conclusion: 61% failure rates are zero, outliers are in Area 2, Area 4, Area 7, Area 8, Area 11

## Diameter -- > predictor (Discrete)

group_by(mydata, Diameter) %>%
  summarize(counts = n())

ggplot(mydata, aes(factor(Diameter))) +
  geom_bar()

### Conclusion: there are 9 types (100, 150, 225, 300, 375, 450, 600, 1050) most of them are of 150 and 225, least 900 and 1050

## Age -- > predictor (Continous)

range(mydata$Age)

group_by(mydata, Age) %>%
  summarize(counts = n())

ggplot(mydata, aes(Age)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0,100, by = 1))+
  theme(axis.text.x = element_text(size = 6))

ggplot(mydata, aes(Age)) +
  geom_histogram(binwidth = 10) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0,100, by = 10)) +
  theme(axis.text.x = element_text(size = 8))

### Conclusion: most of them are about 10 years old but there are some really old assets (over 75 years)

## Length -- > predictor (Continuous)

range(mydata$Length) # range is 0.00065 to 339 so this variable might need transforming for regression

group_by(mydata, Length) %>%
  summarize(counts = n())

ggplot(mydata, aes(Length)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(0,10, by = 0.1))

  # cumulative freq
leng <- mydata$Length
breaks = seq(0, 350, by=10)
leng.cut = cut(leng, breaks, right=FALSE)
leng.freq = table(leng.cut)
cumfreq0 = c(0, cumsum(leng.freq)) 
plot(breaks, cumfreq0,            # plot the mydata 
          main="Cumulative freq for pipe length",  # main title 
          xlab="Length in Km",        # x−axis label 
          ylab="Cumulative length")   # y−axis label 
lines(breaks, cumfreq0)           # join the points

### Conclusion: most pipes are very short, less than 10km

## Events -- > counts

range(mydata$Events) # 0 to 457

ggplot(mydata, aes(Events)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0,20, by = 1)) +
  scale_y_continuous(limits = c(0,350))

group_by(mydata, Events) %>% # 61% are zero
  summarise(counts = n()) %>%
  mutate(perc_zero = counts/nrow(mydata))

# cumulative freq
ev <- mydata$Events
breaks = seq(0, 20, by=1)
leng.cut = cut(ev, breaks, right=FALSE)
leng.freq = table(leng.cut)
cumfreq0 = c(0, cumsum(leng.freq)) 
plot(breaks, cumfreq0,            # plot the mydata 
     main="Cumulative events",  # main title 
     xlab="Events",        # x−axis label 
     ylab="Cumulative freq")   # y−axis label 
lines(breaks, cumfreq0)           # join the points


# Checks: Variables by Region and Dimeter ---------------------------------------------

## Length by Region

# How many observations in each Region?
  obs_in_region <- group_by(mydata, Region) %>%
    summarise(counts = n())
  
  ggplot(mydata, aes(x=Region)) +
    geom_bar(fill = "pink") +
    theme_bw()
  
# How many observations of each diameter in each Region?
    obs_byDiam_in_region <- group_by(mydata, Region, Diameter) %>%
    summarise(counts = n()) %>%
    dcast(Region~Diameter)
  
  ggplot(mydata, aes(x=factor(Diameter))) +
    geom_bar(fill = "red") +
    theme_bw() +
    facet_grid(Region~.) 
 
# what are summary stats for each diamenter in each region?
  length_byDiam_in_region <- group_by(mydata, Region, Diameter) %>%
    summarise(avg_length = mean(Length), median_length = median(Length)) #%>%
    #dcast(Region~Diameter)
  
# length boxplots by diamenter and region
  ggplot(mydata, aes(x=factor(Diameter), y=Length)) +
    geom_boxplot(fill = "magenta") + # shows outliers
    theme_bw() +
    facet_wrap(~Region)
 
# length averages by diamenter and region  
  ggplot(length_byDiam_in_region, aes(x=factor(Diameter), y=avg_length, color = Region)) +
    geom_point(size = 4) + # shows length averages by diameter for each region
    theme_bw() 

# length medians by diameter and region
  ggplot(length_byDiam_in_region, aes(x=factor(Diameter), y=median_length, color = Region)) +
    geom_point(size = 4) + # shows length averages by diameter for each region
    theme_bw()   
    
# Conclusion: sample sizes of each pipes are similar in all regions. 
# There are a few types of diameters: 100, 150, 225, 300, 375, 450, 600, 900, 1050
# For each type of diameter, average length of each pipe are different because we have outliers


# what are summary stats for each diamenter in each region?
age_byDiam_in_region <- group_by(mydata, Region, Diameter) %>%
  summarise(avg_age = mean(Age), median_age = median(Age)) #%>%
#dcast(Region~Diameter)

# length boxplots by diamenter and region
ggplot(mydata, aes(x=factor(Diameter), y=Age)) +
  geom_boxplot(fill = "green") + # shows outliers
  theme_bw() +
  facet_wrap(~Region)

# length averages by diamenter and region  
ggplot(age_byDiam_in_region, aes(x=factor(Diameter), y=avg_age, color = Region)) +
  geom_point(size = 5) + # shows length averages by diameter for each region
  theme_bw() 

# length medians by diameter and region
ggplot(age_byDiam_in_region, aes(x=factor(Diameter), y=median_age, color = Region)) +
  geom_point(size = 5) + # shows length averages by diameter for each region
  theme_bw()     


      
# Modelling -------------------

## Can we use multiple regression? https://stats.idre.ucla.edu/r/dae/poisson-regression/
## No, response is not normally distributed, and it represents counts
ggplot(mydata, aes(Age, Events)) + geom_point() + theme_bw()
ggplot(mydata, aes(Length, Events)) + geom_point() + theme_bw()
ggplot(mydata, aes(factor(Diameter), Events)) + geom_boxplot() + theme_bw()
ggplot(mydata, aes(factor(Region), Events)) + geom_boxplot() + theme_bw()
ggplot(mydata, aes(Events)) + geom_histogram(binwidth = 10) + theme_bw()

## Can we use Poisson regression? 
## Yes we have non-negative integers

##### POISSON 1 ------------------------------------
pois1 <- glm2(failure_rate ~ Age+Diameter+Length+Region, 
     family = "poisson",
     data = mydata)

anova(pois1)

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(pois1)

# obtain robust SE
cov.pois1 <- vcovHC(pois1, type="HC0")
std.err <- sqrt(diag(cov.pois1))
r.est <- cbind(Estimate= coef(pois1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(pois1)/std.err), lower.tail=FALSE),
               LL = coef(pois1) - 1.96 * std.err,
               UL = coef(pois1) + 1.96 * std.err)

r.est

## examine output 

### 1. deviance residuals should be approx normally distributed
modelPoisson <- summary(pois1)
x <- modelPoisson$deviance.resid
hist(x,
     col="red",
     breaks=10) # it does look a bit skewed 
mean(x)
### 2. coefficients: 
coeffPoisson <- as.data.frame(coefficients(pois1))

### 3. goodness of fit for the overall model
#We can use the residual deviance to perform a goodness of fit test for the overall model. 
#The residual deviance is the difference between the deviance of the current model and the maximum deviance of the ideal model where the predicted values are identical to the observed. 
#Therefore, if the residual difference is small enough, the goodness of fit test will not be significant, indicating that the model fits the data. 
#We conclude that the model fits reasonably well because the goodness-of-fit chi-squared test is not statistically significant. 
#If the test had been statistically significant, it would indicate that the data do not fit the model well. In that situation, we may try to determine if there are omitted predictor variables, if our linearity assumption holds and/or if there is an issue of over-dispersion.

with(pois1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

#Conclusion: the test is statistically significant so either - linearity assumption doesn't hold - there is over-dispersion

# set up axis to plot the regression 

ax <- seq(0,400, by = 0.1)
Y <- predict(pois1, list(Length= ax))
plot(Length, Number, xlab = "expl", ylab="failure_rate", pch = 16)


##### POISSON 2 ------------------------------------
## update first model by dropping Region
pois1 <- glm2(failure_rate ~ Age+Diameter+Length+Region, 
              family = "poisson",
              data = mydata)
pois2 <- update(pois1, . ~ . - Region)
anova(pois2, pois1, test="Chisq") # models are not the same, second significantly better

# to compute the standard error 
cov.pois1 <- vcovHC(pois1, type="HC0")
std.err <- sqrt(diag(cov.pois1))
r.est <- cbind(Estimate= coef(pois1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(pois1)/std.err), lower.tail=FALSE),
               LL = coef(pois1) - 1.96 * std.err,
               UL = coef(pois1) + 1.96 * std.err)
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), 
                 coef(pois1), cov.pois1)

## exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s

rexp.est

# goodness of fit 
with(pois2, cbind(res.deviance = deviance, df = df.residual,
                  p = pchisq(deviance, df.residual, lower.tail=FALSE)))
modelPoisson <- summary(pois2)
x <- modelPoisson$deviance.resid
hist(x,
     col="red",
     breaks=10) # it does look a bit skewed 

# diagnostic plots
infl <- influence(pois2) # regression diagnostics

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(pois2)

## predict 
(s1 <- data.frame(math = mean(p$math),
                  prog = factor(1:3, levels = 1:3, labels = levels(p$prog))))

predict(m1, s1, type="response", se.fit=TRUE)

##### POISSON 3 ------------------------------------
## update second model by dropping Length
pois2 <- glm2(failure_rate ~ Age+Diameter+Length, 
              family = "poisson",
              data = mydata)
pois3 <- update(pois2, . ~ . - Length)
anova(pois3, pois2, test="Chisq") # models are not the same, third significantly better

# to compute the standard error 
cov.pois3 <- vcovHC(pois3, type="HC0")
std.err <- sqrt(diag(cov.pois3))
r.est <- cbind(Estimate= coef(pois3), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(pois3)/std.err), lower.tail=FALSE),
               LL = coef(pois3) - 1.96 * std.err,
               UL = coef(pois3) + 1.96 * std.err)
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3)), 
                 coef(pois3), cov.pois3)

## exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s

rexp.est

# goodness of fit 
with(pois3, cbind(res.deviance = deviance, df = df.residual,
                  p = pchisq(deviance, df.residual, lower.tail=FALSE)))
modelPoisson <- summary(pois3)
x <- modelPoisson$deviance.resid
hist(x,
     col="red",
     breaks=10) # it does look a bit skewed 
mean(x)

# diagnostic plots
infl <- influence(pois3) # regression diagnostics
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(pois3)

##### POISSON 4 ------------------------------------
# fit a quasi-Poisson model to the same data https://www.theanalysisfactor.com/glm-r-overdispersion-count-regression/ 
#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html
pois3q <- glm(failure_rate~Age+Diameter, 
              family = "quasipoisson", 
              data = mydata)

# goodness of fit 
with(pois3q, cbind(res.deviance = deviance, df = df.residual,
                  p = pchisq(deviance, df.residual, lower.tail=FALSE)))
modelPoisson <- summary(pois3)
x <- modelPoisson$deviance.resid
hist(x,
     col="red",
     breaks=10) # it does look a bit skewed 
mean(x) # less than 1 so the problem is under-dispersion


##### POISSON 5 ------------------------------------
## fit interactions on third model
pois3 <- glm2(failure_rate ~ Age+Diameter+Age:Diameter, 
              family = "poisson",
              data = mydata)
warnings()
# to compute the standard error 
cov.pois3 <- vcovHC(pois3, type="HC0")
std.err <- sqrt(diag(cov.pois3))
r.est <- cbind(Estimate= coef(pois3), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(pois3)/std.err), lower.tail=FALSE),
               LL = coef(pois3) - 1.96 * std.err,
               UL = coef(pois3) + 1.96 * std.err)
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3)), 
                 coef(pois3), cov.pois3)

## exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s

rexp.est

# goodness of fit 
with(pois3, cbind(res.deviance = deviance, df = df.residual,
                  p = pchisq(deviance, df.residual, lower.tail=FALSE)))
modelPoisson <- summary(pois3)
x <- modelPoisson$deviance.resid
hist(x,
     col="red",
     breaks=10) # it does look a bit skewed 

# diagnostic plots
infl <- influence(pois3) # regression diagnostics
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(pois3)

# check if there is a problem with overdispersion
# residual mean deviance from summary analysis should be near 1, certainly less than 2
mean(x) # -0.4

dev.off()





















# model against data 
fitv <- fitted(fitPoisson)
coeff <- coefficients(fitPoisson)
eq = paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1))

ggplot(mydata, aes(Age, Events)) + 
         geom_point() + 
         theme_bw() +
  geom_abline(intercept =  2.134509712, slope = 0.010762623, color="red", 
              
              linetype="dashed", size=1.5)

dev.off()






# Binning the response variable to use logistic regression / decision trees
# Failure rate = failures per km per year
to_split <- round(mydata$failure_rate, digits = 3) # failure rate as a vector

binned_fr <- bins(to_split, 
                  target.bins = 7,
                  max.breaks = 20,
                  exact.groups = F, 
                  verbose = F, 
                  errthresh = 0.1,
                  minpts = NA) # try different ways of binning

# number of points in each 
x <- as_tibble(binned_fr$binct) %>% 
  rownames_to_column()
# plot bins
ggplot(x, aes(rowname, value)) + 
  geom_bar(stat = "identity") 
  

binned_fr1 <- bins.greedy(to_split,
                          nbins = 23,
                          minpts = 18,
                          thresh = 0.4,
                          naive = TRUE)

# number of points in each 
y <- as_tibble(binned_fr1$binct) %>% 
  rownames_to_column()
# plot bins
ggplot(y, aes(rowname, value)) + 
  geom_bar(stat = "identity") 





# transform continous data in discrete data 
mydata <- mutate(mydata, 
                 BinnedFR = ifelse(failure_rate == 0, 0, 
                            ifelse(failure_rate > 0 & failure_rate <= 1, 1,
                            ifelse(failure_rate > 1 & failure_rate <= 10, 2,
                            ifelse(failure_rate > 10 & failure_rate <= 30, 3, 4
                                   )
                                   )
                                   )
                                   )
                 )


# plot bins
ggplot(mydata, aes(BinnedFR)) + 
  geom_histogram(bins = 4) 


fitGLM<- glm2(BinnedFR ~ Age+Region+Length, 
              family = binomial(link = "logit"), 
              data = mydata, 
              model = TRUE)
summary(fitGLM)
plot(fitGLM)
coefficients(fitGLM)
anova(fitGLM)




fitGLM<- glm2(BiEvents~Age, 
     family = binomial(link = "logit"), 
     data = mydata, 
     model = TRUE)
summary(fitGLM)
plot(fitGLM)
coefficients(fitGLM)
anova(fitGLM)






##### LOGISTIC REGRESSION -------------------

rm(list = ls())

mydata <- mutate(mydata, fr_binned = ifelse(failure_rate == 0, 0, 1))


log1 <- glm(fr_binned~Age+Diameter+Length+Region,
            family = binomial(link = 'logit'),
            data = mydata)
summary(log1)
anova(log1, test = "Chisq")


# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(log1)

# obtain robust SE
cov.log1 <- vcovHC(log1, type="HC0")
std.err <- sqrt(diag(cov.log1))
r.est <- cbind(Estimate= coef(log1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(log1)/std.err), lower.tail=FALSE),
               LL = coef(log1) - 1.96 * std.err,
               UL = coef(log1) + 1.96 * std.err)

r.est

## examine output 

### 1. deviance residuals should be approx normally distributed
modelPoisson <- summary(log1)
x <- modelPoisson$deviance.resid
hist(x,
     col="red",
     breaks=10) # it does look a bit skewed 

### 2. coefficients: 
coeffPoisson <- as.data.frame(coefficients(log1))

### 3. goodness of fit for the overall model
#We can use the residual deviance to perform a goodness of fit test for the overall model. 
#The residual deviance is the difference between the deviance of the current model and the maximum deviance of the ideal model where the predicted values are identical to the observed. 
#Therefore, if the residual difference is small enough, the goodness of fit test will not be significant, indicating that the model fits the data. 
#We conclude that the model fits reasonably well because the goodness-of-fit chi-squared test is not statistically significant. 
#If the test had been statistically significant, it would indicate that the data do not fit the model well. In that situation, we may try to determine if there are omitted predictor variables, if our linearity assumption holds and/or if there is an issue of over-dispersion.

with(log1, cbind(res.deviance = deviance, df = df.residual,
                  p = pchisq(deviance, df.residual, lower.tail=FALSE)))


## update first model by dropping Region
log1 <- glm(fr_binned~Age+Diameter+Length+Region,
            family = binomial(link = 'logit'),
            data = mydata)
log2 <- update(log1, . ~ . - Region)
anova(log2, log1, test="Chisq") # models are almost the same, but at 5% significance levels might be different

# Conclusion: dropping Region 

### 1. deviance residuals should be approx normally distributed
modelPoisson <- summary(log2)
x <- modelPoisson$deviance.resid
hist(x,
     col="red",
     breaks=10)

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(log2)
dev.off()

### 3. goodness of fit for the overall model
with(log2, cbind(res.deviance = deviance, df = df.residual,
                 p = pchisq(deviance, df.residual, lower.tail=FALSE)))

### 4. Infuential points - check outliders
# Standardised deviance residuals in binary logistic regression are not normallly distributed
residualPlots(log2)
library(zoo)
n <- nrow(mydata)

infl_log2 <- influence(log2)
h <- as.data.frame(infl_log2$hat) # Leverage H
colnames(h) <- "hat"
cutoffH <-  3*4/n # cut off for Leverage 
cutoffLeverageH <-  as.data.frame(rep(cutoffH, n))
colnames(cutoffLeverageH) <- "H"

xaxis<- index(mydata)
plot(xaxis, h$hat)
abline(h=cutoffLeverageH$H, col = "red")

# Influential Observations
# added variable plots 
avPlots(log2)

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mydata)-length(log2$coefficients)-2))  # cut off for Cooks
cutoffC <- rep(n)
cutoffC <- as.data.frame(rep(cutoff, n)) 
colnames(cutoffC) <- "Cook"
plot(log2, which=4, cook.levels=cutoff)
abline(h = cutoffC$Cook, col = "red")


#Dffit
cutoffD <- 2*(sqrt(4/n)) # cutoff for D

# Influence Plot 
#influencePlot(log2,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# SUMMARY INFLUENTIAL POINTS ####
#select only relevant DDFITS with all other measures
influential <- influence.measures(log2)
influential_df <- as.data.frame(influential[[1]]) %>%
  mutate(Influential = if_else(hat > cutoffH, 1, 
                              if_else(cook.d> cutoffC, 1, 
                                      if_else(abs(dffit)> cutoffD,1, 0))))
infl <- as.data.frame(influential_df$Influential)
colnames(infl) <- "Influential"

detach("package:car", unload=TRUE)
detach("package:MASS", unload=TRUE)

Influential_data <- cbind(mydata, infl) %>%
  filter(Influential == 1) %>%
  select(c(Region, Diameter, Age, Length, Events))


# Predictions ####

# model fitting 

train <- mydata[1:1700,]
test <- mydata[1700:2648,]

model <- glm(fr_binned~Age+Diameter+Length,
             family = binomial(link = 'logit'),
             data = train)

fitted.results <- predict(model, 
                          newdata = subset(test, 
                                           select = c(2,3,4)), 
                          type = 'response')
fitted.results <- ifelse(fitted.results > 0.5, 1,0)

misClasificError <- mean(fitted.results != test$fr_binned)
# Accuracy 19%


# ROC plot
library(ROCR)
pr <- prediction(fitted.results, test$fr_binned)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc # area under the curve 


#prediction of one value
new.dat <- data.frame(Diameter = 100, Age = 95, Length = 0.5)

firstPrediction <- predict(log2, newdata = new.dat, interval = 'confidence') #predicting mean # interval = 'prediction'



rm(list=ls())




#95% confidence interval 
ggplot(mydata, aes(x=Age, y=fr_binned)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=glm, color='#2C3E50')

#95% confidence and prediction intervals
income_credit <- predict(model, interval="prediction")
new_df <- cbind(sales, income_credit)
ggplot(new_df, aes(income, credit))+
  geom_point() +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method=lm, se=TRUE)+ theme_bw()






















