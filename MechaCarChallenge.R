library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(jsonlite)

# MPG Regression
mechacar_mpg <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
names(mechacar_mpg)[names(mechacar_mpg)=="vehicle length"] <- "vehicle_length"
names(mechacar_mpg)[names(mechacar_mpg)=="vehicle weight"] <- "vehicle_weight"
names(mechacar_mpg)[names(mechacar_mpg)=="spoiler angle"] <- "spoiler_angle"
names(mechacar_mpg)[names(mechacar_mpg)=="ground clearance"] <- "ground_clearance"

# p-value of mpg is 0.79, greater than 0.05.
shapiro.test(mechacar_mpg$mpg)

# Multiple linear regression
summary(lm(mpg ~ AWD + ground_clearance + spoiler_angle + vehicle_weight + vehicle_length,data = mechacar_mpg))

# Calculating random variances between each variable
summary(aov(mpg~AWD + ground_clearance + spoiler_angle + vehicle_weight + vehicle_length,data=mechacar_mpg))

# Multiple linear regression mpg vs vehicle_length; slope: 6.267e+00
model <- lm(mpg ~ vehicle_length,mechacar_mpg)
yvals <- model$coefficients['vehicle_length']*mechacar_mpg$vehicle_length + model$coefficients['(Intercept)']
plt <- ggplot(mechacar_mpg,aes(x=vehicle_length,y=mpg))
plt + geom_point() + geom_line(aes(y=yvals), color ="red")

# Multiple linear regression mpg vs vehicle_weight; slope: 1.245e-03 
model2 <- lm(mpg ~ vehicle_weight, mechacar_mpg)
yvals2 <- model2$coefficients['vehicle_weight']*mechacar_mpg$vehicle_weight + model2$coefficients['(Intercept)']
plt2 <- ggplot(mechacar_mpg,aes(x=vehicle_weight,y=mpg))
plt2 + geom_point() + geom_line(aes(y=yvals2), color ="red")

# Multiple linear regression mpg vs spoiler_angle; slope: 6.877e-02
model3 <- lm(mpg ~ spoiler_angle, mechacar_mpg)
yvals3 <- model3$coefficients['spoiler_angle']*mechacar_mpg$spoiler_angle + model3$coefficients['(Intercept)']
plt3 <- ggplot(mechacar_mpg,aes(x=spoiler_angle,y=mpg))
plt3 + geom_point() + geom_line(aes(y=yvals3), color ="red")

# Multiple linear regression mpg vs ground clearance; slope: 3.546e+00
model4 <- lm(mpg ~ ground_clearance, mechacar_mpg)
yvals4 <- model4$coefficients['ground_clearance']*mechacar_mpg$ground_clearance + model4$coefficients['(Intercept)']
plt4 <- ggplot(mechacar_mpg,aes(x=ground_clearance,y=mpg))
plt4 + geom_point() + geom_line(aes(y=yvals4), color ="red")

# Compare the difference between two samples
mpg_awd0 <- mechacar_mpg %>% filter(AWD==0)
mpg_awd1 <- mechacar_mpg %>% filter(AWD==1)
t.test(mpg_awd0$mpg,mpg_awd1$mpg,paired = T)

# Compare the difference between two samples using chisq.test function
tbl <- table(mechacar_mpg$mpg,mechacar_mpg$AWD)
chisq.test(tbl)

#----------------------------------------------
# Suspension Coil Summary
mechacar_coil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

# Coil summary data and standard deviation
summary(lm(PSI~Manufacturing_Lot, mechacar_coil))

# Mean and Median PSI of suspension coil
mean_psi <- summary(mechacar_coil,mean(PSI))
mean_psi

# Variance PSI of suspension coil
# Coil summary data of DF=2, Sum Sq=26, Mean Sq=12.92, F value=0.17, and Pr(>F)=0.85
summary(aov(PSI~Manufacturing_Lot,data=mechacar_coil))

# Suspension Coil T-Test; p-value=0.38
sample_table <- mechacar_coil %>% sample_n(50)
t.test(log10(sample_table$PSI),mu=mean(log10(mechacar_coil$PSI)))
