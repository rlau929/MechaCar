# MechaCar

## For mechacarwriteup
-Which variables/coefficients provided a non-random amount of variance to the mpg values in the dataset?
AWD, spoiler angle, and vehicle weight provides non-random amount of variance to mpg values. Ground clearance and vehicle length provides random amounts of variance.

-Is the slope of the linear model considered to be zero? Why or why not?
The slope is definitely not zero, but most variables will have slopes close to zero. By using the summary function to calculate the linear model, 
we can see the slope values of each variable. Since each variable contains values other than 0, we can confirm the slopes are not zero. We can also plot 
the linear model using the ggplot() function and view it using a geom_point() function and geom_line() function. Doing so, we can visually determine that all variable slopes will not be zero.

-Does this linear model predict mpg of MechaCar prototypes effectively? Why or why not?
The data is useful and could predict mpg of MechaCar prototypes effectively. For example, AWD, spoiler angle, and vehicle weight have Pr(>|t|) values above the significance level of 0.05. 
We cannot reject the null hypothesis with these three variables because it is significant to our data analysis. Additionally, the data is normally distributed. 
However, there is more random variances with ground clearance and vehicle length and the Pr(>|t|) is below the significance value of 0.05. As a result, we can reject the null hypothesis for these two variables.

## Multiple Linear Regression Written Analysis
1. The two selected variables had p values < 0.05.
2. The non-zero slope was because of the overall p value < 0 in the multiple regression model for the 5 variables.
3. The model predicts efffectively because r = 0.70, meaning that about 70% predictions are correct.

 
----------------------------------------------------------------------------
## Suspension coil test
The mean of the suspension coil test is 1500 PSI and the median is 1500 PSI, and the standard deviation is 1.76. The Pr(>|t|) of Lot1 is 0.85, Lot2 is 0.80 and Lot3 is 0.56 which is above the significance level of 0.05 and 
we cannot reject the null hypothesis. As a result, all manufacturing lots are statistically similar and the data is normally distributed.

The design specifications for the MechaCar suspension coils dictate that the variance of the suspension 
coils must not exceed 100 pounds per inch. Does the current manufacturing data meet this 
design specification? Why or why not?

According to the summary statistics,  we can safely assume the design specifications of the MechaCar suspension coils will not exceed 100 pounds per inch, because the Pr(>|t|) value is 
above the significance level of 0.05 (Lot1 is 0.85, Lot2 is 0.80 and Lot3 is 0.56). We cannot reject the null hypothesis and that each manufacturing lot will not have random variances. 
Additionally, we can safely assume that each manufacturing lot are significantly similar and normally distributed. Overall, since there is a strong significance level and no random variances, we 
can safely assume that the suspension coil data meets the design specifications.

From the t.test() function, our p-value is above our significance level of 0.74. Therefore, we do not have sufficient evidence to reject the null hypothesis, 
and we would state that the data is normally distributed. Overall, the sample population is significantly similar to the mean population.

## Summary stats including mead, median & variance etc.
The median of the suspension coil test is 0.085, manufacturing_lot2 has standard deviation of 1.76 and pr(>|t|) is 0.80, and manufacturing_lot3 has standard deviation of 1.76 and pr(>|t|) is 0.56. Mean_psi is 1500, median is 1500, minimum is 1463, and maximum is 1536. The p-value is 0.57 


----------------------------------------------------------------------------
## Design Your Own Study

To better understand the how the MechaCar compares to other cars in the market, we have to design a new study. In the new study, we will create a null hypothesis and an alternative hypothesis. 
Our null hypothesis is that the MechaCar is the safest car in the market; whereas, our alternative hypothesis is that the MechaCar is not the safest car in the market. 
In the new study, we will compare the safety ratings to other variables such as cost, coil settings, fuel efficiency, color options, hp, crash test, chassis rigidity, vehicle weight, vehicle length, tire size, and 0-60mph time. 
We can do a statistical test and use the summary() function to test the aov or lm of each variable. We can extrapolate the data and find the p-value and determine the amount of random variances in the data. 
We can use the p-value to either accept or reject our null hypothesis; whether MechaCar is the safest car compared to other cars. 

Another test we can compare is the safety rating to sales (theoretically, this should be tested first). We cannot perform this study with the MechaCar since the MechaCar is a production car and has no sales yet. 
However, there is large amounts of data of other car sales and safety ratings. We can make another null hypothesis in this case and state that consumers are more likely to purchase cars with higher safety rating. 
If our null hypothesis is accepted, we can continue with our MechaCar analysis since we’ve concluded that consumers will purchase based on safety ratings. Note: this hypothesis should be tested first before continuing to test 
and compare MechaCar’s safety ratings.
