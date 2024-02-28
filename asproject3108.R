#load csv
library(readr)
data <- read_csv("data.csv")#add your Dataset(csv.file)
View(data)

#Check the structure of the dataset
str(data)


#Display the first few rows of the dataset
head(data)

#Display the last few rows of the dataset
tail(data)

#Summarize numerical variables
summary(data$Age) # Summary statistics for Age
summary(data$AnnualHouseholdincome) # Summary statistics for Annual Household Income


#Summarize categorical variables
table(data$Gender) # Frequency table for Gender
table(data$Healthinsurance) # Frequency table for Health Insurance
table(data$Financialstrainexperienced) # Frequency table for Financial Strain Experienced
# Check the column names in your dataset
names(data)

table(data$`Satistifaction
`) # Frequency table for Satisfaction

# two variables G and l, where G represents the gender 
#information from your dataset, and l represents the annual household income.
G=data$Gender
l=data$AnnualHouseholdincome
#one sample t-test for Income variable
t.test(l)
##Since the p-value is less than the significance level (0.05), we reject the null 
##hypothesis that the true mean annual household income is equal to 0.

#two sample t-test(Income~Gender)
t.test(l~G)
##the p-value (0.1346) is greater than the significance level (0.05), 
##we fail to reject the null hypothesis.


#two sample test for(Health insurance)
H=data$Healthinsurance
t.test(l~H)

##the p-value (0.1725) is greater than the significance level (0.05), 
##we fail to reject the null hypothesis. 


#NORMATLITY TEST->SHAPIRO TEST FOR Numeric variable(Income)
shapiro.test(l)

##The Shapiro-Wilk normality test is used to assess whether a variable follows a normal distribution.


# Corrected table
table <- c(11, 5, 8, 19, 6, 9)
m <- matrix(table, nrow = 2, byrow = TRUE)
rownames(m) <- c("Female", "Male")
colnames(m) <- c("Yes", "No", "Maybe")  # Corrected column names

print("Contingency Table:")
print(m)

##we have the contingency table correctly set up, we can interpret it accordingly.
## It seems like you have counts for different categories 
#("Yes", "No", "Maybe") of a variable broken down by gender ("Female", "Male").



# Perform chi-square test
result <- chisq.test(m)
print(result)

##The chi-square test result indicates that 
##the chi-squared statistic is 0.57605 with 2 degrees of freedom, and the associated p-value is approximately 0.7497.
#Since the p-value is greater than the significance level of 0.05, we fail to reject the null hypothesis



#This adjustment is done to ensure that all vectors have the same length before 
#creating the dataframe.
Yes <- c(50000, 50000, 100000, 50000, 500000, 100000, 500000, 500000)
No <- c(100000, 500000, 500000, 100000, 100000, 50000, 50000, 50000)
Sometimes <- c(500000, 100000, 500000, 50000, 100000, 100000, 100000, 100000)

# Adjusting No to have the same length as Yes and Sometimes
No <- c(No, rep(NA, length(Yes) - length(No)))

# Create the dataframe
data <- data.frame(Yes = Yes, No = No, Sometimes = Sometimes)



# Assuming you want to perform ANOVA
Anova_results <- aov(cbind(Yes, No, Sometimes) ~ 1, data = data)
summary(Anova_results)

##The ANOVA results indicate that there are no significant differences between the groups 
##(Yes, No, Sometimes) in terms of their annual household incomes.




#TWO WAY ANOVA (Income ~  visit a healthcare professional +Delayed_Treatment)
data <- read_csv("data.csv")
View(data)
data$Healthcareprofessional<-as.factor(data$Healthcareprofessional)
data$DelayedTreatment<-as.factor(data$DelayedTreatment)
anova<-aov(l ~`Healthcareprofessional` +DelayedTreatment, data = data)
anova

#The ANOVA results show that there is no significant difference in the mean annual household income between different levels of 
#Healthcareprofessional and DelayedTreatment.

#Non Parametric tests
#WILCOXON Signed Rank Test (Alternative of one sample t test)
wilcox.test(data$AnnualHouseholdincome) 
#pval 0.00....1<0.05 at 5% los Reject H0
##The test stastics V=1711 and p-value=2.492e-11 hencewe reject the !0


#Mann Whitney U Test/ Wilcoxon Sum Rank Test (Alt of Unpaired/ independent sample t test) (Income~Gender)
wilcox.test(data$AnnualHouseholdincome~data$Gender, paired=FALSE, exact=FALSE)
#PVAL 0.02<0.05 at 5% los ACCEPT H0

##The test statistic (W) is 375.5, and the p-value is 0.6011. 
##With a significance level typically set at 0.05, since the p-value is greater than this threshold, we fail to reject the null hypothesis.


#kruskal wallis test (Alt of One way ANOVA)
kruskal.test(data$AnnualHouseholdincome~data$Financialstrainexperienced) #(Income ~factor_increased opportunity)

##Since the p-value (0.5794) is greater than the significance level of 0.05, we fail to reject the null hypothesis.


kruskal.test(data$Healthcareprofessional ~ data$Healthinsurance)
##Since the p-value (0.3365) is greater than the significance level of 0.05, we fail to reject the null hypothesis


kruskal.test(data$AnnualHouseholdincome~data$Healthinsurance) #(Income~Stream)
##Since the p-value (0.002422) is less than the significance level of 0.05, we reject the null hypothesis



#descriptive statistics for numerical variable
income<-c(100000,500000,500000,100000,100000,500000,50000,50000,100000,50000,50000,25000,500000,100000,100000,50000,100000,50000,50000,50000,500000,
          100000,100000,100000,500000,100000,25000,100000,25000,25000,50000,500000,500000,100000,50000,25000,2000000,50000,50000,500000,250000,500000,
          5000000,25000,500000,500000,500000,100000,500000,25000,500000,25000,500000,500000,500000,100000,100000,100000)
#Measure of central tendency

mean(income)
median(income)
max(income)
min(income)
#measures o dispersion
range(income)
var(income)
sd(income)
summary(income)





#Visualisation

library(ggplot2)

# Bar Plot using ggplot
ggplot(data, aes(x = DelayedTreatment)) +
  geom_bar(fill = "red", color = "black") +
  labs(x = "DelayedTreatment", y = "Frequency") +
  theme_minimal()

##The bar plot visualizes the frequency distribution of the DelayedTreatment variable. Each bar represents 
##the count of observations corresponding to different levels of delayed treatment. 


# Scatter plot of Annual Household Income vs Healthcareprofessional
ggplot(data, aes(x = AnnualHouseholdincome, y = Healthcareprofessional)) +
  geom_point(color = "purple") +
  labs(title = "Annual Household Income vs Healthcareprofessional", x = "Annual Household Income", y = "Healthcareprofessional")

##The scatter plot visualizes the relationship between annual household income and healthcare professionalism.



# Box plot of Age by Gender
ggplot(data, aes(x = Gender, y = Age, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Gender", x = "Gender", y = "Age")

##the box plot provides a concise summary of the age distribution by gender, 
##allowing for easy comparison between groups.





# Bar plot of Financial Strain experienced
ggplot(data, aes(x = Financialstrainexperienced)) +
  geom_bar(fill = "orange") +
  labs(title = "Financial Strain Experienced", x = "Financial Strain", y = "Count")

##We can identify which level of financial strain is most common among the respondents.



#Barplot for the annual household income
ggplot(data, aes(x =  AnnualHouseholdincome)) +
  geom_bar(fill = "yellow", color = "blue") +
  labs(title = "Frequency of Annual Household Income",
       x = "Annual Household Income",
       y = "Frequency")

##This visualization provides valuable insights into 
##the income distribution of the surveyed population, which can be useful for understanding socioeconomic trends and making informed decisions.


##OVERALL CONCLUSION
##while gender and certain healthcare factors do not appear to significantly influence annual household income, health insurance coverage does have a notable impact. 
##These findings can help policymakers, healthcare providers, and organizations better understand the socioeconomic factors affecting income and make informed decisions 
##to address disparities and improve financial well-being.
