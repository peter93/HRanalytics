#############################HR Analytics Solution###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

# Based on the survey and current employee information,
# the company has maintained a database containing personal/demographic information,
# the survey conducted related to the experience of each employee and manager ratings.

## AIM:

# The aim is to automate the process of predicting 
# if a employee would leave or not and to find the factors affecting the attrition 
# Whether a employee would leave or not will depend on data from the following three buckets:

# 1. Demographic Information
# 2. Survey results of the employee
# 3. Employee Ratings provided by the manager

################################################################

### Data Understanding

# Install and Load the required packages
# install.packages("MASS")
# install.packages("car")
# install.packages("e1071")
# install.packages("caret", dependencies = c("Depends", "Suggests"))
# install.packages("cowplot")
# install.packages("GGally")
# install.packages("caTools")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)

# Loading 3 files
general_data<- read.csv("general_data.csv", stringsAsFactors = F)
employee_data<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_data<- read.csv("manager_survey_data.csv", stringsAsFactors = F)

in_time<- read.csv("in_time.csv", stringsAsFactors = F)
out_time<- read.csv("out_time.csv", stringsAsFactors = F)

str(general_data)    # 4410 obs of 24 variables including the target variable
str(employee_data) # 4410 obs of 4 variables
str(manager_data) # 4410 obs of 3 variables

# Collate the data together in one single file
length(unique(tolower(general_data$EmployeeID)))    # 4410, confirming EmployeeID is key 
length(unique(tolower(employee_data$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(manager_data$EmployeeID))) # 4410, confirming EmployeeID is key

setdiff(general_data$EmployeeID,employee_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,manager_data$EmployeeID) # Identical EmployeeID across these datasets

employee<- merge(employee_data,manager_data, by="EmployeeID",all = F)
employee<- merge(employee,general_data, by="EmployeeID", all = F)

View(employee) #master file

#Mapping time difference between in and out from timesheets
timemap <- Map(difftime, out_time[,-1], in_time[,-1])


#initiase the timedf with 0 value
timedf <- rep(0.0, nrow(in_time))
timedf

#initiase the total_working_days with 0 value
total_working_days <- rep(0, nrow(in_time))
total_working_days

#initiase the total_leaves with 0 value
total_leaves <- rep(0, nrow(in_time))
total_leaves

#initiase the avg_working_hrs with 0 value
avg_working_hrs <- rep(0, nrow(in_time))
avg_working_hrs

#Calculating total working hours for each employee in a seperate column
for(x in 1: nrow(in_time)){
  for(i in 1:(ncol(in_time)-1)){
    if(!is.na(timemap[[i]][x]))
    {timedf[x]= timedf[x]+timemap[[i]][x]
    total_working_days[x]= total_working_days[x]+1;
    }
  }
}

total_leaves <- ncol(out_time) - total_working_days 

avg_working_hrs <- timedf/total_working_days;

employee<- cbind(employee,timedf,total_leaves,avg_working_hrs) 


################################################################

### Data Preparation & Exploratory Data Analysis

# Understanding the structure of the collated file
str(employee) #4410 obs. of 30 variables;

# DistanceFromHome, MonthlyIncome, PercentSalaryHike are continuous
# Education,JobLevel need to be changed from integer to categorical


# Barcharts for categorical features with stacked telecom information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")



plot_grid(ggplot(employee, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(), 
          ggplot(employee, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   


plot_grid(ggplot(employee, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=MaritalStatus,fill=Attrition))+ geom_bar() +bar_theme1,
          align = "h") 



plot_grid(ggplot(employee, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar() +bar_theme1,
          ggplot(employee, aes(x=JobInvolvement,fill=Attrition))+ geom_bar() +bar_theme1,
          ggplot(employee, aes(x=PerformanceRating,fill=Attrition))+ geom_bar() +bar_theme1,
          align = "h")


#reveals strong contrast for employee wrt Gender,JobRole,BusinessTravel,EducationField
#moderate wrt whether Department and MaritalStatus

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(employee, aes(DistanceFromHome))+ geom_histogram(),
          ggplot(employee, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(employee, aes(MonthlyIncome))+ geom_histogram(),
          ggplot(employee, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(employee, aes(PercentSalaryHike))+ geom_histogram(binwidth = 1),
          ggplot(employee, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(employee, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 1),
          ggplot(employee, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(employee, aes(TotalWorkingYears))+ geom_histogram(binwidth = 1),
          ggplot(employee, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(employee, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 1),
          ggplot(employee, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(employee, aes(YearsAtCompany))+ geom_histogram(binwidth = 1),
          ggplot(employee, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(employee, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 1),
          ggplot(employee, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(employee, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 1),
          ggplot(employee, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(employee, aes(emptime))+ geom_histogram(binwidth = 1),
          ggplot(employee, aes(x="",y=emptime))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

#Need to check outliers in numeric variables



# Boxplots of numeric variables relative to employee status
plot_grid(ggplot(employee, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(employee, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Shorter working years sees more Attrition

# Correlation between numeric variables
library(GGally)
ggpairs(employee[, c("DistanceFromHome", "MonthlyIncome", "PercentSalaryHike","TotalWorkingYears","NumCompaniesWorked","TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager","timedf")])

#YearsAtCompany and YearsSinceLastPromotion are highly correlated (corr 0.618)


################################################################
### Data Preparation

# De-Duplication
# not needed

# Missing value
sapply(employee, function(x) sum(is.na(x))) # shows NAs are in EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance,NumCompaniesWorked,TotalWorkingYears column
View(subset(employee, is.na(TotalWorkingYears))) # 

#Setting missing values with mode value
freqval <- table(employee$EnvironmentSatisfaction)
employee$EnvironmentSatisfaction[which(is.na(employee$EnvironmentSatisfaction))]<- as.integer(names(freqval[freqval==max(freqval)]))
freqval <- table(employee$JobSatisfaction)
as.integer(names(freqval[freqval==max(freqval)]))
employee$JobSatisfaction[which(is.na(employee$JobSatisfaction))]<- as.integer(names(freqval[freqval==max(freqval)]))
freqval <- table(employee$WorkLifeBalance)
employee$WorkLifeBalance[which(is.na(employee$WorkLifeBalance))]<- as.integer(names(freqval[freqval==max(freqval)]))
freqval <- table(employee$NumCompaniesWorked)
employee$NumCompaniesWorked[which(is.na(employee$NumCompaniesWorked))]<- as.integer(names(freqval[freqval==max(freqval)]))
which(is.na(employee$EnvironmentSatisfaction))

#Setting missing TotalWorkingYears values with approx YearsAtCompany value
employee$TotalWorkingYears[which(is.na(employee$TotalWorkingYears))]<- employee$YearsAtCompany[which(is.na(employee$TotalWorkingYears))]


# Bringing the variables in the correct format
employee$EnvironmentSatisfaction<- ifelse(employee$EnvironmentSatisfaction==1,"Low",ifelse(employee$EnvironmentSatisfaction==2,"Medium",ifelse(employee$EnvironmentSatisfaction==3,"High",ifelse(employee$EnvironmentSatisfaction==4,"Very High",""))))
employee$JobSatisfaction<- ifelse(employee$JobSatisfaction==1,"Low",ifelse(employee$JobSatisfaction==2,"Medium",ifelse(employee$JobSatisfaction==3,"High",ifelse(employee$JobSatisfaction==4,"Very High",""))))
employee$WorkLifeBalance<- ifelse(employee$WorkLifeBalance==1,"Bad",ifelse(employee$WorkLifeBalance==2,"Good",ifelse(employee$WorkLifeBalance==3,"Better",ifelse(employee$WorkLifeBalance==4,"Best",""))))
employee$JobInvolvement<- ifelse(employee$JobInvolvement==1,"Low",ifelse(employee$JobInvolvement==2,"Medium",ifelse(employee$JobInvolvement==3,"High",ifelse(employee$JobInvolvement==4,"Very High",""))))
employee$PerformanceRating<- ifelse(employee$PerformanceRating==1,"Low",ifelse(employee$PerformanceRating==2,"Good",ifelse(employee$PerformanceRating==3,"High",ifelse(employee$PerformanceRating==4,"Very High",""))))
employee$Education<- ifelse(employee$Education==1,"Below College",ifelse(employee$Education==2,"College",ifelse(employee$Education==3,"Bachelor",ifelse(employee$Education==4,"Master","Doctor"))))
employee$JobLevel<- ifelse(employee$JobLevel==1,"JLevel1",ifelse(employee$JobLevel==2,"JLevel2",ifelse(employee$JobLevel==3,"JLevel3",ifelse(employee$JobLevel==4,"JLevel4","JLevel5"))))
employee$StockOptionLevel<- ifelse(employee$StockOptionLevel==0,"SOLevel0",ifelse(employee$StockOptionLevel==1,"SOLevel1",ifelse(employee$StockOptionLevel==2,"SOLevel2","SOLevel3")))



# Outlier treatment and imputing missing value

#Removing outliers for MonthlyIncome
quantile(employee$MonthlyIncome,seq(0,1,.01),na.rm = T)

#We can see a jump in value in 1% to 2% 13620.6 to 18590.0
employee$MonthlyIncome[which(employee$MonthlyIncome < 18590.0)]<-18590.0

#We can see a jump in value in 95% to 96% 178560.0 to 186437.6
employee$MonthlyIncome[which(employee$MonthlyIncome > 178560.0)]<-178560.0

#Removing outliers for NumCompaniesWorked
quantile(employee$NumCompaniesWorked,seq(0,1,.01),na.rm = T)
#no outliers

#Removing outliers for tot
quantile(employee$TotalWorkingYears,seq(0,1,.01),na.rm = T)

#We can see a sudden jump in value in 99% to 100% 35 to 40
employee$TotalWorkingYears[which(employee$TotalWorkingYears > 35)]<-35


#Removing outliers for TrainingTimesLastYear
quantile(employee$TrainingTimesLastYear,seq(0,1,.01),na.rm = T)

#Removing outliers for YearsAtCompany
quantile(employee$YearsAtCompany,seq(0,1,.01),na.rm = T)

#We can see a sudden jump in value in 98% to 99% 24 to 31
employee$YearsAtCompany[which(employee$YearsAtCompany > 24)]<-24


#Removing outliers for YearsSinceLastPromotion
quantile(employee$YearsSinceLastPromotion,seq(0,1,.01),na.rm = T)

#We can see a sudden jump in value in 95% to 96% 9 to 11
employee$YearsSinceLastPromotion[which(employee$YearsSinceLastPromotion > 9)]<-9

#Removing outliers for YearsWithCurrManager
quantile(employee$YearsWithCurrManager,seq(0,1,.01),na.rm = T)

#We can see a sudden jump in value in 99% to 100% 14 to 17
employee$YearsWithCurrManager[which(employee$YearsWithCurrManager > 14)]<-14

#Removing outliers for timedf
quantile(employee$timedf,seq(0,1,.01),na.rm = T)

#We can see a sudden jump in value in 99% to 100% 2637.283 to 2723.378 
employee$timedf[which(employee$timedf > 2637.283)]<-2637.283

#Removing outliers for total_leaves
quantile(employee$total_leaves,seq(0,1,.01),na.rm = T)
#no outliers

#Removing outliers for avg_working_hrs
quantile(employee$avg_working_hrs,seq(0,1,.01),na.rm = T)
#no outliers

################################################################
# Feature standardisation

# Normalising continuous features 
employee$Age<- scale(employee$Age) # scale used: mean 36.9, sd 9.13
employee$DistanceFromHome<- scale(employee$DistanceFromHome) # scale used: mean 9.2, sd 8.1
employee$MonthlyIncome<- scale(employee$MonthlyIncome) # scale used: mean 65030, sd 47068.9
employee$NumCompaniesWorked<- scale(employee$NumCompaniesWorked) # scale used: mean 2.7, sd 2.5
employee$PercentSalaryHike<- scale(employee$PercentSalaryHike) # scale used: mean 15.21, sd 3.66
employee$TotalWorkingYears<- scale(employee$TotalWorkingYears) # scale used: mean 11.28, sd 7.78
employee$TrainingTimesLastYear<- scale(employee$TrainingTimesLastYear) # scale used: mean 2.8, sd 1.3
employee$YearsAtCompany<- scale(employee$YearsAtCompany) # scale used: mean 7, sd 6.1
employee$YearsSinceLastPromotion<- scale(employee$YearsSinceLastPromotion) # scale used: mean 2.2, sd 3.2
employee$YearsWithCurrManager<- scale(employee$YearsWithCurrManager) # scale used: mean 4.1, sd 3.6
employee$timedf<- scale(employee$timedf) # scale used: mean 1821, sd 331.36
employee$avg_working_hrs<- scale(employee$avg_working_hrs) # scale used: mean 7.700799, sd 1.340216
employee$total_leaves<- scale(employee$total_leaves) # scale used: mean 25.73469, sd 5.503779


# converting target variable employee from No/Yes character to factorwith levels 0/1 
employee$Attrition<- ifelse(employee$Attrition=="Yes",1,0)

# Checking churn rate of prospect customer

Attrition <- sum(employee$Attrition)/nrow(employee)
Attrition # 16.12% churn rate. 


# creating a dataframe of categorical features
employee_attr<- employee[,-c(1,7,8,11,14,19,20,21,22,23,25,26,27,28,29,30,31,32)]
str(employee_attr)

# converting categorical attributes to factor
employee_fact<- data.frame(sapply(employee_attr, function(x) factor(x)))
str(employee_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(employee_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =employee_fact))[,-1]))

#gender "male" is 1 

# Final dataset
employee_final<- cbind(employee[,c(7,8,11,19,20,22,25,26,27,28,29,30,31,32)],dummies) 
View(employee_final) #4410 obs. of  58 variables

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(employee_final$Attrition, SplitRatio = 0.7)

train = employee_final[indices,]

test = employee_final[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2147.4......nullDev 2728...resDev 2031.4

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
library(car)
vif(model_2)

#Excluding YearsAtCompany
model_3<- glm(formula = Attrition ~ -  JobInvolvement.xLow                 
              + Education.xBelow.College + EducationField.xOther + MonthlyIncome                       
              + DistanceFromHome + StockOptionLevel.xSOLevel1          
              + Education.xCollege + JobRole.xLaboratory.Technician      
              + EnvironmentSatisfaction.xVery.High + JobLevel.xJLevel2                   
              + JobRole.xResearch.Scientist + WorkLifeBalance.xBest               
              + TrainingTimesLastYear + JobRole.xResearch.Director          
              + BusinessTravel.xTravel_Rarely + JobSatisfaction.xLow                
              + JobRole.xSales.Executive + Age + WorkLifeBalance.xGood               
              + JobSatisfaction.xVery.High + Department.xResearch...Development  
              + Department.xSales + YearsWithCurrManager + TotalWorkingYears                   
              + WorkLifeBalance.xBetter + YearsSinceLastPromotion + EnvironmentSatisfaction.xLow        
              + NumCompaniesWorked + BusinessTravel.xTravel_Frequently   
              + MaritalStatus.xSingle + avg_working_hrs  , 
              family = "binomial", data = train) 

summary(model_3) 

vif(model_3) 

#cannot exclude any more variable based on vif 
#as most of them have low vif; those with higher vif are very significant and not correlated


# Excluding EducationField.xOther  
model_4<- glm(formula = Attrition ~ -  JobInvolvement.xLow                 
              + Education.xBelow.College  + MonthlyIncome                       
              + DistanceFromHome + StockOptionLevel.xSOLevel1          
              + Education.xCollege + JobRole.xLaboratory.Technician      
              + EnvironmentSatisfaction.xVery.High + JobLevel.xJLevel2                   
              + WorkLifeBalance.xBest + JobRole.xResearch.Scientist               
              + TrainingTimesLastYear + JobRole.xResearch.Director          
              + BusinessTravel.xTravel_Rarely + JobSatisfaction.xLow                
              + JobRole.xSales.Executive + Age + WorkLifeBalance.xGood               
              + JobSatisfaction.xVery.High + Department.xResearch...Development  
              + Department.xSales + YearsWithCurrManager + TotalWorkingYears                   
              + WorkLifeBalance.xBetter + YearsSinceLastPromotion + EnvironmentSatisfaction.xLow        
              + NumCompaniesWorked + BusinessTravel.xTravel_Frequently   
              + MaritalStatus.xSingle + avg_working_hrs  , 
              family = "binomial", data = train) 

summary(model_4)

#Excluding Education.xBelow.College  due to lower significance
model_5<- glm(formula = Attrition ~ -  JobInvolvement.xLow                 
                      + MonthlyIncome                       
                       + DistanceFromHome + StockOptionLevel.xSOLevel1          
                       + Education.xCollege + JobRole.xLaboratory.Technician      
                       + EnvironmentSatisfaction.xVery.High + JobLevel.xJLevel2                   
                       + WorkLifeBalance.xBest + JobRole.xResearch.Scientist               
                       + TrainingTimesLastYear + JobRole.xResearch.Director          
                       + BusinessTravel.xTravel_Rarely + JobSatisfaction.xLow                
                       + JobRole.xSales.Executive + Age + WorkLifeBalance.xGood               
                       + JobSatisfaction.xVery.High + Department.xResearch...Development  
                       + Department.xSales + YearsWithCurrManager + TotalWorkingYears                   
                       + WorkLifeBalance.xBetter + YearsSinceLastPromotion + EnvironmentSatisfaction.xLow        
                       + NumCompaniesWorked + BusinessTravel.xTravel_Frequently   
                       + MaritalStatus.xSingle + avg_working_hrs  , 
                       family = "binomial", data = train) 

summary(model_5) 

#Excluding DistanceFromHome  due to lower significance
model_6<-  glm(formula = Attrition ~ -  JobInvolvement.xLow                 
               + MonthlyIncome + StockOptionLevel.xSOLevel1          
               + Education.xCollege + JobRole.xLaboratory.Technician      
               + EnvironmentSatisfaction.xVery.High + JobLevel.xJLevel2                   
               + WorkLifeBalance.xBest + JobRole.xResearch.Scientist               
               + TrainingTimesLastYear + JobRole.xResearch.Director          
               + BusinessTravel.xTravel_Rarely + JobSatisfaction.xLow                
               + JobRole.xSales.Executive + Age + WorkLifeBalance.xGood               
               + JobSatisfaction.xVery.High + Department.xResearch...Development  
               + Department.xSales + YearsWithCurrManager + TotalWorkingYears                   
               + WorkLifeBalance.xBetter + YearsSinceLastPromotion + EnvironmentSatisfaction.xLow        
               + NumCompaniesWorked + BusinessTravel.xTravel_Frequently   
               + MaritalStatus.xSingle + avg_working_hrs  , 
               family = "binomial", data = train) 

summary(model_6)

#Excluding StockOptionLevel.xSOLevel1   due to lower significance
model_7<-   glm(formula = Attrition ~ -  JobInvolvement.xLow                 
                + MonthlyIncome + Education.xCollege + JobRole.xLaboratory.Technician      
                + EnvironmentSatisfaction.xVery.High + JobLevel.xJLevel2                   
                + WorkLifeBalance.xBest + JobRole.xResearch.Scientist               
                + TrainingTimesLastYear + JobRole.xResearch.Director          
                + BusinessTravel.xTravel_Rarely + JobSatisfaction.xLow                
                + JobRole.xSales.Executive + Age + WorkLifeBalance.xGood               
                + JobSatisfaction.xVery.High + Department.xResearch...Development  
                + Department.xSales + YearsWithCurrManager + TotalWorkingYears                   
                + WorkLifeBalance.xBetter + YearsSinceLastPromotion + EnvironmentSatisfaction.xLow        
                + NumCompaniesWorked + BusinessTravel.xTravel_Frequently   
                + MaritalStatus.xSingle + avg_working_hrs  , 
                family = "binomial", data = train)  

summary(model_7) 

#Excluding MonthlyIncome due to lower significance 
model_8<-   glm(formula = Attrition ~ -  JobInvolvement.xLow                 
                + Education.xCollege + JobRole.xLaboratory.Technician      
                + EnvironmentSatisfaction.xVery.High + JobLevel.xJLevel2                   
                + WorkLifeBalance.xBest + JobRole.xResearch.Scientist               
                + TrainingTimesLastYear + JobRole.xResearch.Director          
                + BusinessTravel.xTravel_Rarely + JobSatisfaction.xLow                
                + JobRole.xSales.Executive + Age + WorkLifeBalance.xGood               
                + JobSatisfaction.xVery.High + Department.xResearch...Development  
                + Department.xSales + YearsWithCurrManager + TotalWorkingYears                   
                + WorkLifeBalance.xBetter + YearsSinceLastPromotion + EnvironmentSatisfaction.xLow        
                + NumCompaniesWorked + BusinessTravel.xTravel_Frequently   
                + MaritalStatus.xSingle + avg_working_hrs  , 
                family = "binomial", data = train) 

summary(model_8) 

#Excluding Education.xCollege due to lower significance 
model_9<-    glm(formula = Attrition ~ -  JobInvolvement.xLow                 
                 + JobRole.xLaboratory.Technician      
                 + EnvironmentSatisfaction.xVery.High + JobLevel.xJLevel2                   
                 + WorkLifeBalance.xBest + JobRole.xResearch.Scientist               
                 + TrainingTimesLastYear + JobRole.xResearch.Director          
                 + BusinessTravel.xTravel_Rarely + JobSatisfaction.xLow                
                 + JobRole.xSales.Executive + Age + WorkLifeBalance.xGood               
                 + JobSatisfaction.xVery.High + Department.xResearch...Development  
                 + Department.xSales + YearsWithCurrManager + TotalWorkingYears                   
                 + WorkLifeBalance.xBetter + YearsSinceLastPromotion + EnvironmentSatisfaction.xLow        
                 + NumCompaniesWorked + BusinessTravel.xTravel_Frequently   
                 + MaritalStatus.xSingle + avg_working_hrs  , 
                 family = "binomial", data = train) 

summary(model_9)

#Excluding JobRole.xLaboratory.Technician due to lower significance 
model_10<-  glm(formula = Attrition ~ -  JobInvolvement.xLow       
                + EnvironmentSatisfaction.xVery.High + JobLevel.xJLevel2                   
                + WorkLifeBalance.xBest + JobRole.xResearch.Scientist               
                + TrainingTimesLastYear + JobRole.xResearch.Director          
                + BusinessTravel.xTravel_Rarely + JobSatisfaction.xLow                
                + JobRole.xSales.Executive + Age + WorkLifeBalance.xGood               
                + JobSatisfaction.xVery.High + Department.xResearch...Development  
                + Department.xSales + YearsWithCurrManager + TotalWorkingYears                   
                + WorkLifeBalance.xBetter + YearsSinceLastPromotion + EnvironmentSatisfaction.xLow        
                + NumCompaniesWorked + BusinessTravel.xTravel_Frequently   
                + MaritalStatus.xSingle + avg_working_hrs  , 
                family = "binomial", data = train) 

summary(model_10)

#Excluding JobRole.xResearch.Scientist due to lower significance 
model_11<- glm(formula = Attrition ~ -  JobInvolvement.xLow       
               + EnvironmentSatisfaction.xVery.High + JobLevel.xJLevel2                   
               + WorkLifeBalance.xBest              
               + TrainingTimesLastYear + JobRole.xResearch.Director          
               + BusinessTravel.xTravel_Rarely + JobSatisfaction.xLow                
               + JobRole.xSales.Executive + Age + WorkLifeBalance.xGood               
               + JobSatisfaction.xVery.High + Department.xResearch...Development  
               + Department.xSales + YearsWithCurrManager + TotalWorkingYears                   
               + WorkLifeBalance.xBetter + YearsSinceLastPromotion + EnvironmentSatisfaction.xLow        
               + NumCompaniesWorked + BusinessTravel.xTravel_Frequently   
               + MaritalStatus.xSingle + avg_working_hrs  , 
               family = "binomial", data = train)  

summary(model_11)

#Excluding EnvironmentSatisfaction.xVery.High due to lower significance 
model_12<- glm(formula = Attrition ~ -  JobInvolvement.xLow + JobLevel.xJLevel2                   
               + WorkLifeBalance.xBest              
               + TrainingTimesLastYear + JobRole.xResearch.Director          
               + BusinessTravel.xTravel_Rarely + JobSatisfaction.xLow                
               + JobRole.xSales.Executive + Age + WorkLifeBalance.xGood               
               + JobSatisfaction.xVery.High + Department.xResearch...Development  
               + Department.xSales + YearsWithCurrManager + TotalWorkingYears                   
               + WorkLifeBalance.xBetter + YearsSinceLastPromotion + EnvironmentSatisfaction.xLow        
               + NumCompaniesWorked + BusinessTravel.xTravel_Frequently   
               + MaritalStatus.xSingle + avg_working_hrs  , 
               family = "binomial", data = train)  

summary(model_12)

#Excluding JobLevel.xJLevel2  due to lower significance 
model_13<- glm(formula = Attrition ~ -  JobInvolvement.xLow                   
               + WorkLifeBalance.xBest              
               + TrainingTimesLastYear + JobRole.xResearch.Director          
               + BusinessTravel.xTravel_Rarely + JobSatisfaction.xLow                
               + JobRole.xSales.Executive + Age + WorkLifeBalance.xGood               
               + JobSatisfaction.xVery.High + Department.xResearch...Development  
               + Department.xSales + YearsWithCurrManager + TotalWorkingYears                   
               + WorkLifeBalance.xBetter + YearsSinceLastPromotion + EnvironmentSatisfaction.xLow        
               + NumCompaniesWorked + BusinessTravel.xTravel_Frequently   
               + MaritalStatus.xSingle + avg_working_hrs  , 
               family = "binomial", data = train) 

summary(model_13)

#Excluding JobRole.xResearch.Director due to lower significance 
model_14<- glm(formula = Attrition ~ -  JobInvolvement.xLow                   
               + WorkLifeBalance.xBest + TrainingTimesLastYear           
               + BusinessTravel.xTravel_Rarely + JobSatisfaction.xLow                
               + JobRole.xSales.Executive + Age + WorkLifeBalance.xGood               
               + JobSatisfaction.xVery.High + Department.xResearch...Development  
               + Department.xSales + YearsWithCurrManager + TotalWorkingYears                   
               + WorkLifeBalance.xBetter + YearsSinceLastPromotion + EnvironmentSatisfaction.xLow        
               + NumCompaniesWorked + BusinessTravel.xTravel_Frequently   
               + MaritalStatus.xSingle + avg_working_hrs  , 
               family = "binomial", data = train)   

summary(model_14)

#Excluding JobRole.xSales.Executive due to lower significance 
model_15<- glm(formula = Attrition ~ -  JobInvolvement.xLow                   
               + WorkLifeBalance.xBest + TrainingTimesLastYear           
               + BusinessTravel.xTravel_Rarely + JobSatisfaction.xLow                
               + Age + WorkLifeBalance.xGood               
               + JobSatisfaction.xVery.High + Department.xResearch...Development  
               + Department.xSales + YearsWithCurrManager + TotalWorkingYears                   
               + WorkLifeBalance.xBetter + YearsSinceLastPromotion + EnvironmentSatisfaction.xLow        
               + NumCompaniesWorked + BusinessTravel.xTravel_Frequently   
               + MaritalStatus.xSingle + avg_working_hrs  , 
               family = "binomial", data = train)

summary(model_15)

#Excluding WorkLifeBalance.xBest  due to lower significance 
model_16<- glm(formula = Attrition ~ -  JobInvolvement.xLow                   
               + TrainingTimesLastYear           
               + BusinessTravel.xTravel_Rarely + JobSatisfaction.xLow                
               + Age + WorkLifeBalance.xGood               
               + JobSatisfaction.xVery.High + Department.xResearch...Development  
               + Department.xSales + YearsWithCurrManager + TotalWorkingYears                   
               + WorkLifeBalance.xBetter + YearsSinceLastPromotion + EnvironmentSatisfaction.xLow        
               + NumCompaniesWorked + BusinessTravel.xTravel_Frequently   
               + MaritalStatus.xSingle + avg_working_hrs  , 
               family = "binomial", data = train) 

summary(model_16)

#Excluding WorkLifeBalance.xGood due to lower significance 
model_17<- glm(formula = Attrition ~ -  JobInvolvement.xLow                   
               + TrainingTimesLastYear           
               + BusinessTravel.xTravel_Rarely + JobSatisfaction.xLow                
               + Age + JobSatisfaction.xVery.High + Department.xResearch...Development  
               + Department.xSales + YearsWithCurrManager + TotalWorkingYears                   
               + WorkLifeBalance.xBetter + YearsSinceLastPromotion + EnvironmentSatisfaction.xLow        
               + NumCompaniesWorked + BusinessTravel.xTravel_Frequently   
               + MaritalStatus.xSingle + avg_working_hrs  , 
               family = "binomial", data = train) 

summary(model_17)



########################################################################
# With 16 significant variables in the model

final_model<- model_17

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_Attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_pred_Attrition,test_actual_Attrition)


#######################################################################
test_pred_Attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

#install.packages("e1071")
library(e1071)

test_conf <- confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes")
test_conf
#######################################################################

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual_Attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.000748 to 0.9264 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.90,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff

# Let's choose a cutoff value of 0.1628 for final model

test_cutoff_Attrition <- factor(ifelse(test_pred >=0.1628, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_Attrition, test_actual_Attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_Attrition <- ifelse(test_cutoff_Attrition=="Yes",1,0)
test_actual_Attrition <- ifelse(test_actual_Attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_Attrition, test_actual_Attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_Attrition, test_pred, groups = 10)
Attrition_decile
