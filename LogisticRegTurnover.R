# Logistic Regresssion Project
set.seed(123)
library(jtools)
turnover <- read.csv('\\Desktop\\ADA\\Staff_Turnover.csv') # Example Link

head(turnover)



# Data manipulation

turnover$Attrition.f <- factor(turnover$Attrition,
                                  levels = c(0,1),
                                  labels = c('Stayed','Left'))
turnover$BusinessTravel.f <- factor(turnover$BusinessTravel,
                                       levels = c(2,3,4),
                                       labels = c('Travel Frequently','Travel Rarely', 
                                                 'No Travel'))

turnover$Department.f <- factor(turnover$Department,
                                   levels = c(1,2,3),
                                   labels= c('HR', 'R&D', 'Sales'))

turnover$Gender.f <- factor(turnover$Gender,
                               levels = c(1,2),
                               labels = c('Female', 'Male'))

turnover$JobSatisfaction.f <- factor(turnover$JobSatisfaction,
                                        levels = c(0,1,2),
                                        labels = c('Low', 'Medium', 'High'))

#Make an 80% training set and 20% testing set

#sample <- sample(c(TRUE,FALSE), nrow(turnover), replace=TRUE, prob=c(0.8,0.2))
#train <- turnover[sample,]
#test <- turnover[!sample,]

glm_model <- glm(Attrition.f ~ BusinessTravel.f + Department.f + Gender.f +
                 + JobSatisfaction.f + EmployeeID + HourlyRate + DistanceFromHome +
                 + Age, data=turnover, family = binomial)                              

summary(glm_model)
                                       
#significant are Business Travel, Job Satisfaction, Age, Distance from home

# Exponentiate coefficients to get Adjusted Odds Ratios
exp(coef(glm_model))
# Obtain confidence intervals and exponentiate
exp(confint(glm_model))

summ(glm_model)
                                    
