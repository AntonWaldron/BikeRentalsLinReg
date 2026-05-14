# Applied Data Analysis Assignment - Linear Regression

library(ggplot2)
library(dplyr)
library(tidyverse)
library(viridis)
library(jtools)
library(car)

bike_data_full <- read.csv('C:\\Users\\kingc\\OneDrive\\Desktop\\ADA\\UniqueDataset.csv')

set.seed(500)

bike_data_full$is_holiday.f <- factor(bike_data_full$is_holiday,
                                      levels=c(0, 1),
                                      labels=c("Not Holiday", "Yes Holiday"))

bike_data_full$is_weekend.f <- factor(bike_data_full$is_weekend,
                                      levels = c(0,1),
                                      labels = c("Weekday", "Weekend"))

bike_data_full$season.f <- factor(bike_data_full$season,
                                  levels = c(0,1,2,3),
                                  labels = c("Winter", "Spring", "Summer", "Autumn"))

bike_data_full$month.f <- factor(bike_data_full$Month,
                                 levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                 labels=c("January", "February", "March", "April", "May",
                                          "June", "July", "August", "September", "October",
                                          "November", "December"))

bike_data_full$hour.f <- factor(bike_data_full$Hour, 
                                levels = c(0, 1, 2, 3, 4, 5, 6 ,7, 8, 9 , 10, 11,
                                           12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23))

bike_data_full$rain.f <- factor(bike_data_full$Rain,
                                levels = c(0, 1),
                                labels = c("Not Raining", "Raining"))

bike_data_full$id <- 1:nrow(bike_data_full)

# Use 70% for training and 30% for testing

train <- bike_data_full %>% dplyr::sample_frac(0.70)
test <- dplyr::anti_join(bike_data_full, train, by = 'id')

t1_scatterplot <- ggplot(train, aes(x=t1, y=Bikes)) +
  geom_smooth(method='lm') +
  geom_point()

t1_scatterplot

# Season and Bikes boxplot with colors
train %>%
  ggplot(aes(x = season.f, y = Bikes, fill = season.f)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("Spring" = "green", "Summer" = "yellow", "Autumn" = "orange", "Winter" = "lightblue")) +
  ggtitle("Season and Bikes boxplot") +
  xlab("") +
  theme_minimal()


# Hour and Bikes boxplot with colors
train %>%
  ggplot(aes(x = hour.f, y = Bikes, fill = hour.f)) + 
  geom_boxplot() +
  ggtitle("Hour and Bikes boxplot") +
  xlab("") +
  theme_minimal()

# Rain and Bikes boxplot with colors
train %>%
  ggplot(aes(x = rain.f, y = Bikes, fill = rain.f)) + 
  geom_boxplot() +
  ggtitle("Rain and Bikes boxplot") +
  xlab("") +
  theme_minimal()

# Season and rain
ggplot(train, aes(x=season.f, y=Bikes, color=rain.f)) +
  geom_point() + 
  scale_color_manual(values = c("Not Raining"="black", "Raining"="red"))

# Checking for interaction between hour and rain
ggplot(bike_data_full, aes(x=hour.f, y=Bikes, color=rain.f)) +
  geom_point() +
  scale_color_manual(values = c("Not Raining"="black", "Raining"="red"))

# Checking for interaction between hour and season
ggplot(bike_data_full, aes(x=hour.f, y=Bikes, color=season.f)) +
  geom_point() +
  scale_color_manual(values= c("Winter"="blue",
                               "Spring"="green", 
                               "Summer"="yellow", 
                               "Autumn"="brown" ))

# Does the effect of temp change based on the hour of the day
hour_colours <- setNames(rainbow(24), as.character(0:23))

ggplot(train, aes(x=t1, y=Bikes, color=hour.f)) +
  geom_point() + 
  scale_color_manual(values=hour_colours) +
  labs(color = "Hour")

#Scatterplot
pairs(train[,c("Bikes", "t1", "t2","wind_speed","hum")])

#head(train)

# Rain and Bikes boxplot with colors #, fill = rain.f)
train %>%
  ggplot(aes(x = is_holiday.f, y = Bikes, fill=is_holiday.f)) + 
  geom_boxplot() +
  ggtitle("Holiday and Bikes boxplot") +
  xlab("") +
  theme_minimal()


# Rain and Bikes boxplot with colors
train %>%
  ggplot(aes(x = is_weekend.f, y = Bikes, fill=is_weekend.f)) + 
  geom_boxplot() +
  ggtitle("Weekend and Bikes boxplot") +
  xlab("") +
  theme_minimal()


#Set the color values for "Weekend" and "Weekday" (assuming levels are "0" and "1")
weekend_colors <- c("Weekday" = "Blue", "Weekend" = "Red")

# Use the correct factor column in ggplot
ggplot(train, aes(x = t1, y = Bikes, color = is_weekend.f)) +
  geom_point() + 
  scale_color_manual(values = weekend_colors) + 
  labs(color = "Weekend Status")

# Holiday scatterplot 
holiday_colours <- c("0" = "Blue", "1" = "Red")

ggplot(bike_data_full, aes(x=t1, y=Bikes, color=as.factor(is_holiday))) +
  geom_point() + 
  scale_color_manual(values=holiday_colours) +
  labs(color = "Holidays")

#Humidty weekend interaction
ggplot(train, aes(x = hum, y = Bikes, color = is_weekend.f)) +
  geom_point() + 
  scale_color_manual(values = weekend_colors) + 
  labs(color = "Weekend Status")

#Wind speed weekend interaction
ggplot(train, aes(x = wind_speed, y = Bikes, color = is_weekend.f)) +
  geom_point() + 
  scale_color_manual(values = weekend_colors) + 
  labs(color = "Weekend Status")

# First model
lm_v1 <- lm(Bikes ~ hum + season.f + rain.f + hour.f + is_weekend.f:hum + is_weekend.f:t1, data = train)
Anova(lm_v1, type = 'III')
summ(lm_v1, vif=TRUE)

# Second model
# No interaction
lm_v2 <- lm(Bikes ~ hum + season.f + rain.f + hour.f + is_weekend.f:hum, data = train)
Anova(lm_v2, type = 'III')
summ(lm_v2, vif=TRUE)

# Partial f - test

with_interaction <- lm(Bikes ~ hum + season.f + rain.f + hour.f + is_weekend.f:hum, data = train)
no_interaction <-lm(Bikes ~ hum + season.f + rain.f + hour.f, data = train)

anova(no_interaction, with_interaction)

# Final model and residual analysis
model <- lm(Bikes ~ hum + season.f + rain.f + hour.f + is_weekend.f:hum, data = train)
plot(model)

hist(rstandard(model))

library(MASS)

b<- boxcox(model)

lambda <- b$x[which.max(b$y)]

print(lambda)
# Lambda is almost 0 so we'll just do a log transform instead

# Log model
log_model <- lm(log(Bikes) ~ hum + hour.f + season.f +rain.f + hum:is_weekend, data=train)
summary(log_model)
plot(log_model)

hist(log_model$residuals)

#Predicted vs Actual plot
summary(log_model, vif=TRUE)
plot(x=((predict(log_model, newdata=test))), y= log(test$Bikes),
     xlab='Predicted log(Bikes) ',
     ylab='Actual log(Bikes)',
     main='Predicted vs. Actual Values')
abline(a=0, b=1, col = 'red')

#Calculating residuals and MSE

# Calculate residuals 
residuals <- log(test$Bikes) - predict(log_model, newdata=test)

# Compute MSE
mse <- mean(residuals^2)
print(mse)
