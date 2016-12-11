# Exploration of NBA Shot Data
# Author: Matt Hixon
# CIS 490: BIG DATA ANALYTICS

setwd("/Users/matthixon/Documents/K-State/Fall 2016/CIS 490/Final Project/dataset/NBA-Shot-Analysis")

library(ggplot2)
library(dplyr)

shots <- read.csv("shot_logs.csv", stringsAsFactors = FALSE, header = T)

# Feature Engineering: Shot Type
# Inspired from: 
# https://www.kaggle.com/slangenborg/d/dansbecker/nba-shot-logs/analyzing-the-best-defenders-in-the-nba

shots$SHOT_TYPE[shots$DRIBBLES <= 1 & shots$SHOT_DIST  > 4] = 'Catch and Shoot'
shots$SHOT_TYPE[shots$DRIBBLES <= 1 & shots$SHOT_DIST <= 4] = 'Cut'
shots$SHOT_TYPE[shots$DRIBBLES > 1 & shots$SHOT_DIST  <= 4] = 'Drive'
shots$SHOT_TYPE[shots$DRIBBLES > 4] = 'ISO/Post up'
shots$SHOT_TYPE[shots$DRIBBLES > 20] = 'Long ISO'
shots$SHOT_TYPE[shots$DRIBBLES <=1 & shots$PTS_TYPE == 3] = 'Spot Up Three'
shots$SHOT_TYPE[is.na(shots$SHOT_TYPE)] = 'Unknown'
shots$SHOT_TYPE = factor(shots$SHOT_TYPE)

# Feature Engineering: Shot Quality
# Inspired from: 
# https://www.kaggle.com/slangenborg/d/dansbecker/nba-shot-logs/analyzing-the-best-defenders-in-the-nba

shots$SHOT_QUALITY = 'Other'
shots$SHOT_QUALITY[shots$CLOSE_DEF_DIST <= 2.0] = 'Tightly Contested'
shots$SHOT_QUALITY[shots$CLOSE_DEF_DIST > 2.0 & shots$CLOSE_DEF_DIST <= 3.0] = 'Contested'
shots$SHOT_QUALITY[shots$CLOSE_DEF_DIST > 3.0 & shots$CLOSE_DEF_DIST <= 5.0] = 'Open'
shots$SHOT_QUALITY[shots$CLOSE_DEF_DIST > 5.0] = 'Wide Open'
shots$SHOT_QUALITY = factor(shots$SHOT_QUALITY)

# Feature Engineering: Game Time Partition

shots$PERIOD <- as.factor(shots$PERIOD)

shots$GAME_CLOCK_PARTITION <- 'UNKNOWN'
shots$GAME_CLOCK_PARTITION[shots$GAME_CLOCK >= '9:00'] = 'Beginning'
shots$GAME_CLOCK_PARTITION[shots$GAME_CLOCK < '9:00' & shots$GAME_CLOCK >= '6:00'] = 'Early Middle'
shots$GAME_CLOCK_PARTITION[shots$GAME_CLOCK < '6:00' & shots$GAME_CLOCK >= '3:00'] = 'Late Middle'
shots$GAME_CLOCK_PARTITION[shots$GAME_CLOCK < '3:00'] = 'End'
shots$GAME_CLOCK_PARTITION = factor(shots$GAME_CLOCK_PARTITION)

# Feature Engineering: Game Clock
# Inspired from: 
# https://www.kaggle.com/grejsegura/d/dansbecker/nba-shot-logs/shot-analysis-field-goal-prediction-xgboost/comments

GAME_CLOCK <- strptime(shots$GAME_CLOCK, format = '%M:%S')
clock <- GAME_CLOCK$min * 60 + GAME_CLOCK$sec
shots <-cbind(shots,clock)
shots$SHOT_CLOCK[is.na(shots$SHOT_CLOCK)] <- shots$clock[is.na(shots$SHOT_CLOCK)]

# Feature Engineering: Shot Clock

shots$SHOT_CLOCK_PARTITION <- 'UNKNOWN'
shots$SHOT_CLOCK_PARTITION[shots$SHOT_CLOCK >= 24] = 'Beginning'
shots$SHOT_CLOCK_PARTITION[shots$SHOT_CLOCK < 24 & shots$SHOT_CLOCK >= 20] = 'Very Early'
shots$SHOT_CLOCK_PARTITION[shots$SHOT_CLOCK < 20 & shots$SHOT_CLOCK >= 15] = 'Early'
shots$SHOT_CLOCK_PARTITION[shots$SHOT_CLOCK < 15 & shots$SHOT_CLOCK >= 10] = 'Middle'
shots$SHOT_CLOCK_PARTITION[shots$SHOT_CLOCK < 10 & shots$SHOT_CLOCK >= 5] = 'Late'
shots$SHOT_CLOCK_PARTITION[shots$SHOT_CLOCK < 5 & shots$SHOT_CLOCK >= 2] = 'Very Late'
shots$SHOT_CLOCK_PARTITION[shots$SHOT_CLOCK < 2] = 'Forced'
shots$SHOT_CLOCK_PARTITION = factor(shots$SHOT_CLOCK_PARTITION)

# Feature Engineering: Shot Distance

shots$SHOT_DIST_PARTITION <- 'UNKNOWN'
shots$SHOT_DIST_PARTITION[shots$SHOT_DIST >= 30] = 'Prayer'
shots$SHOT_DIST_PARTITION[shots$SHOT_DIST < 30 & shots$SHOT_DIST >= 25] = 'Deep 3'
shots$SHOT_DIST_PARTITION[shots$SHOT_DIST < 25 & shots$SHOT_DIST >= 22] = '3 Pointer'
shots$SHOT_DIST_PARTITION[shots$SHOT_DIST < 22 & shots$SHOT_DIST >= 15] = 'Deep 2'
shots$SHOT_DIST_PARTITION[shots$SHOT_DIST < 15 & shots$SHOT_DIST >= 8] = 'Mid Range'
shots$SHOT_DIST_PARTITION[shots$SHOT_DIST < 8 & shots$SHOT_DIST >= 3] = 'Close'
shots$SHOT_DIST_PARTITION[shots$SHOT_DIST < 3] = 'Dunk'
shots$SHOT_DIST_PARTITION = factor(shots$SHOT_DIST_PARTITION)

# Visualization 1: Shot Distance vs. FG%
shot_dist_percent <- shots %>%
  group_by(SHOT_DIST) %>%
  summarise(PERCENT_MADE = (sum(FGM)/length(FGM))*100)

plot <- ggplot(data = shot_dist_percent, aes(x = SHOT_DIST, y = PERCENT_MADE, fill = SHOT_DIST)) + geom_line(stat = 'identity') + xlab("Shot Distance") + ylab("FG%")
plot

# Visualization 2: Shot Clock vs. FG%

shot_clock_pct <- aggregate(FGM ~ SHOT_CLOCK_PARTITION, data = shots, mean)

plot <- ggplot(data = shot_clock_pct, aes(x = SHOT_CLOCK_PARTITION, y = FGM)) + geom_bar(stat = 'identity') + xlab("Shot Clock Time") + ylab("FG%")
plot

# Visualization 3: Shot Type vs. FG%

shot_type_perc <- shots %>%
  group_by(SHOT_TYPE) %>%
  summarise(PERCENT = (sum(FGM)/length(FGM))*100)

plot <- ggplot(data = shot_type_perc, aes(x = SHOT_TYPE, y = PERCENT, fill = SHOT_TYPE)) + geom_bar(stat = 'identity') + xlab("Shot Type") + ylab("FG%")
plot

# Visualization 4: Shot Quality vs. FG%

shot_quality_perc <- shots %>%
  group_by(SHOT_QUALITY) %>%
  summarise(PERCENT = (sum(FGM)/length(FGM))*100)

plot <- ggplot(data = shot_quality_perc, aes(x = SHOT_QUALITY, y = PERCENT, fill = SHOT_QUALITY)) + geom_bar(stat = 'identity') + xlab("Shot Quality") + ylab("FG%")
plot

# Visualization 5: Shot Type vs. FG% (with respect to Shot Quality)

shot_type_qualtiy_pct <- aggregate(FGM ~ SHOT_TYPE + SHOT_QUALITY, data = shots, mean)

plot <- ggplot(shot_type_qualtiy_pct,aes(x=SHOT_TYPE,y=FGM*100,fill=SHOT_QUALITY))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Shot Quality",
                      breaks=c("Contested", "Open", "Tightly Contested", "Wide Open"),
                      labels=c("Contested", "Open", "Tightly Contested", "Wide Open"))+
  xlab("Shot Type")+ylab("FG%")
plot

# Visualization 6: Period vs. FG% (with respect to Game Clock)

FGM_PERIOD_TIME_SUM <- aggregate(FGM ~ PERIOD + GAME_CLOCK_PARTITION, data = shots, sum)

plot <- ggplot(FGM_PERIOD_TIME_SUM,aes(x=PERIOD,y=(FGM/length(unique(shots$GAME_ID))),fill=GAME_CLOCK_PARTITION))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Time in Period",
                      breaks=c("Beginning", "Early Middle", "End", "Late Middle"),
                      labels=c("Beginning", "Early Middle", "End", "Late Middle"))+
  xlab("Period")+ylab("Average FG Attempts per Game")
plot

# End of Exploration
#######################
# Model

library(caret)
library(randomForest)
smp_size <- floor(0.75 * nrow(shots))
set.seed(123)
train_ind <- sample(seq_len(nrow(shots)), size = smp_size)
train <- shots[train_ind, ]
test <- shots[-train_ind, ]

### Model: Random Forest

# Data Preperation
trainx <- data.frame(train$SHOT_CLOCK_PARTITION,train$SHOT_QUALITY,train$SHOT_TYPE,train$GAME_CLOCK_PARTITION,train$DRIBBLES,train$TOUCH_TIME)
trainy <- as.factor(train$FGM)
testx <- data.frame(test$SHOT_CLOCK_PARTITION,test$SHOT_QUALITY,test$SHOT_TYPE,test$GAME_CLOCK_PARTITION,test$DRIBBLES,test$TOUCH_TIME)
testy <- as.factor(test$FGM)

# Tuning Variables
tuneRF(trainx, trainy, stepFactor=0.1)

# Train Model
fit <- randomForest(x=trainx, y=trainy,xtest=testx,ytest=testy,
                    data=train, method="class",importance = TRUE, ntree = 300)

# Results from Model

plot(fit)
importance(fit, type=2)
print(fit)
varImpPlot(fit)

# Random Model
# OOB-Estimate -> 39.38%
# Actual Error -> 39.17%

# Accuracy -> 60.83%

### End of Model