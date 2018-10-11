#2018 Statcast Batted Ball Data

#Get the data that we're interested in
S1 <- read.csv("/Users/Owner/Documents/BaseballBookRData/BookWork/2018 Statcast Data/September 18/Sept1_Sept8.csv")
S2 <- read.csv("/Users/Owner/Documents/BaseballBookRData/BookWork/2018 Statcast Data/September 18/Sept9_Sept15.csv")
S3 <- read.csv("/Users/Owner/Documents/BaseballBookRData/BookWork/2018 Statcast Data/September 18/Sept16_Sept22.csv")
S4 <- read.csv("/Users/Owner/Documents/BaseballBookRData/BookWork/2018 Statcast Data/September 18/Sept23_Sept30.csv")

#Combine all files to a single data frame
SeptemberData <- rbind(S1, S2, S3, S4)

#Select the specific data that I want
BattedBallData <- SeptemberData %>%
  select(launch_speed, launch_angle, bb_type, events)

#Look at data
str(BattedBallData)

#need to change "launch_angle" and "launch_speed" from factors to numeric values
BattedBallData$launch_speed <- as.numeric(as.character(BattedBallData$launch_speed))
BattedBallData$launch_angle <- as.numeric(as.character(BattedBallData$launch_angle))

#Clean our data by getting rid of all NA's
BattedBallData <- na.omit(BattedBallData)

#*****Identify all of the different "values" in the events columns using SQLdf****
#******************************************************************************
library(sqldf)
b <- sqldf('SELECT DISTINCT BattedBallData.bb_type FROM BattedBallData')
b

#Identify the batted ball types we want
good_columns <- c('line_drive', 'fly_ball', 'ground_ball', 'popup')
BattedBallData <- BattedBallData[BattedBallData$bb_type %in% good_columns,]

BattedBallData$bb_type <- factor(BattedBallData$bb_type)

head(BattedBallData)

#Random Forest Setup
library(caret)
inTrain <- createDataPartition(BattedBallData$bb_type,p=0.7,list=FALSE)
training <- BattedBallData[inTrain,]
testing <- BattedBallData[-inTrain,]

method <- 'rf'
#train the model
ctrl <- trainControl(method = 'repeatedcv', number = 5, repeats = 5)
modelFit <- train(bb_type ~., method=method, data=training, trControl=ctrl)

#Run the model on the test set
predicted <- predict(modelFit,newdata=testing)
# Check out the confusion matrix
confusionMatrix(predicted, testing$bb_type)

#Plot our model
install.packages('plotly')
library(plotly)
# Exit velocities from 40 to 120
x <- seq(40,120,by=1)
# Hit angles from 10 to 50
y <- seq(10,50,by=1)
# Make a data frame of the relevant x and y values
plotDF <- data.frame(expand.grid(x,y))
# Add the correct column names
colnames(plotDF) <- c('launch_speed','launch_angle')
# Add the classification
plotPredictions <- predict(modelFit,newdata=plotDF)
plotDF$pred <- plotPredictions

p <- plot_ly(data=plotDF, x=~launch_speed, y = ~launch_angle, color=~pred, type="scatter", mode="markers") %>%
  layout(title = "Exit Velocity + Launch Angle Outcomes")
p




