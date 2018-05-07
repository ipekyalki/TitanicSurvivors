#Load the data
train <- read.csv("train.csv")
test <- read.csv("test.csv")
train.new <- train[, c(2,1,3,4,5,6,7,8,9,10,11,12)]

#Equate train and train.new
train <- train.new

#Add a "Survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

#Combine data sets 
data.combined <- rbind(train.new, test.survived)

#Check out the structure
str(data.combined)

data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)


#Take a look at the survival rates
table(data.combined$Survived)

#Distribution of classes
table(data.combined$Pclass)

#load up ggplot2 package to use for data visualization
library(ggplot2)

#Hypothesis - Rich folks survived at a higher rate
train.new$Pclass <- as.factor(train.new$Pclass)
ggplot(train,aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar(width = 0.5) +
  xlab("Plass") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Examine the first few names in the training data set
head(as.character(train$Name))

#How manny unique names are they across both train & test?
length(unique(as.character(data.combined$Name)))

#Result = 1307 which means probably there are 2 duplicate names
#Now, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

#Take a look at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]

#What's up wth Mr. and Miss. thing?
library(stringr)

#Any correlation between the other variables like sibsp or parch?
misses <- data.combined[which(str_detect(data.combined$Name,"Miss")),]
misses[1:5,]

#Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
mrses[1:5,]

#Check male to see if the pattern continues
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]



#Expand upan the relationship between the "Survived" and "Pclass" by adding new 'Title' variable to the
#dataset and then explore a potential 3-dimensional relationship

#create a utility function to help with title extraction
extractTitle <- function(Name){
  Name <- as.character(Name)
  
  if (length(grep("Miss.", Name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Master.", Name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }}

titles <- NULL
for (i in 1:nrow(data.combined)){
  titles <- c(titles,extractTitle(data.combined[i,"Name"]))
}
data.combined$title <- as.factor(titles)
data.combined$titles <- NULL #delete the extra "titles" column

#Since we only have survived labels in the train set, only use first 891 observations
ggplot(data.combined[1:891,], aes(x=title, fill = Survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill = "Survived")


#What's the distribution of females to males across the train&test?
table(data.combined$Sex)

#Visualize 3-way relationship of sex, pclass, and survival compare to analysis
ggplot(data.combined[1:891,], aes(x=Sex, fill=Survived))+
  geom_bar(width = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("Total Count")+
  labs(fill="Survived")

#OK, age and sex seem pretty important as derived from the analysis of title,
#let's take a look at the distribution of age over entire data set
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

#Take a look at the survival rates broken out by sex, pclass and age
ggplot(data.combined[1:891,], aes(x=Age, fill=Survived))+
  geom_histogram(binwidth = 10)+
  facet_wrap(~Sex + Pclass)+
  xlab("Age")+
  ylab("Total Count")

#Validate that "Master. is a good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)

#We know "Misses" is a lot more complicated, let's take a look at that
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x=Age, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass)+
  ggtitle("Age for Miss by Pclass")+
  xlab("Age")+
  ylab("Total Count")

#OK,it appears that female children may have different survival rates
#could be a candidate for feature engineering later
misses_alone <- data.combined[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses_alone$Age)
length(which(misses_alone$Age <= 14.5))

#Summarize the sibsp variable
summary(data.combined$SibSp)

#Can we treat it as a factor?
length(unique(data.combined$SibSp))

#convert it to a factor
data.combined$SibSp <- as.factor(data.combined$SibSp)

#We believe title is predictive, visualize survival rates by pclass, sipsp and title
ggplot(data.combined[1:891,], aes(x=SibSp, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("SibSp")+
  ylab("Total Count")+
  labs(fill="Survived")

#Treat parch variable as a factor and visualize
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x=Parch, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("Parch")+
  ylab("Total Count")+
  labs(fill="Survived")

#Feature engineering. What about creating a family size feature?
team.sibsp <- c(train$SibSp, test$SibSp)
team.parch <- c(train$Parch, test$Parch)
data.combined$family.size <- as.factor(team.sibsp + team.parch +1)

#Visualize it if it's predictive
ggplot(data.combined[1:891,], aes(x=family.size, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + title)+
  ggtitle("Pclass, Title")+
  xlab("family.size")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

