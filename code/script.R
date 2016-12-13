## Set SPARK_HOME environment variable if it has not been specified before
if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
    Sys.setenv(SPARK_HOME = "C:\\spark-2.0.2-bin-hadoop2.7")
}

## Load libraries and create Spark session
library(dpl)
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
sparkR.session(master = "local[*]", sparkConfig = list(spark.driver.memory = "2g"))

## Load data
trainData <- read.csv("./data/train.csv", stringsAsFactors = FALSE)
testData <- read.csv("./data/test.csv", stringsAsFactors = FALSE)
str(trainData)

## Data visualizations
## Visualize distribution of passengers by their age, fare, sex and class
jpeg(filename = "./figures/passengerDistribution.jpg")
par(mfrow = c(2,2))
plot(density(trainData$Age, na.rm = TRUE), xlab = "Age", main = "Density of passengers by age")
plot(density(trainData$Fare, na.rm = TRUE), xlab = "Fare", main = "Density of passengers by fare")
sexCount <- table(trainData$Sex)
sexVisualization <- barplot(sexCount, xlab = "Sex", main = "Number of passengers by sex")
text(x = sexVisualization, y = sexCount - 25, label = sexCount)
classCount <- table(trainData$Pclass)
classVisualization <- barplot(classCount, xlab = "Class", main = "Number of passengers by their class")
text(x = classVisualization, y = classCount - 25, label = classCount)
dev.off()
par(mfrow=c(1,1))

## Data visualization comparing survival depending on different passenger characteristics
jpeg(filename = "./figures/passengerSurvivalComparison.jpg", width = 1200, height = 1200, units = "px",
     quality = 100)
par(mfrow = c(3,1))
# Survival by age
ageSurvivalCounts <- table(trainData$Survived, trainData$Age)
rownames(ageSurvivalCounts) <- c("Died", "Survived")
barplot(ageSurvivalCounts, xlab = "Age", ylab = "Number of people", 
        main = "Survival by age", legend.text=TRUE)
#Survival by sex
sexSurvivalCounts <- table(trainData$Survived, trainData$Sex)
rownames(sexSurvivalCounts) <- c("Died", "Survived")
sexSurvivalPlot <- barplot(sexSurvivalCounts, xlab = "Sex", ylab = "Number of people", 
        main = "Survival by sex", legend.text=TRUE, cex.axis = 1.5)
sexSurvivalLabels = sexSurvivalCounts["Died",] / colSums(sexSurvivalCounts)
sexSurvivalLabels = paste(sexSurvivalLabels * 100, "% (Died)")
text(x = sexSurvivalPlot, y = 45, label = sexSurvivalLabels, cex = 1.5)
#Survival by class
classSurvivalCounts <- table(trainData$Survived, trainData$Pclass)
rownames(classSurvivalCounts) <- c("Died", "Survived")
classSurvivalPlot <- barplot(classSurvivalCounts, xlab = "Class", ylab = "Number of people", 
        main = "Survival by class", legend.text=TRUE)
classSurvivalLabels = classSurvivalCounts["Died",] / colSums(classSurvivalCounts)
classSurvivalLabels = paste(classSurvivalLabels * 100, "% (Died)")
text(x = classSurvivalPlot, y = 45, label = classSurvivalLabels, cex = 1.5)
dev.off()
par(mfrow=c(1,1))

## Feature Engineering
testData <- cbind(testData, Survived = rep(NA, nrow(testData)))
fullData <- rbind(trainData, testData)
fullData$Surname <- sapply(fullData$Name, function(x) strsplit(x, split = "[.,]")[[1]][1])
fullData$FSize <- fullData$SibSp + fullData$Parch + 1

# Visualize if familis survive or die together
library(ggplot2)
jpeg(filename = "./figures/survivalByFamilySize.jpg", quality = 100)
ggplot(fullData[!is.na(fullData$Survived),], aes(x = FSize, fill = factor(Survived))) + 
    geom_bar(stat = "count", position = "dodge") + 
    scale_x_continuous(breaks = c(1:11)) + 
    labs(x = "Family Size")
dev.off()
jpeg(filename = "./figures/mosaicSurvivalFamilyClass.jpg", quality = 100)
fullData$FamilyClass[fullData$FSize == 1] <- "Singleton"
fullData$FamilyClass[fullData$FSize < 5 & fullData$FSize > 1] <- "Small"
fullData$FamilyClass[fullData$FSize >= 5] <- "Large"
# Visualize survival by family class
mosaicplot(table(fullData$FamilyClass, fullData$Survived), main = "Family size by Survival",
           shade = TRUE)
dev.off()

## Obtain the Deck of each passenger
fullData$Deck <- sapply(fullData$Cabin, function(x) strsplit(x, NULL)[[1]][1])
jpeg(filename = "./figures/survivalByDeck.jpg", quality = 100)
ggplot(fullData[!is.na(fullData$Survived),], aes(x = Deck, fill = factor(Survived))) +
    geom_bar(stat = "count", position = "dodge") +
    labs(x = "Deck")
dev.off()


## MISSING VALUES
# 2 missing embarkment
jpeg(filename = "./figures/embarkmentFareNAs.jpg", quality = 100)
embark_fare <- fullData[fullData$PassengerId != 62 & fullData$PassengerId != 830,]
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
    geom_boxplot() +
    geom_hline(aes(yintercept = 80), colour = 'red', linetype = 'dashed', lwd = 2)
dev.off()
# Median fare for First class with Embarkment C is 80, so we assign this value to NAs
fullData$Embarked[is.na(fullData$Embarked)] <- 'C'

# A passenger is missing the Fare attribute. We'll see similar passengers
similar <- fullData[fullData$Pclass == '3' & fullData$Embarked == 'S',]
jpeg(filename = "./figures/ClassFareNAs.jpg", quality = 100)
ggplot(similar, aes(x = Fare)) + 
    geom_density(fill = '#99d6ff', alpha = .4) + 
    geom_vline(aes(xintercept = median(Fare, na.rm = TRUE)),
               colour = 'red', linetype = 'dashed', lwd = 1)
dev.off()
# We assign the median to the Fare NA

fullData$Fare[is.na(fullData$Fare)] <- median(similar$Fare, na.rm = TRUE)

## Imputation using prediction
# Make factor variables into factors
factor_vars <- c('PassengerId', 'Pclass', 'Sex', 'Embarked', 'Title', 'Surname',
                 'Family', 'FSize')
fullData[factor_vars] <- lapply(fullData[factor_vars], function(x) as.factor(x))
set.seed(1234)
library(mice)
# Perform mice imputation with useful variables
mice_mod <- mice(fullData[, !names(fullData) %in% c('PassengerId', 'Name', 'Ticket', 
                                                    'Cabin', 'Family', 'Survived')], 
                 method = 'rf')
mice_output <- complete(mice_mod)
#Visualize original distribution of passengers age with predicted one
jpeg(filename = "./figures/compareAgeDistribution.jpg", quality = 100)
par(mfrow = c(1, 2))
hist(fullData$Age, freq = F, main = "Age: Original Data", col = "darkgreen", ylim = c(0, .04))
hist(mice_output$Age, freq = F, main = "Age: MICE output", col = "lightgreen", ylim = c(0, .04))
dev.off()

# Replace missing values with predicted ones
fullData$Age <- mice_output$Age