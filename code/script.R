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