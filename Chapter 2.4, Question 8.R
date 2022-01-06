
##set the dir
setwd('C:/Users/96654/Downloads/BA/R lab exercise1/Data_file')

## read csv file
college <- read.csv("./College.csv")
fix(college)

rownames(college)=college[ ,1]
fix(college)
college = college[,-1]
fix(college)

summary(college)

college[,1] = as.numeric(factor(college[,1]))

pairs(college[,1:10])


boxplot(college$Outstate ~ college$Private, xlab = "Private", ylab ="Out of State", main = "Outstate VS Private")


Elite <- rep("No", nrow(college ))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)

summary(college$Elite)

plot(college$Elite ,college$Outstate, xlab = "Elite", ylab ="Out of State", main = "Outstate VS Elite")

par(mfrow = c(2,2))
hist(college$Books, xlab = "Books", ylab = "Count", main = "Histogram of Books")
hist(college$Grad.Rate,  xlab = "Grad Rate", ylab = "Count", main = "Histogram of Grad rate")
hist(college$perc.alumni, xlab = "% alumni", ylab = "Count", main = "Histogram of Alumni")
hist(college$PhD, xlab = "PhD", ylab = "Count", main = "Histogram of PhD")


ElData <- college[college$Elite == "Yes", ]
NonElData  <- college[college$Elite == "No", ]

par(mfrow = c(2,2))
hist(NonElData$PhD, xlab = "PhD", ylab = "Count", main = "PhD at Non Elite")
hist(ElData$PhD, xlab = "PhD", ylab = "Count", main = "PhD at Elite")
hist(NonElData$perc.alumni, xlab = "perc.alumni", ylab = "Count", main = "Perc.alumni at Non Elite")
hist(ElData$perc.alumni, xlab = "perc.alumni", ylab = "Count", main = "Perc.alumni at Non Elite")

