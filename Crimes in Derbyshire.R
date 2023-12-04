install.packages("psych")
library(psych)

#Importing the dataset
library(readr)
AssessmentCrimeData <- read.csv("C:/Users/USER/Desktop/Big Data/Information visualization/R lang/AssessmentCrimeData.csv")

# checking the dataset structure
str(AssessmentCrimeData)

# Summary of the dataset
summary(AssessmentCrimeData)

# Checking the standard deviation 

sd(AssessmentCrimeData$Anti.Social.Behaviour)
sd(AssessmentCrimeData$Burglary)
sd(AssessmentCrimeData$Robbery)
sd(AssessmentCrimeData$Vehicle.Crimes)
sd(AssessmentCrimeData$Violent.Crimes)
sd(AssessmentCrimeData$Shoplifting)
sd(AssessmentCrimeData$Criminal.Damage...Arson)
sd(AssessmentCrimeData$Other.Theft)
sd(AssessmentCrimeData$Drugs)
sd(AssessmentCrimeData$Other.Crimes)
sd(AssessmentCrimeData$Bike.Theft)
sd(AssessmentCrimeData$Possession.of.Weapons)
sd(AssessmentCrimeData$Public.Order)
sd(AssessmentCrimeData$Theft.From.the.Person)

describe(AssessmentCrimeData)

#Box plots for Population vs 14 crime indicators
par(mfrow= c(2,3))
boxplot(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Anti.Social.Behaviour), main = "Anti Social Behaviour", col = "green")
boxplot(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Burglary), main = "Burglary", col = "red")
boxplot(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Robbery), main = "Robbery", col = "blue")
boxplot(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Vehicle.Crimes), main = "Vehicle Crimes", col = "orange")
boxplot(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Violent.Crimes), main = "Violent Crimes", col = "red")
boxplot(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Shoplifting), main = "Shoplifting", col = "red")
boxplot(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Criminal.Damage...Arson), main = "Criminal Damage and Arson", col = "green")
boxplot(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Other.Theft), main = "Other Theft", col = "red")
boxplot(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Drugs), main = "Drugs", col = "green")
boxplot(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Other.Crimes), main = "Other Crimes", col = "orange")
boxplot(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Bike.Theft), main = "Bike Theft", col = "red")
boxplot(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Possession.of.Weapons), main = "Possession of Weapons", col = "blue")
boxplot(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Public.Order), main = "Public Order", col = "green")
boxplot(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Theft.From.the.Person), main = "Theft From the Person", col = "red")


# histograms of Population vs 14 crime indicators

par(mfrow= c(2,3))
hist(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Anti.Social.Behaviour), main = "Anti Social Behaviour", col = "green")
hist(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Burglary), main = "Burglary", col = "red")
hist(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Robbery), main = "Robbery", col = "blue")
hist(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Vehicle.Crimes), main = "Vehicle Crimes", col = "orange")
hist(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Violent.Crimes), main = "Violent Crimes", col = "red")
hist(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Shoplifting), main = "Shoplifting", col = "red")
hist(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Criminal.Damage...Arson), main = "Criminal Damage and Arson", col = "green")
hist(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Other.Theft), main = "Other Theft", col = "red")
hist(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Drugs), main = "Drugs", col = "green")
hist(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Other.Crimes), main = "Other Crimes", col = "orange")
hist(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Bike.Theft), main = "Bike Theft", col = "red")
hist(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Possession.of.Weapons), main = "Possession of Weapons", col = "blue")
hist(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Public.Order), main = "Public Order", col = "green")
hist(log10(AssessmentCrimeData$Population / AssessmentCrimeData$Theft.From.the.Person), main = "Theft From the Person", col = "red")


attach(AssessmentCrimeData)

#fitting line models
model1 <- lm(log10(AssessmentCrimeData$Population) ~ log10(AssessmentCrimeData$Land.Area.in.Hectares) + log10(AssessmentCrimeData$Anti.Social.Behaviour))
model2 <- lm(log10(AssessmentCrimeData$Population) ~ log10(AssessmentCrimeData$Land.Area.in.Hectares) + log10(AssessmentCrimeData$Burglary))
model3 <- lm(log10(AssessmentCrimeData$Population) ~ log10(AssessmentCrimeData$Land.Area.in.Hectares) + log10(AssessmentCrimeData$Robbery))
model4 <- lm(log10(AssessmentCrimeData$Population) ~ log10(AssessmentCrimeData$Land.Area.in.Hectares) + log10(AssessmentCrimeData$Vehicle.Crimes))
model5 <- lm(log10(AssessmentCrimeData$Population) ~ log10(AssessmentCrimeData$Land.Area.in.Hectares) + log10(AssessmentCrimeData$Violent.Crimes))
model6 <- lm(log10(AssessmentCrimeData$Population) ~ log10(AssessmentCrimeData$Land.Area.in.Hectares) + log10(AssessmentCrimeData$Shoplifting))
model7 <- lm(log10(AssessmentCrimeData$Population) ~ log10(AssessmentCrimeData$Land.Area.in.Hectares) + log10(AssessmentCrimeData$Criminal.Damage...Arson))
model8 <- lm(log10(AssessmentCrimeData$Population) ~ log10(AssessmentCrimeData$Land.Area.in.Hectares) + log10(AssessmentCrimeData$Other.Theft))
model9 <- lm(log10(AssessmentCrimeData$Population) ~ log10(AssessmentCrimeData$Land.Area.in.Hectares) + log10(AssessmentCrimeData$Drugs))
model10 <- lm(log10(AssessmentCrimeData$Population) ~ log10(AssessmentCrimeData$Land.Area.in.Hectares) + log10(AssessmentCrimeData$Other.Crimes))
model11 <- lm(log10(AssessmentCrimeData$Population) ~ log10(AssessmentCrimeData$Land.Area.in.Hectares) + log10(AssessmentCrimeData$Bike.Theft))
model12 <- lm(log10(AssessmentCrimeData$Population) ~ log10(AssessmentCrimeData$Land.Area.in.Hectares) + log10(AssessmentCrimeData$Possession.of.Weapons))
model13 <- lm(log10(AssessmentCrimeData$Population) ~ log10(AssessmentCrimeData$Land.Area.in.Hectares) + log10(AssessmentCrimeData$Public.Order))
model14 <- lm(log10(AssessmentCrimeData$Population) ~ log10(AssessmentCrimeData$Land.Area.in.Hectares) + log10(AssessmentCrimeData$Theft.From.the.Person))

#plotting for residuals
par(mfrow= c(2,3))
Residual1 <- residuals(model1)
plot(Residual1[-length(Residual1)], Residual1[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Anti Social behaviour", cex.lab = 1.5, col = "red")
Residual2 <- residuals(model2)
plot(Residual2[-length(Residual2)], Residual2[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Burglary", cex.lab = 1.5, col = "blue")
Residual3 <- residuals(model3)
plot(Residual3[-length(Residual3)], Residual3[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Robbery", cex.lab = 1.5, col = "red")
Residual4 <- residuals(model4)
plot(Residual4[-length(Residual4)], Residual4[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Vehicle Crimes", cex.lab = 1.5, col = "green")
Residual5 <- residuals(model5)
plot(Residual5[-length(Residual5)], Residual5[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Violent Crimes", cex.lab = 1.5, col = "red")
Residual6 <- residuals(model6)
plot(Residual6[-length(Residual6)], Residual6[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Shoplifting", cex.lab = 1.5, col = "orange")
Residual7 <- residuals(model7)
plot(Residual1[-length(Residual7)], Residual7[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Criminal Damage and Arson", cex.lab = 1.5, col = "red")
Residual8 <- residuals(model8)
plot(Residual8[-length(Residual8)], Residual8[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Other Theft", cex.lab = 1.5, col = "black")
Residual9 <- residuals(model9)
plot(Residual9[-length(Residual9)], Residual9[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Drugs", cex.lab = 1.5, col = "green")
Residual10 <- residuals(model10)
plot(Residual10[-length(Residual10)], Residual10[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Other Crimes", cex.lab = 1.5, col = "blue")
Residual11 <- residuals(model11)
plot(Residual11[-length(Residual1)], Residual11[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Bike Theft", cex.lab = 1.5, col = "red")
Residual12 <- residuals(model12)
plot(Residual1[-length(Residual12)], Residual12[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Possession of Weapons", cex.lab = 1.5, col = "blue")
Residual13 <- residuals(model13)
plot(Residual13[-length(Residual13)], Residual13[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Public Order", cex.lab = 1.5, col = "red")
Residual14 <- residuals(model14)
plot(Residual14[-length(Residual14)], Residual1[-1], xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]), pch = 19, cex = 0.2, main = "Theft From the Person", cex.lab = 1.5, col = "green")


install.packages("rlang")
install.packages("rlang")

#linearity plotting
# Define regression models
model1 <- lm(log10(AssessmentCrimeData$Anti.Social.Behaviour) ~ log10(AssessmentCrimeData$Population))
model2 <- lm(log10(AssessmentCrimeData$Burglary) ~ log10(AssessmentCrimeData$Population))
model3 <- lm(log10(AssessmentCrimeData$Robbery) ~ log10(AssessmentCrimeData$Population))
model4 <- lm(log10(AssessmentCrimeData$Vehicle.Crimes) ~ log10(AssessmentCrimeData$Population))
model5 <- lm(log10(AssessmentCrimeData$Violent.Crimes) ~ log10(AssessmentCrimeData$Population))
model6 <- lm(log10(AssessmentCrimeData$Shoplifting) ~ log10(AssessmentCrimeData$Population))
model7 <- lm(log10(AssessmentCrimeData$Criminal.Damage...Arson) ~ log10(AssessmentCrimeData$Population))
model8 <- lm(log10(AssessmentCrimeData$Other.Theft) ~ log10(AssessmentCrimeData$Population))
model9 <- lm(log10(AssessmentCrimeData$Drugs) ~ log10(AssessmentCrimeData$Population))
model10 <- lm(log10(AssessmentCrimeData$Other.Crimes) ~ log10(AssessmentCrimeData$Population))
model11 <- lm(log10(AssessmentCrimeData$Bike.Theft) ~ log10(AssessmentCrimeData$Population))
model12 <- lm(log10(AssessmentCrimeData$Possession.of.Weapons) ~ log10(AssessmentCrimeData$Population))
model13 <- lm(log10(AssessmentCrimeData$Public.Order) ~ log10(AssessmentCrimeData$Population))
model14 <- lm(log10(AssessmentCrimeData$Theft.From.the.Person) ~ log10(AssessmentCrimeData$Population))

# Plot the graphs
par(mfrow= c(2,3))
x <- log10(AssessmentCrimeData$Population)
y <- log10(AssessmentCrimeData$Anti.Social.Behaviour)
plot(x, y, xlab = "Log10 Population", ylab = "Log10 Anti Social Behaviour",
     main = "AntiSocial Behaviour vs Population", cex.lab= 1.5)
abline(model1, col="red", lwd= 2)

x <- log10(AssessmentCrimeData$Population)
y <- log10(AssessmentCrimeData$Burglary)
plot(x, y, xlab = "Log10 Population", ylab = "Log10 Burglary",
     main = "Burglary vs Population", cex.lab= 1.5)
abline(model2, col="blue", lwd= 2)

x <- log10(AssessmentCrimeData$Population)
y <- log10(AssessmentCrimeData$Robbery)
plot(x, y, xlab = "Log10 Population", ylab = "Log10 Robbery",
     main = "Robbery vs Population", cex.lab= 1.5)
abline(model3, col="green", lwd= 2)

x <- log10(AssessmentCrimeData$Population)
y <- log10(AssessmentCrimeData$Vehicle.Crimes)
plot(x, y, xlab = "Log10 Population", ylab = "Log10 Vehicle Crimes",
     main = "Vehicle Crimes vs Population", cex.lab= 1.5)
abline(model4, col="red", lwd= 2)

x <- log10(AssessmentCrimeData$Population)
y <- log10(AssessmentCrimeData$Violent.Crimes)
plot(x, y, xlab = "Log10 Population", ylab = "Log10 Violent Crimes",
     main = "Violent Crimes vs Population", cex.lab= 1.5)
abline(model5, col="green", lwd= 2)

x <- log10(AssessmentCrimeData$Population)
y <- log10(AssessmentCrimeData$Shoplifting)
plot(x, y, xlab = "Log10 Population", ylab = "Log10 Shoplifting",
     main = "Shoplifting vs Population", cex.lab= 1.5)
abline(model6, col="orange", lwd= 2)

x <- log10(AssessmentCrimeData$Population)
y <- log10(AssessmentCrimeData$Criminal.Damage...Arson)
plot(x, y, xlab = "Log10 Population", ylab = "Log10 Criminal Damage and Arson",
     main = "Criminal Damage and Arson vs Population", cex.lab= 1.5)
abline(model7, col="blue", lwd= 2)

x <- log10(AssessmentCrimeData$Population)
y <- log10(AssessmentCrimeData$Other.Theft)
plot(x, y, xlab = "Log10 Population", ylab = "Log10 Other Theft",
     main = "Other Theft vs Population", cex.lab= 1.5)
abline(model8, col="green", lwd= 2)

x <- log10(AssessmentCrimeData$Population)
y <- log10(AssessmentCrimeData$Drugs)
plot(x, y, xlab = "Log10 Population", ylab = "Log10 Drugs",
     main = "Drugs vs Population", cex.lab= 1.5)
abline(model9, col="red", lwd= 2)

x <- log10(AssessmentCrimeData$Population)
y <- log10(AssessmentCrimeData$Other.Crimes)
plot(x, y, xlab = "Log10 Population", ylab = "Log10 Other Crimes",
     main = "Other Crimes vs Population", cex.lab= 1.5)
abline(model10, col="green", lwd= 2)

x <- log10(AssessmentCrimeData$Population)
y <- log10(AssessmentCrimeData$Bike.Theft)
plot(x, y, xlab = "Log10 Population", ylab = "Log10 Bike Theft",
     main = "Bike Theft vs Population", cex.lab= 1.5)
abline(model11, col="brown", lwd= 2)

x <- log10(AssessmentCrimeData$Population)
y <- log10(AssessmentCrimeData$Possession.of.Weapons)
plot(x, y, xlab = "Log10 Population", ylab = "Log10 Possession of Weapons",
     main = "Possession of Weapons vs Population", cex.lab= 1.5)
abline(model12, col="green", lwd= 2)

x <- log10(AssessmentCrimeData$Population)
y <- log10(AssessmentCrimeData$Public.Order)
plot(x, y, xlab = "Log10 Population", ylab = "Log10 Public Order",
     main = "Public Order vs Population", cex.lab= 1.5)
abline(model13, col="blue", lwd= 2)

x <- log10(AssessmentCrimeData$Population)
y <- log10(AssessmentCrimeData$Theft.From.the.Person)
plot(x, y, xlab = "Log10 Population", ylab = "Log10 Theft From the Person",
     main = "Theft From the Person vs Population", cex.lab= 1.5)
abline(model14, col="red", lwd= 2)

#Plotting and checking for homoscedasticity

par(mfrow = c(2, 3))

# Model 1
plot(fitted(model1), residuals(model1))
abline(h = 0, lwd = 2, col = 3)

# Model 2
plot(fitted(model2), residuals(model2))
abline(h = 0, lwd = 2, col = 2)

# Model 3
plot(fitted(model3), residuals(model3))
abline(h = 0, lwd = 2, col = 4)

# Model 4
plot(fitted(model4), residuals(model4))
abline(h = 0, lwd = 2, col = 2)

# Model 5
plot(fitted(model5), residuals(model5))
abline(h = 0, lwd = 2, col = 3)

# Model 6
plot(fitted(model6), residuals(model6))
abline(h = 0, lwd = 2, col = 6)

# Model 7
plot(fitted(model7), residuals(model7))
abline(h = 0, lwd = 2, col = 7)

# Model 8
plot(fitted(model8), residuals(model8))
abline(h = 0, lwd = 2, col = 2)

# Model 9
plot(fitted(model9), residuals(model9))
abline(h = 0, lwd = 2, col = 5)

# Model 10
plot(fitted(model10), residuals(model10))
abline(h = 0, lwd = 2, col = 4)

# Model 11
plot(fitted(model11), residuals(model11))
abline(h = 0, lwd = 2, col = 2)

# Model 12
plot(fitted(model12), residuals(model12))
abline(h = 0, lwd = 2, col = 1)

# Model 13
plot(fitted(model13), residuals(model13))
abline(h = 0, lwd = 2, col = 2)

# Model 14
plot(fitted(model14), residuals(model14))
abline(h = 0, lwd = 2, col = 6)

#plotting residuals between each variables

par(mfrow = c(1, 3))
plot(model1$fitted.values, model1$residuals, cex=0.2, main="Model 1")
plot(model2$fitted.values, model2$residuals, cex=0.2, main="Model 2")
plot(model3$fitted.values, model3$residuals, cex=0.2, main="Model 3")
plot(model4$fitted.values, model4$residuals, cex=0.2, main="Model 4")
plot(model5$fitted.values, model5$residuals, cex=0.2, main="Model 5")
plot(model6$fitted.values, model6$residuals, cex=0.2, main="Model 6")
plot(model7$fitted.values, model7$residuals, cex=0.2, main="Model 7")
plot(model8$fitted.values, model8$residuals, cex=0.2, main="Model 8")
plot(model9$fitted.values, model9$residuals, cex=0.2, main="Model 9")
plot(model10$fitted.values, model10$residuals, cex=0.2, main="Model 10")
plot(model11$fitted.values, model11$residuals, cex=0.2, main="Model 11")
plot(model12$fitted.values, model12$residuals, cex=0.2, main="Model 12")
plot(model13$fitted.values, model13$residuals, cex=0.2, main="Model 13")
plot(model14$fitted.values, model14$residuals, cex=0.2, main="Model 14")

#using a different method
Residuals <- data.frame(model1$residuals, model2$residuals, model3$residuals, model4$residuals, model5$residuals, model6$residuals,
                        model7$residuals, model8$residuals, model9$residuals, model10$residuals,
                        model11$residuals, model12$residuals, model13$residuals, model14$residuals)
colnames(Residuals) <- c("Anti-Social Behaviour", "Burglary", "Robbery", "Vehicle Crimes", "Violent Crimes", "Shoplifting", "Criminal Damage and Arson", "Other Theft", "Drugs", "Other Crimes", "Bike Theft", "Possession of Weapons", "Public Order", "Theft From the Person")
plot(Residuals, cex=0.2)


#creating heatmaps

library(gplots)
library(RColorBrewer)
#correlation of the residual
Henry <- cor(Residuals, use = "pairwise", method = "pearson")
Henry

# Plot heatmap
heatmap.2(Henry,
          trace = "none",         # remove row/column names
          col = colorRampPalette(rev(brewer.pal(11, "RdYlBu"))), # choose color scheme
          dendrogram = "none"     # remove dendrogram
)


#Dendrograms

# Calculate the distance matrix using the Euclidean distance metric
distance_matrix <- dist(Henry, method = "euclidean")

# Cluster the data using complete linkage method
cluster_result <- hclust(distance_matrix, method = "complete")

# Plot the dendrogram
plot(cluster_result, main = "Dendrogram", xlab = "", ylab = "Distance", hang = -1)


#Part 3.
#installing shapefiles

#install packages
install.packages("sf")
install.packages("raster")
install.packages("dplyr")
install.packages("spData")
install.packages("tmap")
install.packages("ggplot2")
install.packages("rgdal")
install.packages("png")
install.packages("rgeos")
install.packages("rio")
install.packages("vctrs")


#libraries
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)    # for static and interactive maps
library(ggplot2) # tidyverse data visualization package
library(rgdal)
library(png)
library(rgeos)
library(rio)
install.packages("arrow")
install.packages("feather")
install.packages("fst")
install.packages("hexView")
install.packages("pzfx")
install.packages("readODS")
install.packages("rmatio")



#importing shapefiles

shp2<- shapefile("C:/Users/USER/Desktop/Big Data/Information visualization/R lang/shapefiles2/Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shx")

regions<- rio::import("C:/Users/USER/Desktop/Big Data/Information visualization/R lang/AssessmentCrimeData.csv")

head(shp2)

shp_main<-subset(shp2, LSOA11CD %in% regions$LSOA)

head(shp_main)

summary(shp_main)

#add variables to the shapefile.

shp_main$"Population Density"<- regions$Population/regions$`Land Area in Hectares`
shp_main$"log10(ASB Density)"<- log10(regions$`Anti-Social Behaviour`/regions$`Land Area in Hectares`)
shp_main$"log10(Shoplifting Density)"<- log10(regions$Shoplifting/regions$`Land Area in Hectares`)
shp_main$"log10(Burglary Density)"<- log10(regions$Burglary/regions$`Land Area in Hectares`)
shp_main$"log10(Drugs Density)"<- log10(regions$Drugs/regions$`Land Area in Hectares`)
shp_main$"log10(Bike Theft Density)"<- log10(regions$`Bike Theft`/regions$`Land Area in Hectares`)
shp_main$"log10(Vehicle Crimes Density)"<- log10(regions$`Vehicle Crimes`/regions$`Land Area in Hectares`)
shp_main$"log10(Violent Crimes Density)"<- log10(regions$`Violent Crimes`/regions$`Land Area in Hectares`)
shp_main$"log10(Possession of Weapons Density)"<- log10(regions$`Possession of Weapons`/regions$`Land Area in Hectares`)
shp_main$"log10(Theft From the Person Density)"<- log10(regions$`Theft From the Person`/regions$`Land Area in Hectares`)
shp_main$"log10(Other Crimes Density)"<- log10(regions$`Other Crimes`/regions$`Land Area in Hectares`)
shp_main$"log10(Other Theft Density)"<- log10(regions$`Other Theft`/regions$`Land Area in Hectares`)
shp_main$"log10(Criminal Damage & Arson Density)"<- log10(regions$`Criminal Damage & Arson`/regions$`Land Area in Hectares`)
shp_main$"log10(Public Order Density)"<- log10(regions$`Public Order`/regions$`Land Area in Hectares`)
shp_main$"log10(Robbery Density)"<- log10(regions$Robbery/regions$`Land Area in Hectares`)


tm_shape(shp_main) +
  tm_polygons("log10(ASB Density)", palette = "YlGn") +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Antisocial Behaviour Density Map")


tm_shape(shp_main) +
  tm_polygons("log10(Shoplifting Density)", palette = "RdYlBu", midpoint = NA) +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Shoplifting Density Map")

tm_shape(shp_main) +
  tm_polygons("log10(Burglary Density)", palette = "RdYlGn", midpoint = NA) +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Burglary Density Map")


tm_shape(shp_main) +
  tm_polygons("log10(Drugs Density)", palette = "PiYG", midpoint = NA) +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Drugs Density Map")

tm_shape(shp_main) +
  tm_polygons("log10(Bike Theft Density)", palette = "PRGn", midpoint = NA) +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Bike Theft Density Map")

tm_shape(shp_main) +
  tm_polygons("log10(Vehicle Crimes Density)", palette = "RdBu", midpoint = NA) +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Vehicle Crimes Density Map")

tm_shape(shp_main) +
  tm_polygons("log10(Violent Crimes Density)", palette = "YlGnBu", midpoint = NA) +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Violent Crimes Density Map")

tm_shape(shp_main) +
  tm_polygons("log10(Possession of Weapons Density)", palette = "PRGn", midpoint = NA) +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Possession of Weapons Density Map")

tm_shape(shp_main) +
  tm_polygons("log10(Theft From the Person Density)", palette = "PiYG", midpoint = NA) +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Theft From the Person Density Map")

tm_shape(shp_main) +
  tm_polygons("log10(Other Crimes Density)", palette = "PRGn", midpoint = NA) +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Other Crimes Density Map")

tm_shape(shp_main) +
  tm_polygons("log10(Other Theft Density)", palette = "RdYlGn", midpoint = NA) +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Other Theft Density Map")

tm_shape(shp_main) +
  tm_polygons("log10(Robbery Density)", palette = "PRGn", midpoint = NA) +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Robbery Density Map")

tm_shape(shp_main) +
  tm_polygons("log10(Criminal Damage & Arson Density)", palette = "PRGn", midpoint = NA) +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Criminal Damage & Arson Density Map")

tm_shape(shp_main) +
  tm_polygons("log10(Public Order Density)", palette = "PiYG", midpoint = NA) +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Public Order Density Map")


#using symbols/indicators to identify hotspots on the maps

# Add variables to the shapefile
shp_main$"Population Density" <- regions$Population / regions$`Land Area in Hectares`
shp_main$"log10(ASB Density)" <- log10(regions$`Anti-Social Behaviour` / regions$`Land Area in Hectares`)
shp_main$"log10(Shoplifting Density)" <- log10(regions$Shoplifting / regions$`Land Area in Hectares`)
shp_main$"log10(Burglary Density)" <- log10(regions$Burglary / regions$`Land Area in Hectares`)
shp_main$"log10(Drugs Density)" <- log10(regions$Drugs / regions$`Land Area in Hectares`)
shp_main$"log10(Bike Theft Density)" <- log10(regions$`Bike Theft` / regions$`Land Area in Hectares`)
shp_main$"log10(Vehicle Crimes Density)" <- log10(regions$`Vehicle Crimes` / regions$`Land Area in Hectares`)
shp_main$"log10(Violent Crimes Density)" <- log10(regions$`Violent Crimes` / regions$`Land Area in Hectares`)
shp_main$"log10(Possession of Weapons Density)" <- log10(regions$`Possession of Weapons` / regions$`Land Area in Hectares`)
shp_main$"log10(Theft From the Person Density)" <- log10(regions$`Theft From the Person` / regions$`Land Area in Hectares`)
shp_main$"log10(Other Crimes Density)" <- log10(regions$`Other Crimes` / regions$`Land Area in Hectares`)
shp_main$"log10(Other Theft Density)" <- log10(regions$`Other Theft` / regions$`Land Area in Hectares`)
shp_main$"log10(Criminal Damage & Arson Density)" <- log10(regions$`Criminal Damage & Arson` / regions$`Land Area in Hectares`)
shp_main$"log10(Public Order Density)" <- log10(regions$`Public Order` / regions$`Land Area in Hectares`)
shp_main$"log10(Robbery Density)" <- log10(regions$Robbery / regions$`Land Area in Hectares`)

# Subset hotspots based on a threshold (e.g., 90th percentile)

hotspots_asb <- subset(shp_main, `log10(ASB Density)` > quantile(`log10(ASB Density)`, 0.9))
hotspots_shoplifting <- subset(shp_main, `log10(Shoplifting Density)` > quantile(`log10(Shoplifting Density)`, 0.9))
hotspots_burglary <- subset(shp_main, `log10(Burglary Density)` > quantile(`log10(Burglary Density)`, 0.9))
hotspots_drugs <- subset(shp_main, `log10(Drugs Density)` > quantile(`log10(Drugs Density)`, 0.9))
hotspots_bike_theft <- subset(shp_main, `log10(Bike Theft Density)` > quantile(`log10(Bike Theft Density)`, 0.9))
hotspots_vehicle_crimes <- subset(shp_main, `log10(Vehicle Crimes Density)` > quantile(`log10(Vehicle Crimes Density)`, 0.9))
hotspots_violent_crimes <- subset(shp_main, `log10(Violent Crimes Density)` > quantile(`log10(Violent Crimes Density)`, 0.9))
hotspots_possession_of_weapons <- subset(shp_main, `log10(Possession of Weapons Density)` > quantile(`log10(Possession of Weapons Density)`, 0.9))
hotspots_theft_from_person <- subset(shp_main, `log10(Theft From the Person Density)` > quantile(`log10(Theft From the Person Density)`, 0.9))
hotspots_other_crimes <- subset(shp_main, `log10(Other Crimes Density)` > quantile(`log10(Other Crimes Density)`, 0.9))
hotspots_other_theft <- subset(shp_main, `log10(Other Theft Density)` > quantile(`log10(Other Theft Density)`, 0.9))
hotspots_criminal_damage_arson <- subset(shp_main, `log10(Criminal Damage & Arson Density)` > quantile(`log10(Criminal Damage & Arson Density)`, 0.9))
hotspots_public_order <- subset(shp_main, `log10(Public Order Density)` > quantile(`log10(Public Order Density)`, 0.9))
hotspots_robbery <- subset(shp_main, `log10(Robbery Density)` > quantile(`log10(Robbery Density)`, 0.9))


# ASB Density Map
tm_shape(shp_main) +
  tm_polygons("log10(ASB Density)", palette = "YlGn") +
  tm_shape(hotspots_asb) +
  tm_symbols(size = 0.8, col = "red", text = "LSOA11NM") +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Antisocial Behaviour Density Map")

# Shoplifting Density Map
tm_shape(shp_main) +
  tm_polygons("log10(Shoplifting Density)", palette = "RdYlBu", midpoint = NA) +
  tm_shape(hotspots_shoplifting) +
  tm_symbols(size = 0.8, col = "red", text = "LSOA11NM") +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Shoplifting Density Map")

# Burglary Density Map
tm_shape(shp_main) +
  tm_polygons("log10(Burglary Density)", palette = "RdYlGn", midpoint = NA) +
  tm_shape(hotspots_burglary) +
  tm_symbols(size = 0.8, col = "red", text = "LSOA11NM") +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Burglary Density Map")

# Drugs Density Map
tm_shape(shp_main) +
  tm_polygons("log10(Drugs Density)", palette = "PiYG", midpoint = NA) +
  tm_shape(hotspots_drugs) +
  tm_symbols(size = 0.8, col = "red", text = "LSOA11NM") +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Drugs Density Map")

# Bike Theft Density Map
tm_shape(shp_main) +
  tm_polygons("log10(Bike Theft Density)", palette = "PRGn", midpoint = NA) +
  tm_shape(hotspots_bike_theft) +
  tm_symbols(size = 0.8, col = "red", text = "LSOA11NM") +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Bike Theft Density Map")

# Vehicle Crimes Density Map
tm_shape(shp_main) +
  tm_polygons("log10(Vehicle Crimes Density)", palette = "RdBu", midpoint = NA) +
  tm_shape(hotspots_vehicle_crimes) +
  tm_symbols(size = 0.8, col = "red", text = "LSOA11NM") +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Vehicle Crimes Density Map")

# Violent Crimes Density Map
tm_shape(shp_main) +
  tm_polygons("log10(Violent Crimes Density)", palette = "YlGnBu", midpoint = NA) +
  tm_shape(hotspots_violent_crimes) +
  tm_symbols(size = 0.8, col = "red", text = "LSOA11NM") +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Violent Crimes Density Map")

# Possession of Weapons Density Map
tm_shape(shp_main) +
  tm_polygons("log10(Possession of Weapons Density)", palette = "RdYlGn", midpoint = NA) +
  tm_shape(hotspots_possession_of_weapons) +
  tm_symbols(size = 0.8, col = "red", text = "LSOA11NM") +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Possession of Weapons Density Map")

# Theft From the Person Density Map
tm_shape(shp_main) +
  tm_polygons("log10(Theft From the Person Density)", palette = "RdYlBu", midpoint = NA) +
  tm_shape(hotspots_theft_from_person) +
  tm_symbols(size = 0.8, col = "red", text = "LSOA11NM") +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Theft From the Person Density Map")

# Other Crimes Density Map
tm_shape(shp_main) +
  tm_polygons("log10(Other Crimes Density)", palette = "BrBG", midpoint = NA) +
  tm_shape(hotspots_other_crimes) +
  tm_symbols(size = 0.8, col = "red", text = "LSOA11NM") +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Other Crimes Density Map")

# Other Theft Density Map
tm_shape(shp_main) +
  tm_polygons("log10(Other Theft Density)", palette = "PuOr", midpoint = NA) +
  tm_shape(hotspots_other_theft) +
  tm_symbols(size = 0.8, col = "red", text = "LSOA11NM") +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Other Theft Density Map")

# Criminal Damage & Arson Density Map
tm_shape(shp_main) +
  tm_polygons("log10(Criminal Damage & Arson Density)", palette = "YlOrBr", midpoint = NA) +
  tm_shape(hotspots_criminal_damage_arson) +
  tm_symbols(size = 0.8, col = "black", text = "LSOA11NM") +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Criminal Damage & Arson Density Map")

# Public Order Density Map
tm_shape(shp_main) +
  tm_polygons("log10(Public Order Density)", palette = "RdPu", midpoint = NA) +
  tm_shape(hotspots_public_order) +
  tm_symbols(size = 0.8, col = "yellow", text = "LSOA11NM") +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Public Order Density Map")

# Robbery Density Map
tm_shape(shp_main) +
  tm_polygons("log10(Robbery Density)", palette = "YlGn", midpoint = NA) +
  tm_shape(hotspots_robbery) +
  tm_symbols(size = 0.8, col = "red", text = "LSOA11NM") +
  tm_shape(shp_main) +
  tm_borders(lwd = 1) +
  tm_layout(title = "Robbery Density Map")
