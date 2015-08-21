#################################
#                               #
# SABR101x: Sabermetrics 101    #
# on the edX platform for       #
# Boston University             #
# Andy Andres                   #
#                               #
#################################


library(dplyr)
#Read in file of Regression from 2013 and 2014 from BU SQL Sandbox
fname = file.choose()
PitcherRegression = read.csv(fname)
summary(PitcherRegression)
attach(PitcherRegression)

#Strikeout Rate Top 20 -> 2013 to 2014 (Smallest Regression to Mean)

head(KRate13,20)
boxplot(head(KRate13, 20), KRate14[(rank(-KRate13) <= 20)], col="tomato",
        main = "Regression to the Mean Example:\nTop 20 2013 KRate to 2014", 
        ylab = "KRate", ylim = c(0.10,.35))
summary(head(KRate13,20))
summary(KRate14[(rank(-KRate13) <= 20)])

#Strikeout Rate Bottom 20 -> 2013 to 2014

tail(KRate13,20)
boxplot(tail(KRate13, 20), KRate14[(rank(KRate13) <= 20)], col="tomato",
        main = "Regression to the Mean Example:\nBottom 20 2013 KRate to 2014", 
        ylab = "KRate", ylim = c(0.10,.35))
summary(tail(KRate13,20))
summary(KRate14[(rank(KRate13) <= 20)])


#Rearrange By ERA (Medium Regression to Mean)
PitcherRegression2 <- (grid.arrange(PitcherRegression, ERA13))
attach(PitcherRegression2)

#ERA Top 20 -> 2013 to 2014

head(ERA13,20)
boxplot(head(ERA13, 20), ERA14[(rank(ERA13) <= 20)], col="tomato",
        main = "Regression to the Mean Example:\nTop 20 2013 ERA to 2014", 
        ylab = "ERA", ylim = c(1.5,6.5))
summary(head(ERA13,20))
summary(ERA14[(rank(ERA13) <= 20)])

#ERA Bottom 20 -> 2013 to 2014

tail(ERA13,20)
boxplot(tail(ERA13, 20), ERA14[(rank(-ERA13) <= 20)], col="tomato",
        main = "Regression to the Mean Example:\nBottom 20 2013 ERA to 2014", 
        ylab = "ERA", ylim = c(1.5,6.5))
summary(tail(ERA13,20))
summary(ERA14[(rank(-ERA13) <= 20)])

#Rearrange By BABIP (Most Regression to Mean)
PitcherRegression3 <- (arrange(PitcherRegression, BABIP13))
attach(PitcherRegression3)

#BABIP Top 20 -> 2013 to 2014

head(BABIP13,20)
boxplot(head(BABIP13, 20), BABIP14[(rank(BABIP13) <= 20)], col="tomato",
        main = "Regression to the Mean Example:\nTop 20 2013 BABIP to 2014", 
        ylab = "BABIP", ylim = c(.240,.360))
summary(head(BABIP13,20))
summary(BABIP14[(rank(BABIP13) <= 20)])

#BABIP Top 20 -> 2013 to 2014

tail(BABIP13,20)
boxplot(tail(BABIP13, 20), BABIP14[(rank(-BABIP13) <= 20)], col="tomato",
        main = "Regression to the Mean Example:\nBottom 20 2013 BABIP to 2014", 
        ylab = "BABIP", ylim = c(.240,.360))
summary(tail(BABIP13,20))
summary(BABIP14[(rank(-BABIP13) <= 20)])


detach(PitcherRegression3)
