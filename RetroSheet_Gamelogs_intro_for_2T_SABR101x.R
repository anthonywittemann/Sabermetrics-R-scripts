#####################################
#                                   #
# SABR101x: Sabermetrics 101        #
# on the edX platform for           #
# Boston University                 #
# Andy Andres and Morris Greenberg  #
# Ethan Bein and Eric Smiley        #
#                                   #
#####################################

install.packages("ggplot2")
require(ggplot2)

# Read in file of Retrosheet Gamelog data from BU SQL Sandbox
attendance_data <- read.csv(file.choose())
summary(attendance_data)
attach(attendance_data)

# The columns we want to see in a scatterplot matrix 
# into a smaller data frame
Game_logs = attendance_data[c("attendance", "game_minutes", "outs")]
# Scatterplot matrix command
pairs(Game_logs)

# Does attendance correlate with minutes played?
qplot(attendance, game_minutes, data = Game_logs, 
      main = "Retrosheet Gamelogs Scatterplot Matrix")
TOGvattend = lm(Game_logs$game_minutes~Game_logs$attendance)
TOGvattend
summary(TOGvattend)$r.squared
cor(Game_logs$game_minutes, Game_logs$attendance)

# Does number of outs correlate with minutes played?
qplot(outs, game_minutes, data = Game_logs, 
      main = "Retrosheet Gamelogs Scatterplot Matrix")
TOGvouts = lm(Game_logs$game_minutes~Game_logs$outs)
TOGvouts
summary(TOGvouts)$r.squared
cor(Game_logs$game_minutes, Game_logs$outs)
