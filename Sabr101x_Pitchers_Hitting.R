install.packages("ggplot2")
library(ggplot2)
# batters: http://www.fangraphs.com/leaders.aspx?pos=p&stats=bat&lg=all&qual=0&type=8&season=2009&month=0&season1=2000&ind=1&team=0&rost=0&age=0&filter=&players=0
batting = read.csv('Sabr101x PitchersB.csv')
# pitchers: http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=8&season=2009&month=0&season1=2000&ind=1&team=0&rost=0&age=0&filter=&players=0
pitching = read.csv('Sabr101x PitchersP.csv')

# subset help
?subset

# narrow by innings and PA
fiftypa = subset(batting,PA>=50)
hundredip = subset(pitching,IP>=100)

# ggplot help
?ggplot

# plot K/9 vs. ERA as pitchers
pPlot = ggplot(hundredip,aes(K.9,ERA))

# error - must add more layers
pPlot

# add points in red
pPlot = pPlot + geom_point(color = 'firebrick3')
pPlot

# add titles
pPlot = pPlot + ggtitle('ERA vs. K/9 for 2000-2009, min. 100 IP')
pPlot = pPlot + xlab('Strikeouts per 9')
pPlot = pPlot + ylab('Earned Run Average')
pPlot

# add regression line
pPlot = pPlot + stat_smooth(method = 'lm',formula = y~x,color = 'black',size = 1)
pPlot

# merge help
?merge

# merge pitching and hitting
pitHit = merge(x = hundredip,y = fiftypa,
               by.x = c('Season','playerid','Name'),by.y = c('Season','playerid','Name'))

# create OPS column
pitHit$OPS = pitHit$OBP + pitHit$SLG

# which best hitter?
pitHit$Name[which.max(pitHit$OPS)]

# top hitters
pitHit = pitHit[order(-pitHit$OPS),]
head(subset(pitHit,subset = TRUE,select = c(Name,Season,OPS,ERA), n=10))

# how many > .700 OPS, < 4.00 ERA?
subset(pitHit,subset = OPS > .7 & ERA < 4,select = c(Name,Season,OPS,ERA))

# best pitcher with at least a .600 OPS?
bestRow = which.min(subset(pitHit,subset = OPS > .6)$ERA)
df = subset(pitHit,subset = TRUE,select = c(Name,Season,OPS,ERA))
df[bestRow,]
