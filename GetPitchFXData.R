#This file is an amalgamation of a few files and so it redundant in many ways.  The three steps of downloading data could easily be done in a more concise way.

# DownloadPitchFX.R
# downloads the massive MLB Gameday data.
# Author: apeecape
# Email: achikule at gmail dot com
# Updated: Jun 13 2010
# Version 0.4
# Version History
# 0.5 ~ grab player data, both pitchers and batters, ability to pick team
# 0.4 ~ get team data, and ability to grab team info, checks to see if regular season
# 0.3 ~ updated so 2010 works, fixed some bugs, and saves as tab delimited file
# 0.2 ~ inputs are start and end dates
# 0.1 ~ grab Pitch f/x data from MLB Gameday, specify date ranges (takes half a minute for a day's worth of data on my 2.5Ghz machine)

# Future Versions:
# ~ ability to pick pitchers, batters, teams
# - ability to grab matchups
# - better searching instead of tediously parsing through each XML file
# ~ connect to mysql database
# ~ don't overheat computer!
# ~ document Gameday Code

# downloading pitch f/x data from MLB website
# Get data from http://gd2.mlb.com/components/game/mlb/
# XML package http://www.omegahat.org/RSXML/shortIntro.html
# Perl script of same application by Mike Fast:
# http://fastballs.files.wordpress.com/2007/09/hack_28_parser_mikefast_test_pl.txt
# Less general R code from Erik Iverson of Blogistic Reflections:
# http://blogisticreflections.wordpress.com/2009/10/04/using-r-to-analyze-baseball-games-in-real-time/
# listing of pitch f/x tools by Baseball Analysts
# http://baseballanalysts.com/archives/2010/03/how_can_i_get_m.php
# downloadable pitch f/x database from Darrell Zimmerman
# http://www.wantlinux.net/category/baseball-data/

# I think gameday data starts 2005
# I think enhanced gameday (pitch fx) has all of 2009, most of 2008, some 2007, tiny bit 2006

# required libraries:
library(XML)

# code for <game type> in game.xml (input game.type in code)
# "S" ~ spring training, "R" ~ regular season, "D" ~ Division Series
# "L" ~ League Championship Series "W" ~ World Series

# code for <game gameday_sw> in game.xml
# http://sports.dir.groups.yahoo.com/group/RetroSQL/message/320
# "N" ~ missing, no pitch info
# "Y" ~ standard w/ pitch locations
# "E" ~ w/ pitch f/x
# "P" ~ for 2010, whatever that's supposed to mean

# code for teams

# code for players

# code for gameday

# code for pitch type

# code for atbat type

# checks for:
# gameday type
# home, away
# player, batter, pitch type

# -----------------------------------------------------------

DownloadPitchFX <- function(fileloc = "./pitchfx.txt",
                            start.date = "2009-05-02", end.date = start.date,
                            URL.base = "http://gd2.mlb.com/components/game/mlb/",
                            game.type = "R",
                            grab.pitch = c("des", "type", "x", "y",
                              "start_speed", "end_speed",
                              "sz_top", "sz_bot", "pfx_x", "pfx_z", "px", "pz",
                              "x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az",
                              "break_y", "break_angle", "break_length", "pitch_type",
                              "type_confidence"),
                            grab.atbat = c("b", "s", "o", "batter", "pitcher", "b_height",
                              "stand", "p_throws", "event")) {
  # write initial variables on file
  meta <- c("Year", "Month", "Day", "Inning", "Home", "Away")
  write(c(meta, grab.atbat, grab.pitch), file = fileloc,
        ncol = length(c(grab.atbat, grab.pitch)) + length(meta), sep = ">")

  # transfer date info
  start.date <- as.POSIXlt(start.date); end.date <- as.POSIXlt(end.date);
  diff.date <- as.numeric(difftime(end.date, start.date))
  date.range <- as.POSIXlt(seq(start.date, by = "days",
                               length = 1 + diff.date))

  for (i in 1:(diff.date+1)) {
    year <- date.range[i]$year + 1900
    month <- date.range[i]$mon + 1
    day <- date.range[i]$mday
    URL.date <- paste(URL.base, "year_", year, "/",
                      ifelse(month >= 10, "month_", "month_0"), month, "/",
                      ifelse(day >= 10, "day_", "day_0"), day, "/", sep = "")

    # grab matchups for today
    ##     URL.scoreboard <- paste(URL.date, "miniscoreboard.xml", sep = "")
    ##     XML.scoreboard <- xmlInternalTreeParse(URL.scoreboard)
    ##     parse.scoreboard <- xpathSApply(XML.scoreboard, "//game[@gameday_link]",
    ##                                     xmlGetAttr, "gameday_link")
    HTML.day <- htmlParse(URL.date)
    parse.day <- xpathSApply(HTML.day, "//a[@*]", xmlGetAttr, "href")
    parse.day <- parse.day[grep("^gid_*", parse.day)]

    # if games exists today
    if (length(parse.day) >= 1) {

      # for each game
      for (game in 1:length(parse.day)) {
        print(game)
        URL.game <- paste(URL.date, parse.day[game], sep = "")
        HTML.game <- htmlParse(URL.game)
        parse.game.exists <- xpathSApply(HTML.game, "//a[@*]", xmlGetAttr, "href")

        # if game.xml exists
        if (sum(match(parse.game.exists, "game.xml"), na.rm = T) > 0) {

          # grab game type (regular season, etc.) and gameday type (pitch f/x, etc.)
          XML.game <- xmlInternalTreeParse(paste(URL.game, "game.xml", sep = ""))
          parse.game <- sapply(c("type", "gameday_sw"), function (x)
                               xpathSApply(XML.game, "//game[@*]", xmlGetAttr, x))

          # if proper game type: "R" ~ regular season, "S" ~ spring, "D" ~ divison series
          # "L" ~ league chamption series, "W" ~ world series
          if (parse.game['type'] == game.type) {
            # grab team names
            parse.teams <- sapply(c("abbrev"), function (x)
                                  xpathSApply(XML.game, "//team[@*]", xmlGetAttr, x))
            home <- parse.teams[1]; away <- parse.teams[2]

            # if pitch f/x data exists
            if (parse.game["gameday_sw"] == "E" | parse.game["gameday_sw"] == "P") {

              # grab number of innings played
              HTML.Ninnings <- htmlParse(paste(URL.game, "inning/", sep = ""))
              parse.Ninnings <- xpathSApply(HTML.Ninnings, "//a[@*]", xmlGetAttr, "href")

              # check to see if game exists data by checking innings > 1
              if (length(grep("^inning_[0-9]", parse.Ninnings)) > 1) {

                # for each inning
                for (inning in 1:length(grep("^inning_[0-9]", parse.Ninnings))) {

                  # grab inning info
                  URL.inning <- paste(URL.game, "inning/", "inning_", inning,
                                      ".xml", sep = "")
                  XML.inning <- xmlInternalTreeParse(URL.inning)
                  parse.atbat <- xpathSApply(XML.inning, "//atbat[@*]")
                  parse.Npitches.atbat <- sapply(parse.atbat, function(x)
                                                 sum(names(xmlChildren(x)) == "pitch"))

                  # check to see if atbat exists
                  if (length(parse.atbat) > 0) {
                    print(paste(parse.day[game], "inning =", inning))

                    # parse attributes from pitch and atbat (ugh, ugly)
                    parse.pitch <- sapply(grab.pitch, function(x)
                                          as.character(xpathSApply(XML.inning, "//pitch[@*]",
                                                                   xmlGetAttr, x)))
                    parse.pitch <- if (class(parse.pitch) == "character") {
                      t(parse.pitch)
                    } else apply(parse.pitch, 2, as.character)
                    results.atbat <- t(sapply(parse.atbat, function(x)
                                              xmlAttrs(x)[grab.atbat]))
                    results.atbat <- results.atbat[rep(seq(nrow(results.atbat)),
                                                       times = parse.Npitches.atbat),]
                    results.atbat <- if (class(results.atbat) == "character") {
                      t(results.atbat)
                    } else results.atbat

                    # write results
                    write(t(cbind(year, month, day, inning, home, away,
                                  results.atbat, parse.pitch)), file = fileloc,
                          ncol = length(c(grab.atbat, grab.pitch)) + length(meta),
                          append = T, sep = ">")
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}



DownloadPitchFX(fileloc = "./2010MLB.txt",start.date="2010-04-05",end.date="2010-10-03")
DownloadPitchFX(fileloc = "./2011MLB.txt",start.date="2011-03-31",end.date="2011-09-28")
DownloadPitchFX(fileloc = "./2012MLB.txt",start.date="2012-04-12",end.date="2012-10-03")




library(XML)

GameID<- function(fileloc = "./GameID.txt",
                            start.date = "2009-05-02", end.date = start.date,
                            URL.base = "http://gd2.mlb.com/components/game/mlb/",
                            game.type = "R") {
  # write initial variables on file
  write("GID", file = fileloc,
        ncol = 1, sep = ">")

  # transfer date info
  start.date <- as.POSIXlt(start.date); end.date <- as.POSIXlt(end.date);
  diff.date <- as.numeric(difftime(end.date, start.date))
  date.range <- as.POSIXlt(seq(start.date, by = "days",
                               length = 1 + diff.date))

  for (i in 1:(diff.date+1)) {
    year <- date.range[i]$year + 1900
    month <- date.range[i]$mon + 1
    day <- date.range[i]$mday
    URL.date <- paste(URL.base, "year_", year, "/",
                      ifelse(month >= 10, "month_", "month_0"), month, "/",
                      ifelse(day >= 10, "day_", "day_0"), day, "/", sep = "")

    # grab matchups for today
    ##     URL.scoreboard <- paste(URL.date, "miniscoreboard.xml", sep = "")
    ##     XML.scoreboard <- xmlInternalTreeParse(URL.scoreboard)
    ##     parse.scoreboard <- xpathSApply(XML.scoreboard, "//game[@gameday_link]",
    ##                                     xmlGetAttr, "gameday_link")
    HTML.day <- htmlParse(URL.date)
    parse.day <- xpathSApply(HTML.day, "//a[@*]", xmlGetAttr, "href")
    parse.day <- parse.day[grep("^gid_*", parse.day)]

    # if games exists today
    if (length(parse.day) >= 1) {

      # for each game
      for (game in 1:length(parse.day)) {
 #       print(game)
 #       URL.game <- paste(URL.date, parse.day[game], sep = "")
 #       HTML.game <- htmlParse(URL.game)
 #       parse.game.exists <- xpathSApply(HTML.game, "//a[@*]", xmlGetAttr, "href")

        # if game.xml exists
  #      if (sum(match(parse.game.exists, "game.xml"), na.rm = T) > 0) {

          # grab game type (regular season, etc.) and gameday type (pitch f/x, etc.)
  #        XML.game <- xmlInternalTreeParse(paste(URL.game, "game.xml", sep = ""))
  #        parse.game <- sapply(c("type", "gameday_sw"), function (x)
  #                             xpathSApply(XML.game, "//game[@*]", xmlGetAttr, x))

          # if proper game type: "R" ~ regular season, "S" ~ spring, "D" ~ divison series
          # "L" ~ league chamption series, "W" ~ world series
  #        if (parse.game['type'] == game.type) {
            # grab team names
  #          parse.teams <- sapply(c("abbrev"), function (x)
  #                                xpathSApply(XML.game, "//team[@*]", xmlGetAttr, x))
  #          home <- parse.teams[1]; away <- parse.teams[2]

            # if pitch f/x data exists
 #           if (parse.game["gameday_sw"] == "E" | parse.game["gameday_sw"] == "P") {

              # grab number of innings played
  #            HTML.Ninnings <- htmlParse(paste(URL.game, "inning/", sep = ""))
  #            parse.Ninnings <- xpathSApply(HTML.Ninnings, "//a[@*]", xmlGetAttr, "href")

              # check to see if game exists data by checking innings > 1
  #            if (length(grep("^inning_[0-9]", parse.Ninnings)) > 1) {

                # for each inning
  #              for (inning in 1:length(grep("^inning_[0-9]", parse.Ninnings))) {

                  # grab inning info
  #                URL.inning <- paste(URL.game, "inning/", "inning_", inning,
  #                                    ".xml", sep = "")
  #                XML.inning <- xmlInternalTreeParse(URL.inning)
  #                parse.atbat <- xpathSApply(XML.inning, "//atbat[@*]")
  #                parse.Npitches.atbat <- sapply(parse.atbat, function(x)
  #                                               sum(names(xmlChildren(x)) == "pitch"))

                  # check to see if atbat exists
  #                if (length(parse.atbat) > 0) {
  #                  print(paste(parse.day[game], "inning =", inning))

                    # parse attributes from pitch and atbat (ugh, ugly)
  #                  parse.pitch <- sapply(grab.pitch, function(x)
  #                                        as.character(xpathSApply(XML.inning, "//pitch[@*]",
  #                                                                 xmlGetAttr, x)))
  #                  parse.pitch <- if (class(parse.pitch) == "character") {
  #                    t(parse.pitch)
  #                  } else apply(parse.pitch, 2, as.character)
  #                  results.atbat <- t(sapply(parse.atbat, function(x)
  #                                            xmlAttrs(x)[grab.atbat]))
  #                  results.atbat <- results.atbat[rep(seq(nrow(results.atbat)),
  #                                                     times = parse.Npitches.atbat),]
  #                  results.atbat <- if (class(results.atbat) == "character") {
  #                    t(results.atbat)
  #                  } else results.atbat

                    # write results
                    write(substr(parse.day[game],1,nchar(parse.day[game])-1), file = fileloc,
                          ncol = 1, append = T, sep = ">")
                  }
                }
              }
            }



GameID(fileloc = "./GameID10.txt",start.date="2010-04-05",end.date="2010-10-03")
GameID(fileloc = "./GameID11.txt",start.date="2011-03-31",end.date="2011-09-28")
GameID(fileloc = "./GameID12.txt",start.date="2012-04-12",end.date="2012-10-03")



# a function to retrieve the homeplate umpire for every game with pitchfx data

library(XML)

# DownloadPitchFX <- function(fileloc = "./pitchfx.txt",
Downloadump <- function(fileloc = "./pitchfx.txt",
                            start.date = "2009-05-02", end.date = start.date,
                            URL.base = "http://gd2.mlb.com/components/game/mlb/",
                            game.type = "R",
                            grab.atbat = c("b", "s", "o", "batter", "pitcher", "b_height",
                              "stand", "p_throws", "event")) {
  # write initial variables on file
#  meta <- c("Year", "Month", "Day", "Inning", "Home", "Away")
#                    write(t(cbind(year, month, day, home, away,hpumpire)),
#  write(c(meta, grab.atbat), file = fileloc,
#        ncol = length(grab.atbat) + length(meta), sep = ">")
  meta <- c("Year", "Month", "Day", "Home", "Away")
  write(c(meta,"HPUmpire"), file = fileloc,
        ncol = length(meta) + 1, sep = ">")

  # transfer date info
  start.date <- as.POSIXlt(start.date); end.date <- as.POSIXlt(end.date);
  diff.date <- as.numeric(difftime(end.date, start.date))
  date.range <- as.POSIXlt(seq(start.date, by = "days",
                               length = 1 + diff.date))

  for (i in 1:(diff.date+1)) {
    year <- date.range[i]$year + 1900
    month <- date.range[i]$mon + 1
    day <- date.range[i]$mday
print(paste("month=",month,"day=",day))
    URL.date <- paste(URL.base, "year_", year, "/",
                      ifelse(month >= 10, "month_", "month_0"), month, "/",
                      ifelse(day >= 10, "day_", "day_0"), day, "/", sep = "")

    HTML.day <- htmlParse(URL.date)
    parse.day <- xpathSApply(HTML.day, "//a[@*]", xmlGetAttr, "href")
    parse.day <- parse.day[grep("^gid_*", parse.day)]

    # if games exists today
    if (length(parse.day) >= 1) {
      # for each game
      for (game in 1:length(parse.day)) {
        print(game)
        URL.game <- paste(URL.date, parse.day[game], sep = "")
        HTML.game <- htmlParse(URL.game)
        parse.game.exists <- xpathSApply(HTML.game, "//a[@*]", xmlGetAttr, "href")
        # if game.xml exists
        if (sum(match(parse.game.exists, "game.xml"), na.rm = T) > 0) {
          # grab game type (regular season, etc.) and gameday type (pitch f/x, etc.)
          XML.game <- xmlInternalTreeParse(paste(URL.game, "game.xml", sep = ""))
          parse.game <- sapply(c("type", "gameday_sw"), function (x)
                               xpathSApply(XML.game, "//game[@*]", xmlGetAttr, x))

          # if proper game type: "R" ~ regular season, "S" ~ spring, "D" ~ divison series
          # "L" ~ league chamption series, "W" ~ world series
          if (parse.game['type'] == game.type) {
            # grab team names
            parse.teams <- sapply(c("abbrev"), function (x)
                                  xpathSApply(XML.game, "//team[@*]", xmlGetAttr, x))
            home <- parse.teams[1]; away <- parse.teams[2]

            # if pitch f/x data exists
            if (parse.game["gameday_sw"] == "E" | parse.game["gameday_sw"] == "P") {

              # grab number of innings played
              HTML.Ninnings <- htmlParse(paste(URL.game, "inning/", sep = ""))
              parse.Ninnings <- xpathSApply(HTML.Ninnings, "//a[@*]", xmlGetAttr, "href")

              # check to see if game exists data by checking innings > 1
              if (length(grep("^inning_[0-9]", parse.Ninnings)) > 1) {
URL.boxscore <- 
URL.game <- paste(URL.date,parse.day[game],sep="")
URL.boxscore <- paste(URL.game,"boxscore.xml",sep="")
parsed.boxscore <- xmlTreeParse(URL.boxscore)
game.info <- xmlValue(parsed.boxscore$doc[[1]][[6]])
hpumpire <- sub(pattern=".*?HP: (.*). 1B:.*", replacement="\\1",x=game.info)
}
                    write(t(cbind(year, month, day, home, away,hpumpire)),
                                  file = fileloc, 1 + length(meta), append = T, sep = ">")
                  }
                }
              }
            }
          }
  }
}


Downloadump(fileloc="./umps2010.txt",start.date="2010-04-05",end.date="2010-10-03")
Downloadump(fileloc="umps2011.txt",start.date="2011-03-31",end.date="2011-09-28")
Downloadump(fileloc="umps2012.txt",start.date="2012-04-12",end.date="2012-10-03")



