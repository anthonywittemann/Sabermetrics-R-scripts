get_home_team_given_attendance <- function(attendance, retrosheet) { 
    home_team <- NULL
    for(i in 1:length(retrosheet$attendance)) { 
        if(retrosheet$attendance[i] == attendance){ 
            home_team <- retrosheet$home[i]
        } 
    }
    home_team
}