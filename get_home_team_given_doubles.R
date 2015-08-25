get_home_team_given_doubles <- function(doubles, retrosheet) { 
    home_team <- NULL
    for(i in 1:length(retrosheet$home_2b)) { 
        condition <- retrosheet$home2b[i] == doubles
        if(!is.null(condition) && length(condition) == 1 && !is.na(condition) && condition){
            home_team <- retrosheet$home[i]
            break
        }
#        else{
#            print(str(condition))
#        }
    }
    home_team
}