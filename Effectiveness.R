library(tidyverse)
library(Lahman)
library(modelr)

results_data <- read.csv("/Users/jpwong/Dropbox/My Mac (JPs-MacBook.local)/Downloads/savant_data-7.csv")

# get all the pitchers and filter out by how many pitches they have thrown
savant_data %>%
  distinct(player_name) -> pitchersdf
all_pitchers <- pitchersdf$player_name

# some_pitchers <- c()
# for (f in all_pitchers){
#   if (sum(savant_data$player_name == f) > 1250 ){
#     some_pitchers <- append(some_pitchers,f)
#   }
# }

# getPercentages returns a vector of size 5 where the first is ball% then strike%  then in play% then wOBA bip then thrown%
getPercentages <- function(total, table){
  events <- table$events
  wOBA <- (sum(events == "single")*.888)+(sum(events == "double")*1.271) + (sum(events == "walk")*.69) + (sum(events == "home_run")*2.101)+(sum(events == "triple")*1.616)+(sum(events == "hit_by_pitch")*.722)
  inPlay <- sum(events == "single") + sum(events == "double") + sum(events == "triple") + sum(events == "home_run") + sum(events == "strikeout") + sum(events == "field_out") + sum(events == "walk") + sum(events == "force_out")+ sum(events == "sac_fly")+ sum(events == "hit_by_pitch")+ sum(events == "grounded_into_double_play")+ sum(events == "fielders_choice")+ sum(events == "strikeout_double_play")+ sum(events == "fielders_choice_out")+ sum(events == "double_play")+ sum(events == "sac_fly_double_play")+ sum(events == "triple_play")
  btable <- subset(table, events != "walk")
  ballPercent <- sum (btable$description == "ball" |btable$description == "blocked_ball" ) /nrow(table)
  stable <- subset(table, events != "strikeout" & events != "strikeout_double_play")
    if (table$strikes [1] == 2 & length(table$strikes) != 0){
      stable <- subset(stable, description != "foul")
    }
  strikePercent <- sum (stable$description == "called_strike" | stable$description == "swinging_strike" |stable$description == "swinging_strike_blocked" |stable$description == "foul_tip" |stable$description == "foul" )/nrow(table)
  inPlayPercent <- inPlay/nrow(table)
  if (!inPlay == 0){
    wOBA <- wOBA/inPlay
  } else {
    wOBA == 0
  }
  thrownPercent <- nrow(table)/total * 100
  c(ballPercent,strikePercent, inPlayPercent,wOBA, thrownPercent)
}
pitcherENumbers <- data.frame (Names = character(), PME = double(), WobaE = double(), Woba = double() )


#for (p  in some_pitchers){
  nombre <- "Romo, Sergio"
  totaltable <- data.frame(emptycol = 0)

savant_data %>%
  filter(player_name == nombre) %>%
  distinct(pitch_name) -> uniquePitches
pitches <- uniquePitches$pitch_name
pitches <- pitches[!pitches %in% ""]

#filter rarely used pitches!!
for (p in pitches){
  t <- nrow(subset(savant_data, player_name == nombre)) * .05
  if (nrow(subset(savant_data, player_name == nombre & pitch_name == p)) < t){
    pitches <- pitches[pitches != p]
  }
    
}

totaldifferencemult <- 0
totalPitchEffectiveness <- 0

for (num_balls in 0:3){
  for (num_strikes in 0:2){
    pitcher_df <- subset(savant_data, player_name == nombre & num_balls == balls & num_strikes == strikes)

    total <- nrow(filter(pitcher_df, pitch_name %in% pitches & description == "hit_into_play" | description == "ball"| description == "called_strike"| description == "foul"| description == "swinging_strike"| description == "swinging_strike_blocked"| description == "blocked_ball"| description == "foul_tip"|description == "hit_by_pitch"))
  
    pitcherTable <- data.frame(emptycol = 0)
  
  
  

    for (pitch in 1:length(pitches)){
      typeOfPitch <- pitches[pitch]
    
     
    
    
      pitch_df <- savant_data %>%
        subset(pitch_name == pitches[pitch] & player_name == nombre & balls == num_balls & strikes == num_strikes)
      
      
      if (num_balls == 0 & num_strikes == 0){
        wOBAStrike <- -.028
        wOBABall <- .002
      }else if (num_balls == 0 & num_strikes == 1) {
        wOBAStrike <- -.058
        wOBABall <- .009
      }else if (num_balls == 0 & num_strikes == 2) {
        wOBAStrike <- 0
        wOBABall <- .015
      }else if (num_balls == 1 & num_strikes == 0) {
        wOBAStrike <- -.021
        wOBABall <- .030
      }else if (num_balls == 1 & num_strikes == 1) {
        wOBAStrike <- -.052
        wOBABall <- .034
      }else if (num_balls == 1 & num_strikes == 2) {
        wOBAStrike <- 0
        wOBABall <- .038
      }else if (num_balls == 2 & num_strikes == 0) {
        wOBAStrike <- -.017
        wOBABall <- .086
      }else if (num_balls == 2 & num_strikes == 1) {
        wOBAStrike <- -.048
        wOBABall <- .085
      }else if (num_balls == 2 & num_strikes == 2) {
        wOBAStrike <- 0
        wOBABall <- .087
      }else if (num_balls == 3 & num_strikes == 0) {
        wOBAStrike <- -.018
        wOBABall <- .69
      }else if (num_balls == 3 & num_strikes == 1) {
        wOBAStrike <- -.046
        wOBABall <- .69
      }else {
        wOBAStrike <- 0
        wOBABall <- .69
      }
      
      heart_df <- subset(pitch_df, Attack_Zone == "heart")
      heartVec <- getPercentages(total, heart_df)
      shadowIn_df <- subset(pitch_df, Attack_Zone == "shadow_in")
      shadowInVec <- getPercentages(total, shadowIn_df)
      shadowOut_df <- subset(pitch_df, Attack_Zone == "shadow_out")
      shadowOutVec <- getPercentages(total, shadowOut_df)
      chase_df <- subset(pitch_df, Attack_Zone == "chase")
      chaseVec <- getPercentages(total, chase_df)
      waste_df <- subset(pitch_df, Attack_Zone == "waste")
      wasteVec <- getPercentages(total, waste_df)
      
    

      heartWOBA <- c(heartVec[1]*wOBABall+ heartVec[2]*wOBAStrike +  heartVec[3]*heartVec[4])
      shadowInWOBA<-c(shadowInVec[1]*wOBABall+ shadowInVec[2]*wOBAStrike+ shadowInVec[3]*shadowInVec[4])
      shadowOutWOBA<-c(shadowOutVec[1]*wOBABall+ shadowOutVec[2]*wOBAStrike+ shadowOutVec[3]*shadowOutVec[4])
      chaseWOBA<-c(chaseVec[1]*wOBABall+ chaseVec[2]*wOBAStrike+ chaseVec[3]*chaseVec[4])
      wasteWOBA<-c(wasteVec[1]*wOBABall+ wasteVec[2]*wOBAStrike+ wasteVec[3]*wasteVec[4])
    
  
 
    
      hvec <- c(heartWOBA, heartVec[5])
      pitcherTable <- cbind (pitcherTable, v1= hvec) 
      names(pitcherTable)[names(pitcherTable) == "v1"] <- paste(typeOfPitch,"heart", sep=" ") 
      
      s1vec <- c(shadowInWOBA, shadowInVec[5])
      pitcherTable <- cbind (pitcherTable, v1= s1vec)
      names(pitcherTable)[names(pitcherTable) == "v1"] <- paste(typeOfPitch,"shadow in", sep=" ")  
    
      s2vec <- c(shadowOutWOBA, shadowOutVec[5])
      pitcherTable <- cbind (pitcherTable, v1= s2vec)
      names(pitcherTable)[names(pitcherTable) == "v1"] <- paste(typeOfPitch,"shadow out", sep=" ")
    
      cvec <- c(chaseWOBA, chaseVec[5])
      pitcherTable <- cbind (pitcherTable, v1= cvec)
      names(pitcherTable)[names(pitcherTable) == "v1"] <- paste(typeOfPitch,"chase", sep=" ") 
      
      wvec <- c(wasteWOBA, wasteVec[5])
      pitcherTable <- cbind (pitcherTable, v1= wvec)
      names(pitcherTable)[names(pitcherTable) == "v1"] <- paste(typeOfPitch,"waste", sep=" ") 
      
      
      
      
      }
  pitcherTable <- select(pitcherTable, -emptycol)
  inverse<- t(pitcherTable)
  vec1 <- inverse[,1]
  vec1 <- as.vector(vec1)
  vec2 <- inverse[,2]
  vec2 <- as.vector(vec2)
  totaltable <- cbind( totaltable, inverse)
  stringCount <- paste(num_balls, num_strikes, sep = "-")
  names(totaltable)[ncol(totaltable) -1] <- paste("wOBA", stringCount, sep = " ")
  names(totaltable)[ncol(totaltable) ] <- paste("usage", stringCount, sep = " ")
  
  vector <- vec1 * vec2
  totaltable %>%
    mutate (name = vector) -> totaltable
  names(totaltable)[ncol(totaltable)] <- paste("mult", stringCount, sep = " ") 
  
  
  
  
  totaldifferencemult <- as.numeric(totaldifferencemult)
  
  totaldifferencemult <- as.numeric(totaldifferencemult) + as.numeric(sum(totaltable[ncol(totaltable)], na.rm = TRUE))
  totalPitchEffectiveness <- as.numeric(totalPitchEffectiveness)
  totalPitchEffectiveness <- as.numeric(totalPitchEffectiveness) + as.numeric(sum(totaltable[ncol(totaltable) -2 ], na.rm = TRUE))
  
  }
}




select(totaltable, -emptycol) -> totaltable

  



results_data %>%
  filter(player_name == nombre) %>%
  select(woba) -> wobacon
wobacon <- as.numeric(wobacon)

totalPitchEffectiveness <- totalPitchEffectiveness/length(pitches)
vec <- c(nombre,as.numeric(totaldifferencemult), as.numeric(totalPitchEffectiveness), wobacon)
pitcherENumbers <- rbind(pitcherENumbers, vec)
colnames(pitcherENumbers)<- c("names", "PME", "WobaE", "Woba")
pitcherENumbers$PME <- as.numeric(pitcherENumbers$PME)
pitcherENumbers$WobaE <- as.numeric(pitcherENumbers$WobaE)
pitcherENumbers$Woba <- as.numeric(pitcherENumbers$Woba)
#}




pitcherENumbers %>%
  mutate (difference = E2/Woba) %>%
  arrange(desc(difference))


m1 <- lm(pitcherENumbers$PME ~ pitcherENumbers$Woba)  #Create a linear model
pitcherENumbers %>%
  add_residuals(m1) -> pitcherENumbers

ggplot(pitcherENumbers, aes(Woba, PME)) + geom_point() + geom_smooth(method='lm') + geom_text(aes(label = names), check_overlap = TRUE)

hist(pitcherENumbers$PME, main = "PME Histogram", xlab = "PME")
mean(pitcherENumbers$PME) -> m
stdvar <- sqrt(var(pitcherENumbers$PME))
