# Monopoly: 
### Note: Chance Cards and Community Chest are not included, red dice is not included, and trading is 
# not allowed


library(XML)


##################################################
############## SETTING UP BOARD GAME #############
##################################################

### Properties ###

url <- paste("http://www.falstad.com/monopoly.html")
download.file(url,destfile = "properties.html", method = "curl")
properties <- data.frame(readHTMLTable("properties.html",header=TRUE,stringsAsFactors=FALSE))
properties <- properties[,-c(1)]

#Change column names of properties to not include NULL nor periods
colnames(properties) <- sub("NULL.", "", colnames(properties))
colnames(properties) <- gsub(".", "", colnames(properties), fixed=TRUE)

#Change class of columns in properties data frame
types <- sapply(properties, class) # All columns are of format character
for(i in 2:dim(properties)[2]) {
  properties[,i] <- as.numeric(properties[,i])
}
types <- sapply(properties, class) # Now only first column is character and rest are numeric

# Only use American version (get rid of alternate name for properties)
properties$Name <- noquote(sub("/.*", "", properties$Name))
properties$Name <- noquote(sub("*", "", properties$Name, fixed=TRUE))
properties$Name[17] <- noquote("Marvin Gardens")
properties$Name[19] <- noquote("North Carolina Avenue")

#Color Code 
properties$color <- "brown"
properties$color[3:5] <- "light blue"
properties$color[6:8] <- "magenta"
properties$color[9:11] <- "orange"
properties$color[12:14] <- "red"
properties$color[15:17] <- "yellow"
properties$color[18:20] <- "green"
properties$color[21:22] <- "blue"


### Railroads ###

railroads <- matrix(nrow=4, ncol=1, data=as.character(seq(1:4)))
railroads[1] <- noquote("Short Line")
railroads[2] <- noquote("Reading Railroad")
railroads[3] <- noquote("B&O Railroad")
railroads[4] <- noquote("Pennsylvania Railroad")
railroads <- as.data.frame(railroads)
colnames(railroads) <- noquote("Name")
railroads <- data.frame(railroads)
railroads$Name <- as.character(railroads$Name)



### Utilities ### 
utilities <- matrix(nrow=2, ncol=1, data=as.character(seq(1:4)))
utilities[1] <- noquote("Water Works")
utilities[2] <- noquote("Electric Company")
utilities <- as.data.frame(utilities)
colnames(utilities) <- noquote("Name")


### Board Setup ###
board <- read.table(header=TRUE, sep=";", text="
                    Type; Name; 
                    Property; Mediterranean Avenue; 
                    Community Chest; ; 
                    Property; Baltic Avenue; 
                    Income Tax; ;
                    Railroad; Reading Railroad; 
                    Property; Oriental Avenue; 
                    Chance; ;
                    Property; Vermont Avenue; 
                    Property; Connecticut Avenue; 
                    Just Visiting/Jail; ; 
                    Property; St. Charles Place; 
                    Utility; Electric Company; 
                    Property; States Avenue; 
                    Property; Virginia Avenue;
                    Railroad; Pennsylvania Railroad; 
                    Property; St. James Place;
                    Community Chest; ; 
                    Property; Tennessee Avenue; 
                    Property; New York Avenue; 
                    Free Parking; ; 
                    Property; Kentucky Avenue; 
                    Chance; ; 
                    Property; Indiana Avenue; 
                    Property; Illinois Avenue; 
                    Railroad; B&O Railroad;
                    Property; Atlantic Avenue; 
                    Property; Ventnor Avenue;
                    Utility; Water Works;
                    Property; Marvin Gardens; 
                    Go to Jail; ;
                    Property; Pacific Avenue; 
                    Property; North Carolina Avenue; 
                    Community Chest; ;
                    Property; Pennsylvania Avenue;
                    Railroad; Short Line Railroad; 
                    Chance; ;
                    Property; Park Place; 
                    Luxury Tax; ; 
                    Property; Boardwalk; 
                    Go; ;")

board <- board[,-c(3)]
trim.leading <- function (x)  sub("^\\s+", "", x)
board$Name <- trim.leading(board$Name)
board$Type <- trim.leading(board$Type)


#Algorithm for rolling 2 die 
twodieroll <- function() {
  roll1 <- sample(1:6,1) 
  roll2 <- sample(1:6,1)
  totalroll <- roll1 + roll2
  doubles <- ifelse(roll1==roll2, "yes", "no")
  list(totalroll, doubles)
}


#Each player has a position, an amount of money, and properties (and number of houses per property)
#for(i in 1:nplayers)  # possible addition: more than 2 players

player1info <- cbind(data.frame(read.table(header=TRUE, stringsAsFactors = FALSE, text="
                                           Position Money Owned Houses
                                           0 1500 No 0")), rbind(data.frame(properties$Name), noquote("Reading Railroad"), noquote("Pennsylvania Railroad"), noquote("B&O Railroad"), noquote("Short Line Railroad"), noquote("Electric Company"), noquote("Water Works")))
colnames(player1info) <- c("Position", "Money", "Owned", "Houses", "Property")


player2info <- cbind(data.frame(read.table(header=TRUE, stringsAsFactors = FALSE, text="
                                           Position Money Owned Houses
                                           0 1500 No 0")), rbind(data.frame(properties$Name), noquote("Reading Railroad"), noquote("Pennsylvania Railroad"), noquote("B&O Railroad"), noquote(" Short Line Railroad"), noquote("Electric Company"), noquote("Water Works")))
colnames(player2info) <- c("Position", "Money", "Owned", "Houses", "Property")



#Each player rolls one dice to determine who goes first (not sure if I'll add it in)
#first.roll.1 <- sample(1:6,1)
#first.roll.2 <- sample(1:6,1)  
#first.player <- ifelse(first.roll.1 > first.roll.2, noquote("player1"), noquote("player2"))

##################################################
##################### PLAYING ####################
##################################################
colors_player_1 <- matrix(ncol=length(unique(properties$color)), nrow=1)
player_1_sets <- NULL
sets.one <- 0
has.property <- NULL


winner <- NULL
player_1_wins <- NULL
counter <- 0

for(j in 1:10) {
  
  #Reset game
  
  
  player1info <- cbind(data.frame(read.table(header=TRUE, stringsAsFactors = FALSE, text="
                                             Position Money Owned Houses
                                             0 1500 No 0")), rbind(data.frame(properties$Name), noquote("Reading Railroad"), noquote("Pennsylvania Railroad"), noquote("B&O Railroad"), noquote("Short Line Railroad"), noquote("Electric Company"), noquote("Water Works")))
  colnames(player1info) <- c("Position", "Money", "Owned", "Houses", "Property")
  
  
  player2info <- cbind(data.frame(read.table(header=TRUE, stringsAsFactors = FALSE, text="
                                             Position Money Owned Houses
                                             0 1500 No 0")), rbind(data.frame(properties$Name), noquote("Reading Railroad"), noquote("Pennsylvania Railroad"), noquote("B&O Railroad"), noquote("Short Line Railroad"), noquote("Electric Company"), noquote("Water Works")))
  colnames(player2info) <- c("Position", "Money", "Owned", "Houses", "Property")
  
  
  
  while(player1info$Money[1] > 0 & player2info$Money[1] > 0){
    turn <- 1 + counter%%2  # Determine who's turn it is
    
    
    
    
    
    #################################
    #################################
    ####### PLAYER 1 TURN ###########
    #################################
    #################################
    
    if(turn==1){
      
      player1info$Position <- player1info$Position + twodieroll()[[1]]
      
      if(player1info$Position[1] >= 40) {
        player1info$Position <- player1info$Position - 40
        player1info$Money <- player1info$Money + 200
      }
      
      
      if(player1info$Position[1] != 0) {
        
        board.type <- board[player1info$Position[1],1]
        
        #################
        #### CHANCE  ####
        #################
        
        # More information about Chance cards at http://monopoly.wikia.com/wiki/Chance
        # Player 1 lands on chance
        if(board.type=="Chance") {
          chance.number <- sample(1:4, 1)
          
          # Chance 1: Advance to Go (Collect $200)
          if(chance.number==1) {   
            player1info$Position <- 40
            player1info$Money <- player1info$Money + 200
          }
          
          #Chance 2: Advance to Illinois Ave. - If you pass Go, collect $200
          if(chance.number==2) {
            
            # +$200 for passing go if already passed Illinois Ave
            if(player1info$Position[1] > 24){
              player1info$Money <- player1info$Money + 200
            }
            
            player1info$Position <- 24
            
            if(player2info$Owned[14]=="Yes") {
              amount.owed <- properties[14, 4 + number.of.houses]  
              player1info$Money <- player1info$Money - amount.owed
              player2info$Money <- player2info$Money + amount.owed
            }
          }
          
          #Chance #3: Advance to St. Charles Place
          if(chance.number==3) {
            original.position <- player1info$Position
            player1info$Position <- 11
            if(original.position[1] > 11) {
              player1info$Money <- player1info$Money + 200  #Pass go
            }
            if(player2info$Owned[6]=="Yes") {
              amount.owed <- properties[6, 4 + number.of.houses]  
              player1info$Money <- player1info$Money - amount.owed
              player2info$Money <- player2info$Money + amount.owed
            }
          }
          
          #Chance #4: Advance token to nearest Utility. If unowned, you may buy it from the Bank. If owned, throw dice and 
          #pay owner a total ten times the amount thrown. (taken here to mean move forward until you reach a utility)
          if(chance.number==4) {
            if(player1info$Position[1]==22){
              player1info$Position <- 28
              property <- "Water Works"
              one.owns.property <- player1info[which(player1info$Property==board[player1info$Position[1],2]),3]
              two.owns.property <- player2info[which(player2info$Property==board[player1info$Position[1],2]),3]
              buyprop <- ifelse(two.owns.property=="No" & one.owns.property=="No", "Yes", "No")
              player1info$Owned[which(player1info$Property==property)] <- ifelse(buyprop=="Yes", "Yes", "No")
              if(buyprop=="Yes"){
                player1info$Money <-player1info$Money - 150
              }
              if(two.owns.property=="Yes") {
                amount.owed <- 10*twodieroll()[[1]]
                player1info$Money <- player1info$Money - amount.owed
                player2info$Money <- player2info$Money + amount.owed
              }
            }
            if(player1info$Position[1]==7|36){
              player1info$Position <- 12
              property <- "Electric Company"
              one.owns.property <- player1info[which(player1info$Property==board[player1info$Position[1],2]),3]
              two.owns.property <- player2info[which(player2info$Property==board[player1info$Position[1],2]),3]
              buyprop <- ifelse(two.owns.property=="No" & one.owns.property=="No", "Yes", "No")
              player1info$Owned[which(player1info$Property==property)] <- ifelse(buyprop=="Yes", "Yes", "No")
              if(buyprop=="Yes"){
                player1info$Money <- player1info$Money - 150
              }
              if(two.owns.property=="Yes") {
                amount.owed <- 10*twodieroll()[[1]]
                player1info$Money <- player1info$Money - amount.owed
                player2info$Money <- player2info$Money + amount.owed
              }
            }
            
            
          }
          
          #Chance #5: 
          #if(chance.number==5) {
          
          #}
          
        }  # End of chances
        
        
        #################
        ### PROPERTY  ###
        #################
        if(board.type=="Property") {
          property <- board[player1info$Position[1],2]
          one.owns.property <- player1info[which(player1info$Property==board[player1info$Position[1],2]),3]
          two.owns.property <- player2info[which(player2info$Property==board[player1info$Position[1],2]),3]
          buyprop <- ifelse(two.owns.property=="No" & one.owns.property=="No", "Yes", "No")  # Player 1 buys property if player 2 doesn't own it and player 1 doesn't already own it
          
          #Determine if player 1 pays money because he's purchasing the property
          player1info$Money <- ifelse(buyprop=="Yes", player1info$Money[1] - properties[which(properties$Name==property),2], player1info$Money[1]) 
          player1info$Owned[which(player1info$Property==property)] <- ifelse(buyprop=="Yes", "Yes", "No")  #Change P1's status for the property just bought to "Yes"
          # If it was in fact bought.
          
          
          #Determine if player 1 pays money because he's paying player 2 (occurs when P2 owns the property, also dependent on how many houses P2 owns)
          if(two.owns.property=="Yes") {
            # If P2 has no houses on the property P1 just landed on
            number.of.houses <- player2info$Houses[which(player2info$Property==property)]
            amount.owed <- properties[which(properties$Name==property), 4 + number.of.houses]  
            player1info$Money <- player1info$Money - amount.owed
            player2info$Money <- player2info$Money + amount.owed
          }
          # Third case for landing on a property is that P1 owns the property and P2 doesn't, in which case nothing happens, so we won't include it.
        }
        
        
        #################
        ### RAILROAD  ###
        #################
        # If player 1 lands on a railroad
        if(board[player1info$Position,1][1]=="Railroad") {
          two.owns.property <- player2info$Owned[which(player2info$Property==as.character(board[player1info$Position[1],2]))]
          buyprop2 <- ifelse(two.owns.property=="No", "Yes", "No")   # If P1 lands on a railroad that P2 doesn't own, he buys it
          if(buyprop2=="Yes") {
            player1info$Money <- player1info$Money - 200
            player1info$Owned[which(player1info$Property==property)] <- "Yes"   # P1 now owns the railroad
          }
          if(two.owns.property=="Yes") {
            allrailroads.two <- player2info[23:26, 3]
            railroads.two <- NULL
            for(i in 1:length(allrailroads.two)){
              railroads.two[i] <- ifelse(allrailroads.two[i]=="Yes", "1", "0")
            }
            rr.owned.two <- sum(as.numeric(railroads.two))
            amount.owed <- 0
            if(rr.owned.two==1) {
              amount.owed <- 25
            }
            if(rr.owned.two==2) {
              amount.owed <- 50
            }
            if(rr.owned.two==3) {
              amount.owed <- 100
            }
            if(rr.owned.two==4) {
              amount.owed <- 200
            }
            player1info$Money <- player1info$Money - amount.owed
            player2info$Money <- player2info$Money + amount.owed
          }
        }
        if(player1info$Position[1] > 40){
          player1info$Position <- player1info$Position - 40
          player1info$Money <- player1info$Money + 200   # $200 for passing go
        }
      }
      
    }
    
    #############################
    #############################
    
    ##############################
    ##############################
    ##### PLAYER 2 TURN ##########
    ##############################
    ##############################
    
    
    if(turn==2){
      
      player2info$Position <- player2info$Position + twodieroll()[[1]]
      
      if(player2info$Position[1] >= 40) {
        player2info$Position <- player2info$Position - 40
        player2info$Money <- player2info$Money + 200   # $200 for passing go
      }
      if(player2info$Position[1] != 0){
        board.type <- board[player2info$Position[1],1]
        
        
        property <- board[player2info$Position[1],2]
        two.owns.property <- player2info[which(player2info$Property==board[player2info$Position[1],2]),3]
        one.owns.property <- player1info[which(player1info$Property==board[player2info$Position[1],2]),3]
        
        #################
        #### CHANCE  ####
        #################
        
        # More information about Chance cards at http://monopoly.wikia.com/wiki/Chance
        # Player 1 lands on chance
        if(board.type=="Chance") {
          chance.number <- sample(1:4, 1)
          
          # Chance 1: Advance to Go (Collect $200)
          if(chance.number==1) {   
            player2info$Position <- 40
            player2info$Money <- player2info$Money + 200
          }
          
          #Chance 2: Advance to Illinois Ave. - If you pass Go, collect $200
          if(chance.number==2) {
            original.position <- player2info$Position
            player2info$Position <- 24
            if(original.position[1] > 24) {
              player2info$Money <- player2info$Money + 200  #Pass go
            }
            if(player1info$Owned[14]=="Yes") {
              amount.owed <- properties[14, 4 + number.of.houses]  
              player2info$Money <- player2info$Money - amount.owed
              player1info$Money <- player1info$Money + amount.owed
            }
          }
          
          #Chance #3: Advance to St. Charles Place
          if(chance.number==3) {
            original.position <- player2info$Position[1]
            player2info$Position <- 11
            if(original.position > 11) {
              player2info$Money <- player2info$Money + 200  #Pass go
            }
            if(player1info$Owned[6]=="Yes") {
              amount.owed <- properties[6, 4 + number.of.houses]  
              player2info$Money <- player2info$Money - amount.owed
              player1info$Money <- player1info$Money + amount.owed
            }
          }
          
          #Chance #4: Advance token to nearest Utility. If unowned, you may buy it from the Bank. If owned, throw dice and 
          #pay owner a total ten times the amount thrown. (taken here to mean move forward until you reach a railroad)
          if(chance.number==4) {
            if(player2info$Position[1]==22){
              player2info$Position <- 28
              property <- "Water Works"
              one.owns.property <- player1info[which(player1info$Property==board[player2info$Position[1],2]),3]
              two.owns.property <- player2info[which(player2info$Property==board[player2info$Position[1],2]),3]
              buyprop <- ifelse(one.owns.property=="No" & two.owns.property=="No", "Yes", "No")
              player2info$Owned[which(player2info$Property==property)] <- ifelse(buyprop=="Yes", "Yes", "No")
              if(buyprop=="Yes"){
                player2info$Money <- player2info$Money - 150
              }
              if(one.owns.property=="Yes") {
                amount.owed <- 10*twodieroll()[[1]]
                player2info$Money <- player2info$Money - amount.owed
                player1info$Money <- player1info$Money + amount.owed
              }
            }
            if(player2info$Position[1]==7|36){
              player2info$Position <- 12
              property <- "Electric Company"
              one.owns.property <- player1info[which(player1info$Property==board[player2info$Position[1],2]),3]
              two.owns.property <- player2info[which(player2info$Property==board[player2info$Position[1],2]),3]
              buyprop <- ifelse(one.owns.property=="No" & two.owns.property=="No", "Yes", "No")
              player2info$Owned[which(player2info$Property==property)] <- ifelse(buyprop=="Yes", "Yes", "No")
              if(buyprop=="Yes"){
                player2info$Money <- player2info$Money - 150
              }
              if(one.owns.property=="Yes") {
                amount.owed <- 10*twodieroll()[[1]]
                player2info$Money <- player2info$Money - amount.owed
                player1info$Money <- player1info$Money + amount.owed
              }
            }
            
          }
          
          #Chance #5: 
          #if(chance.number==5) {
          
          #}
          
          
          
          #Chance #5: 
          #if(chance.number==5) {
          
          #}
          
        }
        
        
        #################
        ### PROPERTY  ###
        #################
        if(board.type=="Property") {
          property <- board[player2info$Position[1],2]
          two.owns.property <- player2info[which(player2info$Property==board[player2info$Position[1],2]),3]
          one.owns.property <- player1info[which(player1info$Property==board[player2info$Position[1],2]),3]
          buyprop <- ifelse(one.owns.property=="No" & two.owns.property=="No", "Yes", "No")  # Player 2 buys property if player 1 doesn't own it and player 2 doesn't already own it
          
          #Determine if player 2 pays Money because he's purchasing the property
          player2info$Money <- ifelse(buyprop=="Yes", player2info$Money[1] - properties[which(properties$Name==property),2], player2info$Money[1]) 
          player2info$Owned[which(player2info$Property==property)] <- ifelse(buyprop=="Yes", "Yes", "No")  #Change P2's status for the property just bought to "Yes"
          # If it was in fact bought.
          
          
          #Determine if player 2 pays Money because he's paying player 1 (occurs when P2 owns the property, also dependent on how many houses P1 owns)
          if(one.owns.property=="Yes") {
            # If P1 has no houses on the property P2 just landed on
            number.of.houses <- player1info$Houses[which(player1info$Property==property)]
            amount.owed <- properties[which(properties$Name==property), 4 + number.of.houses]  
            player2info$Money <- player2info$Money - amount.owed
            player1info$Money <- player1info$Money + amount.owed
          }
          # Third case for landing on a property is that P2 owns the property and P1 doesn't, in which case nothing happens, so we won't include it.
        }
        
        
        #################
        ### RAILROAD  ###
        #################
        # If player 1 lands on a railroad
        if(board[player2info$Position,1][1]=="Railroad") {
          one.owns.property <- player1info$Owned[which(player1info$Property==board[player2info$Position[1],2])]
          buyprop2 <- ifelse(one.owns.property=="No", "Yes", "No")   # If P1 lands on a railroad that P2 doesn't own, he buys it
          if(buyprop2=="Yes") {
            player2info$Money <- player2info$Money - 200
            player2info$Owned[which(player2info$Property==property)] <- "Yes"   # P1 now owns the railroad
          }
          if(one.owns.property=="Yes") {
            allrailroads.one <- player1info[23:26, 3]
            railroads.one <- NULL
            for(i in 1:length(allrailroads.one)){
              railroads.one[i] <- ifelse(allrailroads.one[i]=="Yes", "1", "0")
            }
            rr.owned.one <- sum(as.numeric(railroads.one))
            amount.owed <- 0
            if(rr.owned.one==1) {
              amount.owed <- 25
            }
            if(rr.owned.one==2) {
              amount.owed <- 50
            }
            if(rr.owned.one==3) {
              amount.owed <- 100
            }
            if(rr.owned.one==4) {
              amount.owed <- 200
            }
            player2info$Money <- player2info$Money - amount.owed
            player1info$Money <- player1info$Money + amount.owed
          }
        }
      }
    }
    counter <- counter+1
  }
  
  if(player1info$Money[1] <= 0){
    winner[j] <- "Player 2"
  }
  
  
  if(player2info$Money[1] <= 0){
    winner[j] <- "Player 1"
  }
  
  if(player1info$Money[1] <= 0 | player2info$Money[1]<=0){
    player_1_wins[j] <- ifelse(winner[j]=="Player 1", 1, 0)
    
    
  }
} 






# Determine which color set(s) player 1 ended with

for(k in 1:length(unique(properties$color))){
  color.property <- subset(properties, color==unique(properties$color)[k])
  
  for(l in 1:dim(color.property)[1]){
    has.property[l] <- as.numeric(ifelse(player1info$Owned[which(player1info$Property==color.property$Name[l])]=="Yes", 1, 0))
    properties.owned <- sum(has.property)
  }
  if(dim(color.property)[1] == properties.owned) {
    sets.one <- sets.one + 1
    player_1_sets[k] <- color.property$color[1]
  }
  colors_player_1[j,k] <- as.numeric(ifelse(unique(properties$color)[k] %in% player_1_sets, 1, 0))
  colnames(colors_player_1) <- unique(properties$color)
  
}
# Combine relevant results
money.diff[j] <- player1info$Money - player2info$Money
results <- data.frame(cbind(player_1_wins), colors_player_1, money.diff)







######


# Determine which color set(s) player 1 ended with
has.property <- NULL
sets.one <- 0
player_1_sets <- NULL
colors_player_1 <- NULL

for(k in 1:length(unique(properties$color))){
  color.property <- subset(properties, color==unique(properties$color)[k])
  for(l in 1:dim(color.property)[1]){
    has.property[l] <- as.numeric(ifelse(player1info$Owned[which(player1info$Property==color.property$Name[l])]=="Yes", 1, 0))
  }
  properties.owned <- sum(has.property)
  if(dim(color.property)[1] == properties.owned) {
    sets.one <- sets.one + 1
    player_1_sets[k] <- color.property$color[1]
  }
  colors_player_1[k] <-as.numeric(ifelse(unique(properties$color)[k] %in% player_1_sets, 1, 0))
  colors_player_1 <- as.data.frame(colors_player_1)
  colnames(colors_player_1) <- unique(properties$color)
  
}
# Combine relevant results
money.diff[j] <- player1info$Money - player2info$Money
results <- data.frame(cbind(player_1_wins), colors_player_1, money.diff)





