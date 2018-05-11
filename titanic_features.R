#Title
full$Title <- gsub("^.*, (.*?)\\..*$", "\\1", full$Name)
full$Title_grp <- full$Title
full$Title_grp[full$Title %in% c('Lady', 'Mlle', 'Mme', 'Ms', 'Rev', 'the Countess',
                                 'Dona', 'Sir')]  <- 'Mrs'
full$Title_grp[full$Title %in% c('Capt', 'Don', 'Jonkheer', 'Rev')] <- 'Mr'
full$Title_grp[full$Title %in% c('Col', 'Dr', 'Major')] <- 'Master'

#Ticket Room
full$Ticket_Room <- ifelse(!grepl(pattern = "^(\\D)", x = full$Ticket), 'Numeric', gsub("(\\s|\\.).*", '', full$Ticket))
full$Ticket_Room_grp <-full$Ticket_Room
Losers <- c('A', 'A/4', 'A/5', 'A/S', 'A4', 'AQ/3', 'AQ/4','CA', 'Fa', 'Line', 'LP','S', 'SCO/W', 'SOTON/O',
            'SOTON/O2', 'SOTON/OQ', 'W', 'W/C')
full$Ticket_Room_grp[full$Ticket_Room_grp %in% Losers] <- 'Loser Class'
full$Ticket_Room_grp[!full$Ticket_Room_grp %in% c('Loser Class', 'Numeric')] <- 'Maybe Lucky'


#Family Size
full$Fam_Size <- full$Parch + full$SibSp
full$Fam_Size_Desc[full$Fam_Size == 0] <- 'Single'
full$Fam_Size_Desc[between(full$Fam_Size, 1, 3)] <- 'Small'
full$Fam_Size_Desc[between(full$Fam_Size, 4, 6)] <- 'Medium'
full$Fam_Size_Desc[between(full$Fam_Size, 7, 20)] <- 'Large'


name_df <- data.frame(Name = full$Name,
                      Parch = full$Parch,
                      Sibs = full$SibSp)

name_df <- name_df[order(name_df$Name),]
