#Title
full$Title <- gsub("^.*, (.*?)\\..*$", "\\1", full$Name)

#Ticket Room extraction
full$Ticket_Room <- ifelse(!grepl(pattern = "^(\\D)", x = full$Ticket), 'Numeric', gsub("(\\s|\\.).*", '', full$Ticket))

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
