# For what proportion of motor vehicle thefts in 2001 was an arrest made?
# theft total in 2011:
summary(crime_data$Year == 2011)
#15637
#HOW MANY OF IT WAS ARREST MADE?
summary(crime_data$Year == 2011 & crime_data$Arrest == TRUE)
#625.. 625/15637 = 0,039 wrong answer

#Use the table function then..
table(crime_data$Year, crime_data$Arrest) #bingo!

Top5 <- subset(crime_data, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")

#One of the locations has a much higher arrest rate than the other locations. Which is it? 
table(Top5$LocationDescription, Top5$Arrest)
newcolumn <- c(249/(249+2059), 132/(132+1543), 439/(439+1672), 1603/(1603+13249), 11595/(11595+144969))
Top5_data <- table(Top5$LocationDescription, Top5$Arrest)
cbind(Top5_data, newcolumn)
