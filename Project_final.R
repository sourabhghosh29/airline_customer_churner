######################################
#IST 687, NPS Project
#
#Student Name: Jimit Mistry
#
# Attribution statement:
# 1.  I did this homework by myself, with help from the book and the professor

# Run these three functions to get a clean test of homework code
#dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

# Set working directory 
# Change to the folder containing your homework data files
setwd("/Users/mistr/OneDrive/Documents/IST687/Homework")

library(tidyverse)
library(RCurl)
library(jsonlite)
library(imputeTS)
library(ggplot2)
library(ggmap)
library(arules)
library(arulesViz)
library(tm)

df_nps <- jsonlite::fromJSON("./fall2019-survey-M09.json")
View(df_nps)

na_values <- apply(apply(df_nps, 2, is.na), 2, which)
na_values
View(na_values)

Destination_plot <- ggplot(df_nps, aes(x = "",y = df_nps$Destination.City, fill = df_nps$Destination.City))+
  geom_bar(stat = "identity")
Destination_plot

prop.table(table(df_nps$Airline.Status))
prop.table(table(df_nps$Airline))

ggplot(df_nps, aes(y=df_nps$Likelihood.to.recommend, x = df_nps$Destination.City))+
  geom_point()

#Create Recommender type
df_nps$type_passenger <- cut(df_nps$Likelihood.to.recommend,
                             breaks = c(-1,7,9,Inf),
                             labels = c("detractor","passive","promoter"))
glimpse(df_nps)

df_nps$type_passenger <- factor(df_nps$type_passenger, ordered = TRUE)
df_nps$type_passenger[which(is.na(df_nps$type_passenger))] = "passive"

str(df_nps$type_passenger)

# Create Arrival delay > 5 boolean
df_nps$Arrival_Delay_greater_than_5 <- (df_nps$Arrival.Delay.in.Minutes > 5)
str(df_nps$Arrival_Delay_greater_than_5)

# Create Long trip > 1100 boolean
quantile(df_nps$Flight.Distance)
df_nps$Long_Trip <- df_nps$Flight.Distance > 1100

# Create Flight_time
df_nps$flight_day_part <- cut(df_nps$Scheduled.Departure.Hour,
                             breaks = c(-1,6,12,17,19,Inf),
                             labels = c("late night","morning","afternoon","evening","night"))
glimpse(df_nps)

# Plots


ggplot(df_nps, aes(x = type_passenger))+
  geom_histogram(stat = "count")

prop.table(table(df_nps$type_passenger))

ggplot(df_nps, aes(x = "",y = "", fill = type_passenger))+
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start = 0)


# Apriori Transactions
glimpse(df_nps)
dim(df_nps)
df_nps1 = df_nps[,c(3,5,10,14,33,34,35,36)]

apply(apply(df_nps1, 2, is.na), 2, which)
df_nps1$type_passenger[which(is.na(df_nps1$type_passenger))] = "passive"

df_npsX <- as(df_nps1, "transactions")

inspect(df_npsX)
itemFrequency(df_npsX)
itemFrequencyPlot(df_npsX)

ruleset <- apriori(df_npsX,
                   # Specify threshold of 0.005 for support, and 0.5 for confidence. That is, show only those values that are higher than the thresholds.
                   parameter = list(support=0.05, confidence = 0.05, minlen = 3),
                   # Specify the rhs as "Survived = Yes", and all the rest of the variables of the transaction as lhs of the equation
                   appearance = list(default="lhs", rhs = ("type_passenger=detractor")))
inspect(ruleset)
inspectDT(ruleset)


#Promoter rules
#{Type.of.Travel=Business travel,Class=Eco}	{type_passenger=promoter}	0.110	0.222	1.441	1,132.000

#Detractor
# {Airline.Status=Blue,Type.of.Travel=Personal Travel}	{type_passenger=detractor}	0.213	0.886	1.997	2,186.000
# {Airline.Status=Blue,Type.of.Travel=Personal Travel,Class=Eco}	{type_passenger=detractor}	0.174	0.885	1.995	1,791.000
# {Gender=Female,Type.of.Travel=Personal Travel,Class=Eco,Arrival_Delay_greater_than_5}	{type_passenger=detractor}	0.055	0.928	2.091	565.000
# {Type.of.Travel=Personal Travel,Class=Eco,Arrival_Delay_greater_than_5}	{type_passenger=detractor}	0.084	0.921	2.076	859.000
# {Type.of.Travel=Personal Travel,Arrival_Delay_greater_than_5}	{type_passenger=detractor}	0.100	0.920	2.075	1,028.000
# {Airline.Status=Blue,Type.of.Travel=Personal Travel,flight_day_part=afternoon}	{type_passenger=detractor}	0.072	0.903	2.036	738.000
# {Airline.Status=Blue,Type.of.Travel=Personal Travel,Class=Eco,flight_day_part=afternoon}	{type_passenger=detractor}	0.059	0.902	2.034	610.000
# {Airline.Status=Blue,Gender=Female,Type.of.Travel=Personal Travel}	{type_passenger=detractor}	0.140	0.897	2.021	1,441.000


boxplot(df_nps$Age ~ df_nps$type_passenger)

# type_passenger vs Price.Sensitivity
#df_nps_price <- df_nps %>%
#  select(Price.Sensitivity, type_passenger) %>%
#  group_by(type_passenger)

#qplot(Price.Sensitivity , type_passenger, data=df_nps_price, geom=c("jitter"),
#     fill=Price.Sensitivity, main="Price sensitivity by recommender type",
#    xlab="", ylab="Recommender Type")
df_nps %>%
  select(Price.Sensitivity, type_passenger) %>%
  group_by(type_passenger) %>%
  ggplot(aes(x = Price.Sensitivity, y = type_passenger))+
    geom_jitter(alpha = 0.4, color = 'light green')+
      theme(
        panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "orange"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "orange")
  )

# tpye_passenger vs Gender

df_nps %>%
  select(Gender, type_passenger) %>%
  group_by(type_passenger) %>%
  ggplot(aes(x = Gender, y = type_passenger))+
    geom_jitter(alpha = 0.4, color = 'light green')+
      theme(
        panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "orange"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "orange")
  )

# type_passenger vs Type.of.Travel

df_nps %>%
  select(Type.of.Travel, type_passenger) %>%
  group_by(type_passenger) %>%
  ggplot(aes(x = Type.of.Travel, y = type_passenger))+
    geom_jitter(alpha = 0.4, color = 'light green')+
      theme(
        panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "orange"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "orange")
  )

# type_passenger vs Airline.Status

df_nps %>%
  select(Airline.Status, type_passenger) %>%
  group_by(type_passenger) %>%
  ggplot(aes(x = Airline.Status, y = type_passenger))+
  geom_jitter(alpha = 0.4, color = 'light green')+
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "orange"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "orange")
  )


# type_passenger vs Class

df_nps %>%
  select(Class, type_passenger) %>%
  group_by(type_passenger) %>%
  ggplot(aes(x = Class, y = type_passenger))+
  geom_jitter(alpha = 0.4, color = 'light green')+
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "orange"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "orange")
  )

ggplot(df_nps, aes(Age))+
  geom_histogram(aes(fill = type_passenger), bins = 10, color = 'Black')


ggplot(df_nps, aes(Arrival.Delay.in.Minutes))+
  geom_histogram(aes(fill = type_passenger), bins = 10, color = 'Black')

ggplot(df_nps, aes(Flight.cancelled))+
  geom_histogram(aes(fill = type_passenger), bins = 10, color = 'Black', stat = "count")

ggplot(df_nps, aes(Flight.Distance))+
  geom_histogram(aes(fill = type_passenger), bins = 10, color = 'Black')

ggplot(df_nps, aes(Loyalty))+
  geom_histogram(aes(fill = type_passenger), bins = 10, color = 'Black')

ggplot(df_nps, aes(Shopping.Amount.at.Airport))+
  geom_histogram(aes(fill = type_passenger), bins = 10, color = 'Black')

ggplot(df_nps, aes(Eating.and.Drinking.at.Airport))+
  geom_histogram(aes(fill = type_passenger), bins = 10, color = 'Black')

ggplot(df_nps, aes(flight_day_part))+
  geom_histogram(aes(fill = type_passenger), bins = 10, color = 'Black', stat = "count")

ggplot(df_nps, aes(flight_day_part))+
  geom_histogram(aes(fill = Arrival_Delay_greater_than_5), bins = 10, color = 'Black', stat = "count")

ggplot(df_nps, aes(type_passenger))+
  geom_histogram(aes(fill = Arrival_Delay_greater_than_5), bins = 10, color = 'Black', stat = "count")

# map plt
us <- map_data("state")
df_nps$Origin.State <- tolower(df_nps$Origin.State)
df_nps$Destination.State <- tolower(df_nps$Destination.State)

flight_state_destination <- ggplot(df_nps, aes(map_id = Destination.State))+
  geom_map(map = us, aes(fill = type_passenger), color = 'Black')+
  expand_limits(x = us$long, y = us$lat)+
  #geom_point(aes(x = dlong, y = dlat))+
  ggtitle("Destination State")+
  coord_map()
flight_state_destination

flight_state_origin <- ggplot(df_nps, aes(map_id = Origin.State))+
  geom_map(map = us, aes(fill = type_passenger), color = 'Black')+
  expand_limits(x = us$long, y = us$lat)+
  #geom_point(aes(x = olong, y = olat))+
  ggtitle("Origin State")+
  coord_map()
flight_state_origin



# Find important words from freeText
which(!is.na(df_nps$freeText))
comments <- df_nps$freeText[which(!is.na(df_nps$freeText))]

str(comments)
View(comments)
comments.vec <- VectorSource(comments)
comments.corpus <- Corpus(comments.vec)

comments.corpus <- tm_map(comments.corpus, content_transformer(tolower))
comments.corpus <- tm_map(comments.corpus, removePunctuation)
comments.corpus <- tm_map(comments.corpus, removeNumbers)
comments.corpus <- tm_map(comments.corpus, removeWords, stopwords("english"))

inspect(comments.corpus)

#as(comments.corpus, "transactions")

tdm <- TermDocumentMatrix(comments.corpus)

inspect(tdm)

wordCounts <- rowSums(as.matrix(tdm))
wordCounts <- sort(wordCounts, decreasing = TRUE)

View(wordCounts)

#df_nps$Gender <- factor(df_nps$Gender, labels = "")
#df_nps$Airline.Status <- factor(df_nps$Airline.Status, labels = "")
#df_nps$Type.of.Travel <- factor(df_nps$Type.of.Travel, labels = "")
#df_nps$Class <- factor(df_nps$Class, labels = "")
#df_nps$Partner.Code <- factor(df_nps$Partner.Code, labels = "")
#df_nps$Partner.Name<- factor(df_nps$Partner.Name, labels = "")
#df_nps$Origin.City<- factor(df_nps$Origin.City, labels = "")
#df_nps$Origin.State<- factor(df_nps$Origin.State, labels = "")
#df_nps$Destination.City<- factor(df_nps$Destination.City, labels = "")
#df_nps$Destination.State<- factor(df_nps$Destination.State, labels = "")
