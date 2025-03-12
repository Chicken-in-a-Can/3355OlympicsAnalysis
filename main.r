library("ggplot2")

summer_olympics <- read.csv("Athletes_summer_games.csv", header = TRUE)
winter_olympics <- read.csv("Athletes_summer_games.csv", header = TRUE)
regions <- read.csv("regions.csv", header = TRUE)
global_ses <- read.csv("GLOB.SES.csv", header = TRUE)

olympics <- merge(summer_olympics, winter_olympics)
olympics <- olympics[which(olympics$Sport == "Swimming"), ]
olympics$Medal[which(olympics$Medal == "")] <-NA 

# Plotting Year vs Age (in base graphics) where people won a medal
ggplot(data = olympics[!is.na(olympics$Medal), ]) +
  geom_point(mapping = aes(x = Year, y = Age, color = as.factor(Medal))) + 
  labs(x = "Year", y = "Age", title = "Olympics Medal Winners")
