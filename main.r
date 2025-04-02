library("ggplot2")

summer_olympics <- read.csv("Athletes_summer_games.csv", header = TRUE)
winter_olympics <- read.csv("Athletes_summer_games.csv", header = TRUE)
regions <- read.csv("regions.csv", header = TRUE)
global_ses <- read.csv("GLOB.SES.csv", header = TRUE)

olympics <- merge(summer_olympics, winter_olympics)
olympics <- olympics[which(olympics$Sport == "Swimming"), ]
olympics$Medal[which(olympics$Medal == "")] <-NA 

# Plotting Year vs Age (in ggplot2) where people won a medal
# Colors based on medal won
ggplot(data = olympics[!is.na(olympics$Medal), ]) +
  geom_point(mapping = aes(x = Year, y = Age, color = as.factor(Medal))) + 
  scale_color_manual(values = c("#CD7F32", "#C0C0C0", "#FFD700")) + 
  labs(x = "Competition Year", y = "Athlete Age", title = "Olympics Medal Winners", color = "Medal Won")

#mean/median age for each year
summer <- read.csv("Athletes_summer_games.csv")                                 
socioeconomic <- read.csv("GLOB.SES.csv")
subset_swim <- summer[which(summer[, "Sport"] == "Swimming"), ]

unique_years <- sort(unique(subset_swim[, "Year"]))
mean_ages <- NULL
median_ages <- NULL

for (i in unique_years) {
  print(i)
  mean_ages <- c(mean_ages, mean(subset_swim[which(subset_swim[, "Year"] == i), "Age"], na.rm = TRUE))
  median_ages <- c(median_ages, median(subset_swim[which(subset_swim[, "Year"] == i), "Age"], na.rm = TRUE))
}
mean_ages
median_ages
