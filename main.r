library("ggplot2")
library("dplyr")

# Load datasets
summer_olympics <- read.csv("Athletes_summer_games.csv", header = TRUE)
winter_olympics <- read.csv("Athletes_summer_games.csv", header = TRUE)
regions <- read.csv("regions.csv", header = TRUE)
global_ses <- read.csv("GLOB.SES.csv", header = TRUE)
continent <- read.csv("Countries by continents.csv", header = TRUE)

# Clean olympics dataset
olympics <- merge(summer_olympics, winter_olympics)
olympics <- olympics[which(olympics$Sport == "Swimming"), ]
olympics$decade <- round(olympics$Year, -1)

olympics$Medal[which(olympics$Medal == "")] <- NA 
olympics$Age[which(olympics$Age == "")] <- NA 

olympics <- filter(olympics, !is.na(Age))

olympics$region <- NULL
olympics$X <- NULL


# Clean regions dataset
names(regions)[names(regions) == "region"] <- "country"
regions$country <- ifelse(regions$notes == "" | regions$notes == "NaN", regions$country, regions$notes)

regions$notes = NULL
regions$X = NULL


# Clean SES dataset
global_ses <- filter(global_ses, "year" >= 1900)
global_ses <- select(global_ses, -unid)
names(global_ses)[names(global_ses) == "year"] <- "decade"

# Merge datasets
merged_data <- left_join(regions, olympics, by = "NOC")
merged_data <- left_join(merged_data, global_ses, by = c("country", "decade"))

mean_ages <- merged_data %>%
  filter(Year == 2000) %>%
  group_by(country) %>%
  summarise(age = mean(Age, na.rm = TRUE)) %>%
  data.frame()
  
ses <- merged_data %>%
  filter(Year == 2000) %>%
  group_by(country) %>%
  summarise(ses = mean(SES, na.rm = TRUE)) %>%
  data.frame()

age_ses <- left_join(mean_ages, ses, by = "country")

# Plotting Year vs Age (in ggplot2) where people won a medal
# Colors based on medal won
ggplot(data = merged_data[!is.na(merged_data$Medal), ]) +
  geom_point(mapping = aes(x = Year, y = Age, color = as.factor(Medal))) + 
  geom_smooth(mapping = aes(x = Year, y = Age, color = as.factor(Medal))) + 
  #geom_jitter(mapping = aes(x = Year, y = Age, color = as.factor(Medal))) + 
  scale_color_manual(values = c("#CD7F32", "#C0C0C0", "#FFD700")) + 
  labs(x = "Competition Year", y = "Athlete Age", title = "Olympics Medal Winners", color = "Medal Won")

ggplot(data = age_ses) + geom_smooth(mapping = aes(x = ses, y = age), method = "lm")


#mean/median age for each year
summer <- read.csv("Athletes_summer_games.csv")                                 
socioeconomic <- read.csv("GLOB.SES.csv")
subset_swim <- summer[which(summer[, "Sport"] == "Swimming"), ]

unique_years <- sort(unique(subset_swim[, "Year"]))
mean_ages <- NULL
median_ages <- NULL
range_ages <- NULL

for (i in unique_years) {
  mean_ages <- c(mean_ages, mean(subset_swim[which(subset_swim[, "Year"] == i), "Age"], na.rm = TRUE))
  median_ages <- c(median_ages, median(subset_swim[which(subset_swim[, "Year"] == i), "Age"], na.rm = TRUE))
  range_ages <- c(range_ages, range(subset_swim[which(subset_swim[, "Year"] == i), "Age"], na.rm = TRUE))
}

mean_ages
median_ages
range_ages
