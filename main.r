library("ggplot2")
library("dplyr")

# Load datasets
summer_olympics <- read.csv("Athletes_summer_games.csv", header = TRUE)
winter_olympics <- read.csv("Athletes_summer_games.csv", header = TRUE)
regions <- read.csv("regions.csv", header = TRUE)
global_ses <- read.csv("GLOB.SES.csv", header = TRUE)
continent <- read.csv("Countries by continents.csv", header = TRUE)
#rename column names for continent so it matches with olympics data
colnames(continent) <- c("continent", "country")

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
merged_data <- left_join(merged_data, global_ses, by = c("country", "decade")) %>%
  left_join(continent, by = "country") #join continent data with merged_data

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

#April 22, 2025

#Career Length by Gender

#data frame with just name and gender
name_gender <- merged_data %>% select(Name, Sex, continent)

career <- merged_data %>%
  group_by(Name) %>% #for each athlete
  summarise(career_length = max(Year) - min(Year)) %>% #find difference in last and first olympics
  left_join(name_gender, by = "Name") #join with data frame containing name and gender of athlete
  
#to consider: do we want to dispose of NA continent?
ggplot(data = career) + geom_bar(mapping = aes(x = career_length, fill = Sex))
ggplot(data = career) + geom_bar(mapping = aes(x = career_length, fill = continent))

# Year by Average Age - compare countries, color by gender
#cont_gen <- merged_data %>% select(Sex, country, continent)
avg_age <- merged_data %>%
  group_by(country, Year) %>% #for each year and country
  summarise(mean_age = mean(Age, na.rm = TRUE)) %>% #find the mean age
  left_join(continent, by = "country") #join with continent

ggplot(data = avg_age) + 
  geom_smooth(mapping = aes(x = Year, y = mean_age, color = continent), se = FALSE)

#tried doing stuff with gender but it got weird with a many to many relationship

            
