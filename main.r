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

regions <- regions %>%
  mutate(country = replace(country, country == "USA", "United States")) %>%
  mutate(country = replace(country, country == "UK", "United Kingdom"))

# Clean SES dataset
global_ses <- filter(global_ses, "year" >= 1900)
global_ses <- select(global_ses, -unid)
names(global_ses)[names(global_ses) == "year"] <- "decade"

# Merge datasets
merged_data <- left_join(regions, olympics, by = "NOC")
merged_data <- left_join(merged_data, global_ses, by = c("country", "decade"))
merged_data <- left_join(merged_data, continent, by = "country") #join continent data with merged_data

# Create data for age cs ses, duplicates allowed
mean_ages <- merged_data %>%
  filter(Year %in% c(1900, 1920, 1960, 1980, 2000)) %>%
  group_by(Year, country) %>%
  summarise(age = mean(Age, na.rm = TRUE)) %>%
  data.frame()

ses <- merged_data %>%
  filter(Year %in% c(1900, 1920, 1960, 1980, 2000)) %>%
  group_by(Year, country) %>%
  summarise(ses = mean(SES, na.rm = TRUE)) %>%
  data.frame()

age_ses <- left_join(mean_ages, ses, by = c("country", "Year"))

age_ses$Year.y = NULL
names(age_ses)[names(age_ses) == "Year.x"] <- "Year"

# Create data for age cs ses, no duplicates allowed
mean_ages_no_dup <- merged_data %>%
  filter(Year %in% c(1900, 1920, 1960, 1980, 2000)) %>%
  #filter(country %in% c("United States", "Argentina", "Egypt", "Germany", "Japan", "India", "Australia")) %>%
  distinct(Name, Year, .keep_all = TRUE) %>%
  group_by(Year, country) %>%
  summarise(age = mean(Age, na.rm = TRUE)) %>%
  data.frame()

ses_no_dup <- merged_data %>%
  filter(Year %in% c(1900, 1920, 1960, 1980, 2000)) %>%
  #filter(country %in% c("United States", "Argentina", "Egypt", "Germany", "Japan", "India", "Australia")) %>%
  distinct(Name, Year, .keep_all = TRUE) %>%
  group_by(Year, country) %>%
  summarise(ses = mean(SES, na.rm = TRUE)) %>%
  data.frame()

age_ses_no_dup <- left_join(mean_ages_no_dup, ses_no_dup, by = c("country", "Year"))

age_ses_no_dup$Year.y = NULL
names(age_ses_no_dup)[names(age_ses_no_dup) == "Year.x"] <- "Year"

# Plotting Year vs Age (in ggplot2) where people won a medal
# Colors based on medal won
ggplot(data = merged_data[!is.na(merged_data$Medal), ]) +
  geom_point(mapping = aes(x = Year, y = Age, color = as.factor(Medal))) + 
  geom_smooth(mapping = aes(x = Year, y = Age, color = as.factor(Medal))) + 
  #geom_jitter(mapping = aes(x = Year, y = Age, color = as.factor(Medal))) + 
  scale_color_manual(values = c("#CD7F32", "#C0C0C0", "#FFD700")) + 
  labs(x = "Competition Year", y = "Athlete Age", title = "Olympics Medal Winners", color = "Medal Won")

# plotting age data vs SES. Duplicates allowed
ggplot(data = age_ses) + geom_smooth(mapping = aes(x = ses, y = age), method = "lm") +
  facet_wrap(vars(Year)) + 
  labs(title = "Age of athletes vs SES across time") +
  xlab("Socio-economic Score") +
  ylab("Age of Athletes")

# plotting age data vs SES. Duplicates not allowed
ggplot(data = age_ses_no_dup) + geom_smooth(mapping = aes(x = ses, y = age), method = "lm") +
  facet_wrap(vars(Year)) + 
  labs(title = "Age of athletes vs SES across time") +
  xlab("Socio-economic Score") +
  ylab("Age of Athletes")

#April 22, 2025

#Career Length by Gender

#data frame with just name and gender
name_gender <- merged_data %>% select(Name, Sex, continent)

career <- merged_data %>%
  group_by(Name) %>% #for each athlete
  summarise(career_length = max(Year) - min(Year)) %>% #find difference in last and first olympics
  left_join(name_gender, by = "Name") #join with data frame containing name and gender of athlete

career <- career[which(!is.na(career[,"continent"])), ]

#to consider: do we want to dispose of NA continent?
ggplot(data = career) + geom_bar(mapping = aes(x = career_length, fill = Sex))
ggplot(data = career) + geom_bar(mapping = aes(x = career_length, fill = continent))

#Number of olympics
name_sex <- merged_data %>% 
  distinct(Name, Sex, continent, country)

num_games <- merged_data %>%
  group_by(Name) %>% #for each athlete
  summarise(num_olympics = n_distinct(Year)) %>% 
  left_join(name_sex, by = "Name") #join with data frame containing name and gender of athlete

num_games <- num_games[which(!is.na(num_games[,"continent"])), ]
num_games <- num_games[which(!is.na(num_games[,"Sex"])), ]
ggplot(data = num_games) + geom_bar(mapping = aes(x = num_olympics, fill = continent))

ggplot(data = num_games) + geom_bar(mapping = aes(x = num_olympics, fill = Sex)) +
  labs(title = "Number of Olympics Participated In by Gender", x = "Number of Olympics", y = "Count of Athletes" )

ggplot(data = subset(num_games, country == c("Australia", "India", "Japan", "Germany", "Egypt", "Argentina", "United States"))) +
  geom_bar(mapping = aes(x = num_olympics, fill = country)) +
  labs(title = "Number of Olympics Participated In by Country", x = "Number of Olympics", y = "Count of Athletes" )

# Year by Average Age - compare countries, color by gender
#cont_gen <- merged_data %>% select(Sex, country, continent)
avg_age <- merged_data %>%
  group_by(country, Year) %>% #for each year and country
  summarise(mean_age = mean(Age, na.rm = TRUE)) %>% #find the mean age
  left_join(continent, by = "country") #join with continent %>%

avg_age <- avg_age[which(!is.na(avg_age[,"continent"])), ]

ggplot(data = avg_age) + 
  geom_smooth(mapping = aes(x = Year, y = mean_age), se = FALSE) +
  labs(title = "Average Age of Olympic Swimmers over Time", x = "Year", y = "Mean Age") +
  xlim(1896, 2025) + theme_minimal()

ggplot(data = subset(avg_age)) + 
  geom_smooth(mapping = aes(x = Year, y = mean_age, color = continent), se = FALSE) +
  labs(title = "Average Age of Olympic Swimmers over Time (Continent)", x = "Year", y = "Average Age") +
  xlim(1896, 2025) + theme_minimal()

ggplot(data = subset(avg_age, country == c("Australia", "India", "Japan", "Germany", "Egypt", "Argentina", "United States"))) + 
  geom_smooth(mapping = aes(x = Year, y = mean_age, color = country), se = FALSE) +
  labs(title = "Average Age over Time (Country)", x = "Year", y = "Average Age") +
  xlim(1896, 2025)

#Hypothesis Test

#avg_age <- avg_age %>% filter(continent %in% c("Asia", "Africa"))
y <- avg_age[, "mean_age", drop = TRUE]
x <- avg_age[, "Year", drop = TRUE]
cont <- factor(avg_age[, "continent", drop = TRUE],
levels = c("Asia", "Europe", "Africa", "North America", "South America", "Oceania"))
x2 <- x^2

new_df <- data.frame(x, x2, cont)
#new_df <- data.frame(x, cont)
#m <- lm
m <- lm(y ~ x + x2 + cont)
summary(m)
avg_age$age_pred <- predict(m, new_df)
#hist(resid(m))
#plot(x, avg_age$age_pred)
