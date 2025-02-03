colSums(is.na(netflix_titles))

# There are a lot of NA's in the director, cast, and country columns.

summary(netflix_titles)

# all the data is categorized as character data. To run a regression we want numerical data. 

as.numeric(as.character(netflix_titles$type))

install.packages("fastDummies")
library(fastDummies)

netflix_clean <- na.omit(netflix_titles)

numeric_data1 <- ifelse(netflix_clean$type == "TV Show", 1, 0)


reg1 <- lm(numeric_data1 ~ netflix_clean$release_year)

summary(reg1)


