library(dplyr)
library(tidyr)
?#How many rows and columns are there in the edx dataset?
dim(edx)

#How many zeros were given as ratings in the edx dataset?
edx %>% filter(rating == 0) %>% tally()

#How many threes were given as ratings in the edx dataset?
edx %>% filter(rating == 3) %>% tally()


#How many different movies are in the edx dataset?
n_distinct(edx$movieId)

#How many different users are in the edx dataset?
n_distinct(edx$userId)

#How many movie ratings are in each of the following genres in the edx dataset?
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


#Which movie has the greatest number of ratings?
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#What are the five most given ratings in order from most to least?
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))

#True or False: In general, half star ratings are less common than whole star ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).
edx %>% group_by(rating) %>% summarize(count = n())

#Visually, this can be seen using the following code:
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count, color = rating)) +
  geom_line()

