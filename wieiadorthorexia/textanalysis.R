library(rio)
library(here)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(textstem)
library(viridis)
library(SnowballC)
library(sentimentr)
library(textdata)

#import data
text <- import(here("data/wieiad_raw_data.csv"))

#frequency count
words <- text %>%
  unnest_tokens(word, `Open-ended Question`) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>%  
  count(word, sort = TRUE)
words

words <- words %>% 
  mutate(word = case_when(
    word %in% "unhealthi" ~ "unhealthy",
    word %in% "happi" ~ "happy",
    word %in% "posit" ~ "positive",
    word %in% "contribut" ~ "contribute",
    word %in% "tast" ~ "taste",
    word %in% "peopl" ~ "people",
    word %in% "satisfi" ~ "satisfy",
    word %in% "enjoi" ~ "enjoy",
    word %in% "dai" ~ "day",
    word %in% "moder" ~ "moderation",
    word %in% "balanc" ~ "balance",
    word %in% "provid" ~ "provide",
    word %in% "bodi" ~ "body",
    word %in% "energi" ~ "energy",
    word %in% "ic" ~ "ice",
    word %in% "sugari" ~ "sugary",
    word %in% "delici" ~ "delicious",
    word %in% "fri" ~ "fry",
    word %in% "joi" ~ "joy",
    word %in% "wellb" ~ "wellbeing",
    word %in% "dopamin" ~ "dopamine",
    word %in% "satisfac" ~ "satisfaction",
    word %in% "emot" ~ "emotion",
    word %in% "experi" ~ "experience",
    word %in% "famili" ~ "family",
    word %in% "indulg" ~ "indulge",
    word %in% "memori" ~ "memory",
    word %in% "sourc" ~ "source",
    word %in% "consid" ~ "consider",
    word %in% "cultur" ~ "culture",
    word %in% "fulfil" ~ "fulfill",
    word %in% "healthi" ~ "healthy",
    word %in% "improv" ~ "improve",
    word %in% "it'" ~ "it's",
    word %in% "one'" ~ "one's",
    word %in% "releas" ~ "release",
    word %in% "tasti" ~ "tasty",
    word %in% "ad" ~ "add",
    word %in% "addit" ~ "additional",
    word %in% "calori" ~ "calories",
    word %in% "commonli" ~ "commonly",
    word %in% "freeli" ~ "freely",
    word %in% "increas" ~ "increase",
    word %in% "noutrit" ~ "nutrition",
    word %in% "salti" ~ "salty",
    word %in% "sens" ~ "sense",
    word %in% "serv" ~ "serve",
    word %in% "you'r" ~ "you're",
    TRUE ~ word
  ))

words

#word cloud
png("www/images/wordcloud.png", width = 2000, height = 1600, res = 150)

wordcloud(words$word, words$n,
          scale = c(4, 0.5),
          max.words = 111,
          random.order = FALSE,
          rot.per = 0,
          colors = brewer.pal(12, "Paired"))

dev.off()

#frequency graph
top_words <- words %>% slice_max(n, n = 26) #mentioned 10 times or more
top_words

topwordsplot <- ggplot(top_words, aes(x = reorder(word, n), y = n, fill = n)) +
  geom_col() +
  geom_text(aes(label = n), hjust = 0) +
  scale_fill_viridis(option = "turbo") +
  coord_flip() +
  labs(
    title = "Most Used Words",
       subtitle = "(words used 10 times or more)",
       x = "Word",
       y = "Frequency"
    ) +
  theme_minimal() +
  theme(legend.position = "none")

topwordsplot

ggsave("www/images/topwords_plot.png", plot = topwordsplot)

# sentiment score
score <- sentiment(text$`Open-ended Question`) %>%
  filter(!is.na(word_count)) %>%
  group_by(element_id) %>%
  summarise(avg_sentiment = mean(sentiment))

score

score %>%
  summarise(overall_mean = mean(avg_sentiment, na.rm = TRUE)) # + positive / 0 neutral / - negative 

score %>%
  mutate(type = ifelse(avg_sentiment > 0, "Positive", "Negative")) %>%
  count(type)

# emotion categories 
# Saif M. Mohammad and Peter Turney. (2013), ``Crowdsourcing a Word-Emotion Association Lexicon.'' Computational Intelligence, 29(3): 436-465.
nrc <- get_sentiments("nrc")

emotion_category <- text %>%
     mutate(element_id = row_number()) %>%
     unnest_tokens(word, `Open-ended Question`) %>%
     inner_join(get_sentiments("nrc"), by = "word") %>%
     filter(!sentiment %in% c("positive", "negative")) %>%
     count(element_id, sentiment)

dominant_emotion <- emotion_category %>%
     group_by(element_id) %>%
     slice_max(n, n = 1, with_ties = TRUE)

dominant_emotion <- dominant_emotion %>%
  select(-n)

dominant_emotion_counts <- dominant_emotion %>%
  ungroup() %>%
  select(sentiment) %>%
  count(sentiment, name = "count")

dominantemotionplot <- ggplot(dominant_emotion_counts, aes(x = reorder(sentiment, count), y = count, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = count), hjust = -0.1) +
  scale_fill_viridis_d(option = "turbo") +
  coord_flip() +
  labs(
    title = "Dominant Emotion of Response",
    x = "Emotion",
    y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

dominantemotionplot

ggsave("www/images/dominantemotion_plot.png", plot = dominantemotionplot)
