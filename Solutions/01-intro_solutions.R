library(tidyverse)

raw <- readRDS("data/TWLZ_Seminar_Sample.rds")


# 1. Erstelle eine neue Variable, die angibt, ob ein Tweet ein Keyword deiner Wahl enthält.
dat <- raw %>%
  mutate(has_moodle = str_detect(text, "moodle"))

dat %>%
  filter(has_moodle) %>%
  select(text, has_moodle) %>%
  View()

dat %>%
  count(has_moodle)

### Was könnte dabei noch zu beachten sein?

# Problem:
# - groß und kleinschreibung nicht beachtet!
# - was ist mit Wortgrenzen

# -> Regex!

dat <- raw %>%
  mutate(
    text_clean = text %>% tolower()
  )

dat <- dat %>%
  mutate(
    has_moodle = str_detect(
      text_clean,
      regex("\\bmoodle\\b", ignore_case = TRUE)
    )
  )


## 2. An welchen drei Tagen wurden die meisten Tweets geposted?

raw %>%
  count(date_created, sort = TRUE) %>%
  head(3)


# 3. Wie viele bots gibt es in dem Datensatz?

raw %>% count(is_bot)

# 4. Gibt es einen Zusammenhang zwischen der Anzahl an verwendeten Hashtags und Likes eines Tweets?

raw %>%
  summarise(
    like_hashtags_p = cor(n_hashtags, like_count)
  )

cor(raw$n_hashtags, raw$like_count)

(test <- cor.test(raw$n_hashtags, raw$like_count))

test$p.value < 0.05

# 5. Wie viele Leute wurden durchschnittlich pro Tweet getagged (mentioned)?

raw %>%
  summarise(
    M_mentions = mean(n_mentions)
  )

mean(raw$n_mentions)

