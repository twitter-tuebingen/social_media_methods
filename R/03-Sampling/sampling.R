library(tidyverse)

raw <- readRDS("data/TWLZ_Seminar_Sample.rds")

keywords <- read_lines("R/03-Sampling/keywords.txt")
keywords

# Make a Regex ------------------------------------------------------------

keywords <- keywords %>%
  tolower() %>%
  str_replace_all("ä", "ae") %>%
  str_replace_all("ü", "ue") %>%
  str_replace_all("ö", "oe") %>%
  str_replace_all("ß", "ss")

keywords_regex <- keywords %>% paste0(collapse = "|") # "|" heißt oder


# Find Keywords -----------------------------------------------------------

# Clean text
dat <- raw %>%
  mutate(
    text_clean = text %>% tolower() %>%
      str_replace_all("ä", "ae") %>%
      str_replace_all("ü", "ue") %>%
      str_replace_all("ö", "oe") %>%
      str_replace_all("ß", "ss") %>%
      str_squish()
    # more cleaning steps, z.B. remove punctuation etc.
  )

# Text kontrollieren:
dat %>%
  select(text, text_clean) %>%
  View()

keyword_sample <- dat %>%
  filter(str_detect(text_clean, keywords_regex))

# NOTE: retweets (is_retweet) beachten!

# Find hashtags -----------------------------------------------------------

hashtags <- read_lines("R/03-Sampling/hashtags.txt")
hashtags

hashtags_regex <- hashtags %>% paste0(collapse = "|") # "|" heißt oder

hashtag_sample <- dat %>%
  filter(str_detect(sample_hashtags_str, hashtags_regex))

# sample_hashtags_str = basierend auf all_hashtags, aber nur hashtags die komplett heruntergeladen worden sind, als string
# all_hashtags = alle hashtags die in dem Tweet enthalten sind + sparking hashtags (bei Replies)
# hashtags = alle hashtags die in dem Tweet Text enthalten sind


# Combine Samples -----------------------------------------------------------------

all <- bind_rows(keyword_sample, hashtag_sample) %>%
  distinct(status_id, .keep_all = TRUE) # Duplikate entfernen, alle Vars behalten
