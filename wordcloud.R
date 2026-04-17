library(tidyverse)
library(tidytext)
library(textstem)
library(ggrepel)
library(hunspell)
library(text2vec)
library(reticulate)

seed <- 3255L
set.seed(seed)

# Remove a few stop words that are actually informative
# and add a few new stop words
retain <- c(
  "man",
  "men",
  "group",
  "groups",
  "grouping",
  "grouped",
  "work",
  "works",
  "working",
  "worked",
  "area",
  "areas"
)
stop_words2 <-
  stop_words |>
  filter(!word %in% retain) |>
  add_row(word = c("pp", "set", "call"), lexicon = "Added") # add new stop words

# Get text version of MTH from Internet Archive
mth0 <- read_lines(
  "https://archive.org/stream/ManTheHunter/Man%20the%20Hunter_djvu.txt"
)

# regex to identify html entities
html_entity_regex <- "&([a-z0-9]+|#[0-9]+|#x[0-9a-fA-F]+);"

# Remove leading html tags, frontmatter, and references
# and perform some cleanup before tokenizing
mth <-
  mth0[3716:39949] |>
  str_replace_all(html_entity_regex, " ") |>
  str_replace_all("’", "'") |>
  str_replace_all("‘", "'") |>
  str_flatten(collapse = " ") |>
  str_squish() |>
  str_replace_all("- ", "-") |>

  # Some manual fixes
  str_replace_all("per cent", "percent") |>
  str_replace_all("wor ld", "world") |>
  str_replace_all("IKung", "!Kung") |>
  str_replace_all("Turn-bull", "Turnbull") |>
  str_replace_all("Wood-burn", "Woodburn") |>

  str_split_1(" ")

# Try to un-hyphenate words hyphenated at the
# end of a line but keeping hyphens in hyphenated compound words

# If the de-hyphenated version is a word, then de-hyphenate
dehyphenatedisword <- hunspell_check(str_remove(mth, "[:punct:]"))
mth[dehyphenatedisword] <- str_remove(mth[dehyphenatedisword], "-")

# Otherwise, if the hyphenated word isn't a word then de-hyphenate and hope for the best
isaword <- hunspell_check(str_remove_all(mth, "[^[:alnum:][:space:]-]"))
ishyphenated <- str_detect(mth, "-")
mth[!isaword & ishyphenated] <- str_remove(mth[!isaword & ishyphenated], "-")

# Cleaned up text
mth_clean <- str_c(mth, collapse = ' ')

# use reticulate to call python lemmatizing code
spacy <- reticulate::import("spacy")
spacy$util$fix_random_seed(seed)
nlp <- spacy$load("en_core_web_sm")
nlp$max_length <- str_count(mth_clean) + 10
doc <- nlp(mth_clean)
doc2 <- reticulate::iterate(doc)
# End python

mth_lemmas <-
  tibble(
    word = map_chr(doc2, \(token) token$text),
    lemma = map_chr(doc2, \(token) token$lemma_),
    pos = map_chr(doc2, \(token) token$pos_)
  ) |>
  mutate(
    lemma = str_to_lower(lemma)
  ) |>
  dplyr::filter(
    !pos == "PUNCT",
    !str_detect(lemma, "\\d+|[:punct:]|°|="),
    !lemma %in% stop_words2$word
  )

mth_lemmatized <- str_c(mth_lemmas$lemma, collapse = " ")

# Word embeddings (GlobalVectors)
tokens <- space_tokenizer(mth_lemmatized)
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens)
vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = 5L)
vectorizer <- vocab_vectorizer(vocab)
# use window of 5 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
glove = GlobalVectors$new(rank = 50, x_max = 10)
wv_main = glove$fit_transform(
  tcm,
  n_iter = 20,
  convergence_tol = 0.01,
  n_threads = 8
)
wv_context = glove$components
word_vectors = wv_main + t(wv_context)

# Count lemmas
mth_words <-
  mth_lemmas |>
  group_by(lemma) |>
  summarise(n = n()) |>
  arrange(desc(n))

mth200 <- mth_words[1:200, ]
wordvec200 <- word_vectors[mth200$lemma, ]

# Use reticulate to call python pacmap code
pacmap <- reticulate::import("pacmap")
reducer <- pacmap$PaCMAP(random_state = seed)
embedding <- reducer$fit_transform(wordvec200)
# End python

mth200$e1 <- embedding[, 1]
mth200$e2 <- embedding[, 2]

p_pacmap <-
  ggplot(mth200, aes(e1, e2, label = lemma, size = n)) +
  # geom_point(size = 0.5, alpha = 0.1) +
  geom_text_repel(
    seed = seed,
    max.overlaps = Inf,
    max.iter = 400000,
    max.time = 60,
    min.segment.length = 4,
    show.legend = F,
    verbose = T
  ) +
  scale_size_area(max_size = 12, transform = 'identity') +
  coord_fixed() +
  theme_void()
p_pacmap
ggsave("wordcloud.pdf", width = 10, height = 9)
