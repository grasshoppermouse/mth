# Try to un-hyphenate words hyphenated at the
# end of a line but keeping hyphens in hyphenated compound words
dehyphenate <- function(chr_vec) {
  # If the de-hyphenated version is a word, then de-hyphenate
  dehyphenatedisword <- hunspell_check(str_remove_all(chr_vec, "[:punct:]"))
  chr_vec[dehyphenatedisword] <- str_remove_all(
    chr_vec[dehyphenatedisword],
    "-"
  )

  # Otherwise, if the hyphenated word isn't a word then de-hyphenate and hope for the best
  isaword <- hunspell_check(str_remove_all(chr_vec, "[^[:alnum:]-]"))
  ishyphenated <- str_detect(chr_vec, "-")
  chr_vec[!isaword & ishyphenated] <- str_remove(
    chr_vec[!isaword & ishyphenated],
    "-"
  )
  return(chr_vec)
}

# Split paragraphs
# Very preliminary code
split_paragraphs <- function(raw_text) {
  # Collapse the entire text into a single string to clean up line wraps
  combined_text <- str_flatten(raw_text, collapse = "\n")

  # Clean up formatting artifacts:
  # Replace single line breaks that happen mid-sentence with spaces,
  # but preserve double (or more) line breaks as paragraph separators.
  # Alternatively, split directly on 2 or more newlines:
  paragraphs <- unlist(str_split(combined_text, "\n\\s*\n"))

  # 4. Post-processing: Trim whitespace and remove empty strings or page headers
  paragraphs <- str_squish(paragraphs)
  paragraphs <- paragraphs[paragraphs != ""]
  return(paragraphs)
}

# Identify run-ons
split_runon_word <- function(word) {
  if (hunspell_check(word)) {
    return(word)
  }

  word <- tolower(trimws(word))
  n <- nchar(word)

  # A word must be at least 2 characters to split into two parts
  if (n < 2) {
    return(word)
  }

  # Generate all possible split positions
  splits <- seq_len(n - 1)

  # Create pairs of prefixes and suffixes
  prefixes <- substring(word, 1, splits)
  suffixes <- substring(word, splits + 1, n)

  # Check if both parts are valid English words
  # (hunspell_check returns TRUE for valid dictionary words)
  valid_prefix <- hunspell_check(prefixes)
  valid_suffix <- hunspell_check(suffixes)

  # Find indices where BOTH parts are valid words
  valid_indices <- which(valid_prefix & valid_suffix)

  if (length(valid_indices) > 0) {
    # Combine the valid parts with a space
    # (Returns all valid combinations if there are multiple matches)
    return(paste(prefixes[valid_indices], suffixes[valid_indices], sep = " "))
  } else {
    # Return original if no valid split is found
    return(word)
  }
}

# Function to clean lemmas
# Manual fixes based on discrepancies among lemmatizers
clean_lemmas <- function(lemmas) {
  lemmas |>
    mutate(
      lemma = str_to_lower(lemma),
      lemma = case_when(
        str_to_lower(word) == "gathering" ~ "gather",
        lemma == "wive" ~ "wife",
        lemma == "co-wive" ~ "wife",
        lemma == "hunting" ~ "hunt",
        lemma == "hunters" ~ "hunter",
        lemma == "gathering" ~ "gather",
        lemma == "gatherers" ~ "gatherer",
        lemma == "gathere" ~ "gatherer",
        lemma == "hunter-gatherers" ~ "hunter-gatherer",
        lemma == "hunters-gatherer" ~ "hunter-gatherer",
        lemma == "hunter-gather" ~ "hunter-gatherer",
        lemma == "hunting-gathering" ~ "hunter-gatherer",
        lemma == "hunting-gather" ~ "hunter-gatherer",
        lemma == "net-hunter" ~ "hunter",
        lemma == "net-hunt" ~ "hunt",
        lemma == "societies" ~ "society",
        lemma == "resources" ~ "resource",
        lemma == "groups" ~ "group",
        lemma == "datum" ~ "data",
        lemma == "models" ~ "model",
        lemma == "!kung" ~ "kung",
        lemma == "eskimos" ~ "eskimo",
        lemma == "specie" ~ "species",
        lemma == "bushmen" ~ "bushman",
        lemma == "indians" ~ "indian",
        lemma == "neighbors" ~ "neighbor",
        lemma == "neighboring" ~ "neighbor",
        lemma == "ty" ~ "tie", # udpipe: ties -> ty
        lemma == "eating" ~ "eat",
        lemma == "numb" ~ "number",
        lemma == "fishing" ~ "fish", # match Sisk
        .default = lemma
      ),
    ) |>
    dplyr::filter(
      pos != "PUNCT",
      pos != "SYM",
      pos != "NUM",
      lemma != "-", # spaCy categorizes this as a word & lemma
      !str_detect(lemma, "@card@"),
      !str_detect(lemma, "\\d+|°|=|'|\\.")
    )
}

# Function to plot word vectors
plot_vectors <- function(d, fn, ggrepel = T) {
  p <-
    ggplot(d, aes(e1, e2, label = lemma, size = n)) +
    geom_point(alpha = 0.1) +
    scale_size_area(max_size = 10, transform = 'identity') +
    coord_fixed() +
    theme_void()

  if (ggrepel) {
    p <-
      p +
      geom_text_repel(
        seed = seed,
        max.overlaps = Inf,
        max.iter = 400000,
        max.time = 60,
        min.segment.length = 0.5,
        show.legend = F,
        verbose = T
      )
  } else {
    p <-
      p +
      geom_text()
  }
  ggsave(fn, p, width = 10, height = 10)
}

# Text processing pipeline from Sisk
# https://github.com/msisk1/MtH_NLP/blob/main/MtH_2026.qmd
sisk_lemmas <- function(mth_unmodified) {
  # Line number limits for the main text in the file
  preface_starts <- 75 # start of the prefac (not used)
  part1_starts <- 746 # start of main text (after front matter)
  references_starts <- 36991 # end of main text (before references)

  raw <- mth_unmodified[part1_starts:references_starts]

  modified_text <- paste(raw, collapse = "\n") %>%
    gsub("['\u2018\u2019\u201A\u201B\u0060\u00B4]", "'", .) %>% # standardize apostrophes
    gsub("\r\n", "\n", .) %>% # fix line endings
    gsub("([a-z])-\\s*\n\\s*([a-z])", "\\1\\2", .) %>% # rejoin hyphenated linebreaks
    gsub("(?m)^\\s*\\d+\\s*$", "", ., perl = TRUE) %>% # remove page numbers
    gsub("huntergatherers?\\b", "hunter gatherer", .) %>% # split inaccurate compound
    gsub("huntinggathering", "hunting gathering", .) # split inaccurate compound

  bag_of_words <- tibble(text = modified_text) %>%
    unnest_tokens(word_orig, text) %>%
    mutate(word_orig = gsub("'s$", "", word_orig)) %>% # strip possessives
    filter(nchar(word_orig) > 2) %>% # minimum 3 characters
    mutate(word = lemmatize_words(word_orig)) %>%
    filter(!str_detect(word, "[0-9]")) # remove numeric tokens after lemmatizing to avoid e.g."tenth" becoming "10"

  return(bag_of_words)
}
