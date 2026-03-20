# ============================
# R-Skript: Horror-Filme LDA pro Dekade
# ============================

# ---- Bibliotheken ----
library(dplyr)
library(stringr)
library(tm)
library(topicmodels)
library(readr)
library(ggplot2)
library(tidyr)
library(tidytext)
library(reshape2)

# ---- Pfade ----
ordner <- "~/archive/screenplay_data/data/raw_text_lemmas/raw_text_lemmas"
character_base_dir <- "~/archive/movie_characters/data/movie_character_texts/movie_character_texts"
meta_path <- "~/archive/movie_metadata/movie_meta_data.csv"

dateien <- list.files(ordner, pattern = "_lemmas\\.txt$", full.names = TRUE)
character_dirs <- list.dirs(character_base_dir, recursive = FALSE, full.names = TRUE)

# ---- Hilfsfunktion: Fallback-Heuristik für mögliche Charakternamen ----
extract_name_candidates_from_text <- function(text) {
  
  words <- unlist(str_split(text, "\\s+"))
  words <- words[nchar(words) > 2]
  
  freq <- sort(table(words), decreasing = TRUE)
  top_words <- names(freq)[1:min(60, length(freq))]
  
  bad_words <- c(
    stopwords("english"),
    "int","ext","continued","cont",
    "shot","camera","close","cut","fade","beat","pause",
    "look","see","get","come","go","take","make","know","turn","back",
    "will","one","can","just","like","now","then","still","right","left",
    "door","room","house","night","day","hand","face","eye","head","body",
    "man","woman","child","voice","sound","wall","floor","window","hallway",
    "open","close","start","stop","move","pull","hold","run","walk","sit",
    "stand","say","tell","hear","watch","smile","stare","around","away",
    "something","thing","time","yes","well","okay","gonna","fuck"
  )
  
  candidates <- setdiff(top_words, bad_words)
  candidates <- candidates[nchar(candidates) > 3]
  
  unique(candidates)
}

# ---- Hilfsfunktion: Charakternamen aus Dateinamen extrahieren ----
extract_character_names <- function(character_files) {
  
  raw_names <- basename(character_files)
  raw_names <- str_remove(raw_names, "_text\\.txt$")
  raw_names <- tolower(raw_names)
  
  full_names <- str_replace_all(raw_names, "_", " ")
  full_names <- str_squish(full_names)
  
  split_parts <- str_split(full_names, "\\s+")
  parts <- unlist(split_parts)
  
  parts <- parts[nchar(parts) > 2]
  parts <- parts[!parts %in% c(
    "the", "and", "mrs", "miss", "mr", "ms", "dr",
    "sir", "jr", "sr", "ii", "iii"
  )]
  
  all_names <- unique(c(full_names, parts))
  all_names <- all_names[nchar(all_names) > 2]
  
  unique(all_names)
}

# ---- Hilfsfunktion: Charakternamen aus Text entfernen ----
remove_character_names <- function(text, names_vec) {
  
  if(length(names_vec) == 0) return(text)
  
  names_vec <- unique(names_vec)
  names_vec <- names_vec[order(nchar(names_vec), decreasing = TRUE)]
  
  cleaned_text <- text
  
  for(nm in names_vec) {
    nm_escaped <- str_replace_all(nm, "([.|()\\^{}+$*?]|\\[|\\]|\\\\)", "\\\\\\1")
    pattern <- paste0("\\b", str_replace_all(nm_escaped, "\\s+", "\\\\s+"), "\\b")
    cleaned_text <- str_replace_all(cleaned_text, regex(pattern, ignore_case = TRUE), " ")
  }
  
  str_squish(cleaned_text)
}

# ---- Hilfsfunktion: allgemeine Textbereinigung ----
clean_raw_text <- function(text) {
  
  text <- tolower(text)
  
  text <- str_replace_all(text, "http[s]?://\\S+", " ")
  text <- str_replace_all(text, "www\\.\\S+", " ")
  text <- str_replace_all(text, "http\\S+", " ")
  text <- str_replace_all(text, "httpwww\\S+", " ")
  text <- str_replace_all(text, "â\\S*", " ")
  text <- str_replace_all(text, "[^a-z0-9\\s]", " ")
  text <- str_squish(text)
  
  text
}

# ---- 1. Ganze Texte einlesen ----
df_movies <- data.frame()

for(file in dateien) {
  
  text <- read_file(file)
  text <- clean_raw_text(text)
  
  temp_df <- data.frame(
    movie_id = basename(file),
    text = text,
    stringsAsFactors = FALSE
  )
  
  df_movies <- bind_rows(df_movies, temp_df)
}

# ---- 2. IMDb-ID extrahieren ----
df_movies <- df_movies %>%
  mutate(
    imdb_id = str_extract(movie_id, "(?<=_)(\\d+)(?=_lemmas\\.txt)"),
    imdb_id_num = as.numeric(imdb_id)
  )

# ---- 3. Meta-CSV einlesen und join ----
movie_meta_data <- read_csv(meta_path)

df_full <- df_movies %>%
  left_join(movie_meta_data, by = c("imdb_id_num" = "imdbid"))

# ---- 4. Horror-Filme filtern ----
horror_movies <- df_full %>%
  filter(!is.na(genres)) %>%
  filter(str_detect(genres, regex("Horror", ignore_case = TRUE)))

# ---- 5. Horror-Filme bearbeiten ----
horror_scenes <- data.frame()
horror_without_markers <- data.frame()
horror_without_character_dir <- data.frame()

for(i in seq_len(nrow(horror_movies))) {
  
  file_name <- horror_movies$movie_id[i]
  text <- horror_movies$text[i]
  imdb_id <- horror_movies$imdb_id[i]
  
  cat("Verarbeite:", file_name, "\n")
  
  matching_dir <- character_dirs[str_detect(basename(character_dirs), paste0("_", imdb_id, "$"))]
  
  if(length(matching_dir) == 1) {
    
    char_files <- list.files(matching_dir, pattern = "_text\\.txt$", full.names = TRUE)
    
    if(length(char_files) > 0) {
      char_names <- extract_character_names(char_files)
      text <- remove_character_names(text, char_names)
      
    } else {
      cat("WARNUNG: Leerer Charakterordner -> Fallback:", file_name, "\n")
      
      fallback_names <- extract_name_candidates_from_text(text)
      cat("Fallback Namen für", file_name, ":\n")
      cat(paste(head(fallback_names, 15), collapse = ", "), "\n\n")
      
      text <- remove_character_names(text, fallback_names)
      
      horror_without_character_dir <- bind_rows(
        horror_without_character_dir,
        horror_movies[i, ]
      )
    }
    
  } else {
    cat("WARNUNG: Kein Charakterordner -> Fallback:", file_name, "\n")
    
    fallback_names <- extract_name_candidates_from_text(text)
    cat("Fallback Namen für", file_name, ":\n")
    cat(paste(head(fallback_names, 15), collapse = ", "), "\n\n")
    
    text <- remove_character_names(text, fallback_names)
    
    horror_without_character_dir <- bind_rows(
      horror_without_character_dir,
      horror_movies[i, ]
    )
  }
  
  text <- clean_raw_text(text)
  
  splits <- str_locate_all(text, "\\b(int|ext)\\b")[[1]]
  
  if(nrow(splits) == 0){
    cat("WARNUNG: Keine Szenenmarker (INT/EXT) gefunden in Horrorfilm:", file_name, "\n")
    
    horror_without_markers <- bind_rows(
      horror_without_markers,
      horror_movies[i, ]
    )
    
    next
  }
  
  starts <- splits[,1]
  scenes <- c()
  
  for(j in seq_along(starts)) {
    start_pos <- starts[j]
    end_pos <- ifelse(j < length(starts), starts[j + 1] - 1, nchar(text))
    scene_text <- str_sub(text, start_pos, end_pos)
    scene_text <- str_squish(scene_text)
    
    if(str_count(scene_text, "\\S+") > 50) {
      scenes <- c(scenes, scene_text)
    }
  }
  
  if(length(scenes) == 0) {
    cat("WARNUNG: Alle Szenen zu kurz in Horrorfilm:", file_name, "\n")
    next
  }
  
  temp_df <- data.frame(
    doc_id = paste0(file_name, "_scene_", seq_along(scenes)),
    movie_id = file_name,
    text = scenes,
    imdb_id = horror_movies$imdb_id[i],
    imdb_id_num = horror_movies$imdb_id_num[i],
    year = horror_movies$year[i],
    genres = horror_movies$genres[i],
    stringsAsFactors = FALSE
  )
  
  horror_scenes <- bind_rows(horror_scenes, temp_df)
}

# ---- Optional: Listen ausgeben ----
cat("\n=== Horrorfilme ohne INT/EXT-Marker ===\n")
print(horror_without_markers$movie_id)

cat("\n=== Horrorfilme ohne eindeutigen Charakterordner ===\n")
print(horror_without_character_dir$movie_id)

# ---- 6. Nur ausreichend lange Szenen behalten ----
horror_df <- horror_scenes %>%
  filter(str_count(text, "\\S+") > 50) %>%
  filter(!is.na(year))

# ---- 7. Dekadenspalte hinzufügen ----
horror_df <- horror_df %>%  
  mutate(
    decade_start = floor(year / 10) * 10,
    decade = paste0(decade_start, "-", decade_start + 9)
  )

# ---- 8. Grafik: Anzahl der Szenen pro Dekade ----
scene_count_by_decade <- horror_df %>%
  count(decade_start, decade, name = "n_scenes") %>%
  arrange(decade_start)

plot_scenes_per_decade <- ggplot(scene_count_by_decade,
                                 aes(x = factor(decade, levels = decade),
                                     y = n_scenes)) +
  geom_col() +
  labs(
    title = "Anzahl der Horrorszenen pro Dekade",
    x = "Dekade",
    y = "Anzahl der Szenen"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_scenes_per_decade)

# Optional speichern
ggsave("scenes_per_decade.png", plot_scenes_per_decade, width = 10, height = 6)

# ---- 9. Split nach Dekade ----
horror_by_decade <- horror_df %>%
  group_by(decade_start) %>%
  group_split()

names(horror_by_decade) <- horror_df %>%
  distinct(decade_start, decade) %>%
  arrange(decade_start) %>%
  pull(decade)

# ---- 10. LDA pro Dekade ----
k <- 5
lda_results <- list()

custom_stopwords <- c(
  stopwords("english"),
  "int", "ext", "continued", "cont", "continue",
  "shot", "camera", "close", "cut", "fade", "pan", "beat", "pause",
  "look", "see", "get", "come", "go", "take", "make", "know", "turn", "back",
  "will", "one", "can", "just", "like", "now", "then", "still", "right", "left",
  "say", "tell", "said", "hear", "watch", "stare", "smile",
  "open", "start", "stop", "move", "pull", "hold", "run", "walk", "sit", "stand",
  "yes", "well", "okay", "way", "something", "thing", "time", "two",
  "man", "woman", "mrs", "miss", "mr", "ms",
  "hand", "face", "eye", "head", "body", "arm",
  "door", "room", "house", "window", "hallway", "wall", "floor",
  "night", "day", "light", "sound", "voice",
  "fuck", "gonna"
)

top_terms_all <- data.frame()
topic_means_all <- data.frame()

for(dekade_name in names(horror_by_decade)) {
  
  cat("=== LDA für Dekade:", dekade_name, "===\n")
  
  df_dekade <- horror_by_decade[[dekade_name]]
  
  if(nrow(df_dekade) == 0) {
    cat("Keine Horrorfilme in dieser Dekade.\n")
    lda_results[[dekade_name]] <- NULL
    next
  }
  
  corpus <- VCorpus(VectorSource(df_dekade$text))
  
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("http\\S+", " ", x)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^a-z\\s]", " ", x)))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, custom_stopwords)
  corpus <- tm_map(corpus, stripWhitespace)
  
  dtm <- DocumentTermMatrix(corpus)
  
  dtm_matrix <- as.matrix(dtm)
  if(nrow(dtm_matrix) == 0 || ncol(dtm_matrix) == 0) {
    cat("Leere DTM.\n")
    lda_results[[dekade_name]] <- NULL
    next
  }
  
  keep_terms <- nchar(colnames(dtm)) > 3
  dtm <- dtm[, keep_terms]
  
  dtm <- removeSparseTerms(dtm, 0.99)
  dtm <- dtm[rowSums(as.matrix(dtm)) > 5, ]
  
  if(nrow(dtm) < k || ncol(dtm) == 0) {
    cat("Zu wenige Dokumente oder Terme für LDA.\n")
    lda_results[[dekade_name]] <- NULL
    next
  }
  
  lda_model <- LDA(dtm, k = k, control = list(seed = 1234))
  
  top_words <- terms(lda_model, 10)
  print(top_words)
  
  topic_dist <- posterior(lda_model)$topics
  
  lda_results[[dekade_name]] <- list(
    model = lda_model,
    top_words = top_words,
    topic_distribution = topic_dist
  )
  
  # ---- Top Terms sammeln für Grafik ----
  beta_df <- tidy(lda_model, matrix = "beta")
  
  top_terms <- beta_df %>%
    group_by(topic) %>%
    slice_max(beta, n = 10, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(decade = dekade_name)
  
  top_terms_all <- bind_rows(top_terms_all, top_terms)
  
  # ---- Durchschnittliche Topic-Verteilung sammeln ----
  gamma_df <- tidy(lda_model, matrix = "gamma") %>%
    group_by(topic) %>%
    summarise(mean_gamma = mean(gamma), .groups = "drop") %>%
    mutate(decade = dekade_name)
  
  topic_means_all <- bind_rows(topic_means_all, gamma_df)
}

# ---- 11. Grafik: Top-Wörter je Topic und Dekade ----
# Wird pro Dekade einzeln geplottet
for(dekade_name in unique(top_terms_all$decade)) {
  
  plot_data <- top_terms_all %>%
    filter(decade == dekade_name) %>%
    group_by(topic) %>%
    slice_max(beta, n = 8, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(term = reorder_within(term, beta, topic))
  
  p <- ggplot(plot_data, aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    scale_x_reordered() +
    coord_flip() +
    labs(
      title = paste("Top-Wörter der Topics in der Dekade", dekade_name),
      x = "Wort",
      y = "Beta"
    ) +
    theme_minimal()
  
  print(p)
  
  ggsave(
    filename = paste0("lda_top_terms_", gsub("[^0-9A-Za-z]", "_", dekade_name), ".png"),
    plot = p,
    width = 12,
    height = 8
  )
}

# ---- 12. Grafik: Durchschnittliche Topic-Verteilung pro Dekade ----
topic_means_all <- topic_means_all %>%
  mutate(topic = factor(topic),
         decade = factor(decade, levels = unique(scene_count_by_decade$decade)))

plot_topic_heatmap <- ggplot(topic_means_all,
                             aes(x = topic, y = decade, fill = mean_gamma)) +
  geom_tile() +
  labs(
    title = "Durchschnittliche Topic-Verteilung pro Dekade",
    x = "Topic",
    y = "Dekade",
    fill = "Mittlere\nGamma"
  ) +
  theme_minimal()

print(plot_topic_heatmap)

ggsave("topic_heatmap_by_decade.png", plot_topic_heatmap, width = 8, height = 6)