# Musical data extraction & analysis
# IV Internacional Seminar on Statistics with R
# May, 2019

# Loading necessary packages -------------------------------------------
library(vagalumeR)
library(Rspotify)
library(chorrrds)
library(tidyverse)

# Authenticating vagalume and Spotify --------------------------
key_vagalume <- "vagalume_key"

id <- "spotify_id"
secret <- "spotify_secret"
app <- "my_app"
key_spotify <- spotifyOAuth(app, id, secret)


# Lyrics data  ---------------------------------------------------------
# Setting the artist to use
artist <- "chico-buarque"

# song names retrieval
songs <- artist %>% 
  purrr::map_dfr(songNames)

songs %>% 
  group_by(name) %>% 
  count()

# mapping the lyrics function over the ids found
lyrics <-  songs %>% 
  pull(song.id) %>% 
  purrr::map(lyrics, 
             artist = artist,
             type = "id", 
             key = key_vagalume) %>%
  purrr::map_df(data.frame) %>% 
  dplyr::select(-song) %>% 
  dplyr::right_join(songs %>% select(song, song.id), by = "song.id")


# Spotify data  --------------------------------------------------------
find_artist <- searchArtist("chico buarque", token = key_spotify)

# using the found id to recover album information
albums <- getAlbums(find_artist$id[1], token = key_spotify)

# obtaining the songs in each album
albums_res <- albums %>% 
  dplyr::pull(id) %>% 
  purrr::map_df(
    ~{
      getAlbum(.x, token = key_spotify) %>% 
        dplyr::select(id, name) 
    }) %>% 
  tidyr::unnest()

ids <- albums_res %>% 
  dplyr::pull(id)

# obtaining the features of the song in each album
features <- ids %>% 
  purrr::map_df(
    ~{
      getFeatures(.x, token = key_spotify) 
    }) %>% 
  dplyr::left_join(albums_res, by = "id")

# how to easily obtain the popularity of each song?
# create a function
getPop <- function(id, token){
  req <- httr::GET(paste0("https://api.spotify.com/v1/tracks/", id), 
                   httr::config(token = token))
  json1 <- httr::content(req)
  res <- data.frame(song = json1$name, 
                    popul = json1$popularity, 
                    id = json1$id)
  return(res)
}

# obtaining the popularities for all songs
popul <-  features %>% 
  pull(id) %>% 
  purrr::map_df(
    ~{
      getPop(.x, token = key_spotify) 
    }) 

# joining the popularity with other features
features <- features %>% 
  right_join(
    popul %>% select(-song), 
    by = c("id" = "id"))

# Chords data  ---------------------------------------------------------
songs <- "chico-buarque" %>% 
  chorrrds::get_songs() 

chords <- songs %>% 
  dplyr::pull(url) %>%                     
  purrr::map(chorrrds::get_chords) %>%    
  purrr::map_dfr(dplyr::mutate_if, is.factor, as.character)   %>% 
  chorrrds::clean(message = FALSE) 

#-----------------------------------------------------------------------
# Combining different datasets  
#-----------------------------------------------------------------------

chords <- chords %>% 
  dplyr::mutate(song = stringr::str_remove(music, "chico buarque ")) %>% 
  dplyr::select(-music)

lyrics <- lyrics %>% 
  dplyr::mutate(song = stringr::str_to_lower(song))

features <- features %>% 
  dplyr::mutate(song = stringr::str_to_lower(name)) %>% 
  dplyr::select(-name)

all_data <- chords %>% 
  dplyr::inner_join(lyrics, by = "song") %>% 
  dplyr::inner_join(features, by = "song")

# What if there ate too many unmatches between the 'song' column?
nrow(chords) - nrow(all_data) 

# Fixing some of the unmatches with string distance
library(RecordLinkage)

anti_chords_lyrics <- chords %>% 
  dplyr::anti_join(lyrics, by = "song") 

# Saving names that didn't match
names_to_fix <- anti_chords_lyrics %>% 
  dplyr::distinct(song) %>% 
  dplyr::pull(song)

# Finding the distances between each song in the lyrics data and 
# the ones that didn't match
dists <- lyrics$song %>% 
  purrr::map(levenshteinSim, str1 = names_to_fix)

# Finding the biggest similarities between the songs 
# in the lyrics data and the ones that didn't match
ordered_dists <- dists %>% purrr::map_dbl(max)
max_dists <- dists %>% purrr::map_dbl(which.max)

# Filtering to have only similarities > 0.70
indexes_min_dist <- which(ordered_dists > 0.70)
songs_min_dist <- lyrics$song[indexes_min_dist]
index_lyrics <- max_dists[which(ordered_dists > 0.70)]

# Saving the most similar string in the chords and 
# lyrics data 
results_dist_lyrics <- data.frame(
  from_chords = names_to_fix[index_lyrics],
  from_lyrics = songs_min_dist)

cat(
  paste0("song == ", "'", results_dist_lyrics$from_chords, "' ~ '", 
         results_dist_lyrics$from_lyrics, "', "), collapse = "")

# Changing the songs in the chords dataset so there will be 
# more matches
chords <- chords %>% 
  dplyr::mutate(
    song = 
      dplyr::case_when( 
        song == 'a bela a fera' ~ 'a bela e a fera',  
        song == 'a historia de lily braun' ~ 'a história de lily braun',  
        song == 'a moca do sonho' ~ 'a moça do sonho',  
        song == 'a ostra o vento' ~ 'a ostra e o vento',  
        song == 'a televisao' ~ 'a televisão',  
        song == 'a valsa dos clows' ~ 'a valsa dos clowns',  
        song == 'a voz do dono o dono da voz' ~ 'a voz do dono e o dono da voz',  
        song == 'agora falando serio' ~ 'agora falando sério',  
        song == 'ai se eles me pegam agora' ~ 'ai, se eles me pegam agora',  
        song == 'amanha ninguem sabe' ~ 'amanhã, ninguém sabe',  
        song == 'angelica' ~ 'angélica',  
        song == 'apesar de voce' ~ 'apesar de você',  
        song == 'ate pensei' ~ 'até pensei',  
        song == 'ate segunda feira' ~ 'até segunda-feira',  
        song == 'atras da porta' ~ 'atrás da porta',  
        song == 'barbara' ~ 'bárbara',  
        song == 'boi voador nao pode' ~ 'boi voador não pode',  
        song == 'bye bye brasil' ~ 'bye, bye, brasil',  
        song == 'cala boca barbara' ~ 'cala a boca, bárbara',  
        song == 'cancao de pedroca' ~ 'canção de pedroca',  
        song == 'cantando no toro' ~ 'cantando no toró',  
        song == 'carcara' ~ 'carcará',  
        song == 'cecilia' ~ 'cecília',  
        song == 'chao de esmeraldas' ~ 'chão de esmeraldas',  
        song == 'com acucar com afeto' ~ 'com açúcar, com afeto',  
        song == 'como se fosse primavera' ~ 'como se fosse a primavera',  
        song == 'cuidado com outra' ~ 'cuidado com a outra',  
        song == 'deixe menina' ~ 'deixa a menina',  
        song == 'dis moi comment' ~ 'dis-moi comment',  
        song == 'ela sua janela' ~ 'ela e sua janela',  
        song == 'essa moca ta diferente' ~ 'essa moça tá diferente',  
        song == 'ja passou' ~ 'essa passou',  
        song == 'estacao derradeira' ~ 'estação derradeira',  
        song == 'geni o zepelim' ~ 'geni e o zepelim', 
        song == 'gota dagua' ~ "gota d'água",  
        song == 'historia de uma gata' ~ 'história de uma gata',  
        song == 'ilmo sr ciro monteiro' ~ 'ilmo. sr. ciro monteiro',  
        song == 'introducao para a turma do funil' ~ 'introdução para a turma do funil', 
        song == 'yolanda' ~ 'iolanda',  
        song == 'ja passou' ~ 'já passou',  
        song == 'januaria' ~ 'januária',  
        song == 'logo eu' ~ 'logo eu?',  
        song == 'mano mano' ~ 'mano a mano',  
        song == 'mar lua' ~ 'mar e lua',  
        song == 'massarandupio' ~ 'massarandupió',  
        song == 'meninos eu vi' ~ 'meninos, eu vi',  
        song == 'meu caro barao' ~ 'meu caro amigo',  
        song == 'meu caro barao' ~ 'meu caro barão',  
        song == 'meu refrao' ~ 'meu refrão',  
        song == 'mil perdoes' ~ 'mil perdões',  
        song == 'minha historia' ~ 'minha história',  
        song == 'morena dos olhos dagua' ~ "morena dos olhos d'água",  
        song == 'morena dos olhos dagua' ~ 'morena dos olhos dágua',  
        song == 'moto continuo' ~ 'moto-contínuo',  
        song == 'mulher vou dizer quanto eu te amo' ~ 'mulher, vou dizer quanto te amo', 
        song == 'na ilha de lia no barco de rosa' ~ 'na ilha de lia, no barco de rosa', 
        song == 'nao existe pecado ao sul do equador' ~ 'não existe pecado ao sul do equador', 
        song == 'nao fala de maria' ~ 'não fala de maria', 
        song == 'nao sonho mais' ~ 'não sonho mais',  
        song == 'noticia de jornal' ~ 'notícia de jornal', 
        song == 'o circo mistico' ~ 'o circo místico', 
        song == 'o que sera a flor da pele' ~ 'o que será (à flor da pele)',  
        song == 'o ultimo blues' ~ 'o último blues',  
        song == 'onde que voce estava' ~ 'onde é que você estava', 
        song == 'barbara' ~ 'paroara',  
        song == 'pequea serenata diurna' ~ 'pequeña serenata diurna', 
        song == 'porque era ela porque era eu' ~ 'porque era ela, porque era eu', 
        song == 'qualquer cancao' ~ 'qualquer canção', 
        song == 'quando carnaval chegar' ~ 'quando o carnaval chegar', 
        song == 'quem te viu quem te ve' ~ 'quem te viu, quem te vê', 
        song == 'querido diario' ~ 'querido diário',  
        song == 'retrato em branco preto' ~ 'retrato em branco e preto', 
        song == 'rosa dos ventos' ~ 'rosa-dos-ventos', 
        song == 'samba e amore' ~ 'samba e amor', 
        song == 'sem voce' ~ 'sem você',  
        song == 'sera que cristina volta' ~ 'será que cristina volta?', 
        song == 'sonhos sonhos sao' ~ 'sonhos sonhos são',  
        song == 'suburbano coracao' ~ 'suburbano coração',  
        song == 'suburbio' ~ 'subúrbio', 
        song == 'tempo artista' ~ 'tempo e artista',
        song == 'tipo um baiao' ~ 'tipo um baião', 
        song == 'tira as maos de mim' ~ 'tira as mãos de mim',  
        song == 'todo sentimento' ~ 'todo o sentimento', 
        song == 'trapacas' ~ 'trapaças',  
        song == 'trocando em miudos' ~ 'trocando em miúdos',  
        song == 'um dia de cao' ~ 'um dia de cão',  
        song == 'uma cancao desnaturada' ~ 'uma canção desnaturada',  
        song == 'a valsa dos clows' ~ 'valsa dos clowns',  
        song == 'voce nao ouviu' ~ 'você não ouviu',  
        song == 'voce nao sabe amar' ~ 'você não sabe amar',  
        song == 'xote de navegacao' ~ 'xote de navegação',  
        TRUE ~ song))

# Rejoining data
all_data <- chords %>% 
  dplyr::inner_join(lyrics, by = "song") %>% 
  dplyr::inner_join(features, by = "song")

all_data %>% 
  count(song)

# Saving it!
write.table(all_data, "all_data.txt")

#-----------------------------------------------------------------------
# Exploratory analysis 
#-----------------------------------------------------------------------

# Lyrics part ----------------------------------------------------------
library(tidytext)
library(wordcloud)

all_data <- read.table("all_data.txt", stringsAsFactors = FALSE)

# Taking the portuguse stopwords from the `tm` package
stopwords_pt <- data.frame(word = tm::stopwords("portuguese"))

# Breaking phrases into words
unnested <- all_data %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  # Removing stopwords
  dplyr::anti_join(stopwords_pt, by = c("word" = "word"))

# Common words ---------------------------------------------------------
# 1-gram
unnested %>% 
  dplyr::count(word) %>% 
  dplyr::filter(n < quantile(n, 0.999)) %>% 
  dplyr::top_n(n = 30) %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_linerange(aes(ymin = min(n), ymax = n, x = reorder(word, n)),
                 position = position_dodge(width = 0.2), size = 1, 
                 colour = 'darksalmon') + 
  geom_point(colour = 'dodgerblue4', size = 3, alpha = 0.9) +
  coord_flip() +
  labs(x = 'Top 30 most common words', y = 'Counts') +
  theme_bw()

# The same, with a wordcloud
unnested %>% 
  count(word) %>% 
  with(wordcloud(word, n, family = "serif", 
                 random.order = FALSE, max.words = 30, 
                 colors = c("darksalmon", "dodgerblue4")))

# 2-grams
all_data %>% 
  select(text) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stopwords_pt$word, 
         !is.na(word1), !is.na(word2),
         !word2 %in% stopwords_pt$word) %>%
  count(word1, word2, sort = TRUE) %>% 
  mutate(word = paste(word1, word2)) %>% 
  filter(n < quantile(n, 0.999)) %>% 
  arrange(desc(n)) %>% 
  slice(1:30) %>%  
  ggplot(aes(reorder(word, n), n)) +
  geom_linerange(aes(ymin = min(n), ymax = n, x = reorder(word, n)),
                 position = position_dodge(width = 0.2), size = 1, 
                 colour = 'darksalmon') + 
  geom_point(colour = 'dodgerblue4', size = 3, alpha = 0.9) +
  coord_flip() +
  labs(x = 'Top 30 most common 2-grams', y = 'Counts') +
  theme_bw()

# Adding sentiments ----------------------------------------------------
# devtools::install_github("sillasgonzaga/lexiconPT")

# taking the sentiments from the `lexiconPT` packages
sentiments_pt <- lexiconPT::oplexicon_v2.1 %>% 
  mutate(word = term) %>% 
  select(word, polarity)  

# joining sentiments and the words from the songs 
add_sentiments <- all_data %>% 
  select(text, song) %>% 
  group_by_all() %>% 
  slice(1) %>% 
  ungroup() %>% 
  unnest_tokens(word, text) %>% 
  dplyr::anti_join(stopwords_pt, by = c("word" = "word")) %>% 
  dplyr::inner_join(sentiments_pt, by = c("word" = "word")) 

# separating the common words by sentiment
add_sentiments %>% 
  group_by(polarity) %>% 
  count(word) %>% 
  filter(n < quantile(n, 0.999)) %>% 
  top_n(n = 15) %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_linerange(aes(ymin = min(n), ymax = n, x = reorder(word, n)),
                 position = position_dodge(width = 0.2), size = 1, 
                 colour = 'darksalmon') + 
  geom_point(colour = 'dodgerblue4', size = 3, alpha = 0.9) +
  facet_wrap(~polarity, scales = "free") +
  coord_flip() +
  labs(x = 'Top 15 most common words', y = 'Counts', title = "Sentiments") +
  theme_bw()

# What are the most positive and negative songs? 
summ <- add_sentiments %>% 
  group_by(song) %>% 
  summarise(mean_pol = mean(polarity)) 

# Most positive and most negative songs
summ %>% 
  arrange(desc(mean_pol)) %>% 
  slice(c(1:15, 121:135)) %>% 
  mutate(situation = rep(c('+pos', '+neg'), each = 15)) %>% 
  ggplot(aes(reorder(song, mean_pol), mean_pol)) +
  geom_linerange(aes(ymin = min(mean_pol), ymax = mean_pol, 
                     x = reorder(song, mean_pol)),
                 position = position_dodge(width = 0.2), size = 1, 
                 colour = 'darksalmon') + 
  geom_point(colour = 'dodgerblue4', size = 3, alpha = 0.9) +
  facet_wrap(~situation, scales = "free") +
  coord_flip() +
  labs(x = 'Songs', y = 'Polarity') +
  theme_bw()

# Chords analysis ------------------------------------------------------
library(ggridges)

# Removing enarmonies
chords <- all_data %>%
  select(chord, song) %>% 
  dplyr::mutate(chord = case_when( 
    chord == "Gb" ~ "F#",
    chord == "C#" ~ "Db",          
    chord == "G#" ~ "Ab",
    chord == "A#" ~ "Bb",
    chord == "D#" ~ "Eb",
    chord == "E#" ~ "F",
    chord == "B#" ~ "C",
    TRUE ~ chord)) 

# Top 20 songs with more distinct chords
chords %>% 
  dplyr::group_by(song, chord) %>% 
  dplyr::summarise(distintos = n_distinct(chord)) %>% 
  dplyr::summarise(cont = n()) %>% 
  dplyr::mutate(song = fct_reorder(song, cont)) %>% 
  top_n(n = 20) %>% 
  ggplot(aes(y = cont, x = song)) +
  geom_bar(colour = 'dodgerblue4', fill = 'darksalmon',
           size = 0.5, alpha = 0.6, stat = "identity") +
  labs(x = 'Songs', y = 'Counts') +
  coord_flip() +
  theme_bw()


# Feature extraction from the chords
feat_chords <- all_data %>%
  select(chord, song) %>% 
  chorrrds::feature_extraction() %>% 
  select(-chord) %>% 
  group_by(song) %>% 
  summarise_all(mean)

dt <- feat_chords %>% 
  tidyr::gather(group, vars, minor, seventh, 
                seventh_M, sixth, fifth_dim, fifth_aug, 
                fourth, ninth, bass, dimi, augm)

# Checking current levels to rename
levels(as.factor(dt$group))

dt$group <- forcats::lvls_revalue(
  dt$group,
  c("Augmented", "Bass", "Diminished", 
    "Aug. Fifth", "Dim. Fifth", 
    "Fourth", "Minor", "Ninth", "Seventh",
    "Major Seventh", "Sixth"))

# Plotting densities of the extracted features
dt %>% 
  ggplot(aes(vars, group, fill = group)) +
  geom_density_ridges(alpha = 0.6) +
  scale_fill_cyclical(values = c("dodgerblue4", "darksalmon")) +
  guides(fill = FALSE) +
  xlim(0, 1) +
  labs(x = "Densities", y = "Extracted variables") +
  theme_bw()

# Creating our chord diagram 
comp <- chords %>% 
  dplyr::mutate(
    # Cleaning the chords to the base form
    chord_clean = stringr::str_extract(chord, pattern = "^([A-G]#?b?)"),
    seq = lead(chord_clean)) %>% 
  dplyr::filter(chord_clean != seq) %>% 
  dplyr::group_by(chord_clean, seq) %>%  
  dplyr::summarise(n = n())

mat <- tidyr::spread(comp, key = chord_clean, value = n, fill = 0)  
mm <- as.matrix(mat[, -1]) 

# Building the chords diagram
chorddiag::chorddiag(mm, showTicks = FALSE,
                     palette = "Blues")

# Spotify analysis ------------------------------------------------------
# Selecting the Spotify features
spot <- all_data %>% 
  group_by(song) %>% 
  slice(1) %>% 
  ungroup()

# Density of popularity
spot %>% 
  ggplot(aes(popul)) +
  geom_density(colour = 'dodgerblue4',
               fill = "darksalmon", 
               alpha = 0.8) +
  theme_bw()

# Most popular and least popular songs
spot %>% 
  arrange(desc(popul)) %>%  
  slice(c(1:15, 121:135)) %>% 
  mutate(situation = rep(c('+popul', '-popul'), each = 15)) %>% 
  select(popul, situation, song) %>% 
  ggplot(aes(reorder(song, popul), popul, group = 1)) +
  geom_point(colour = 'dodgerblue4',  size = 3, alpha = 0.9) +
  facet_wrap(~situation, scales = "free") +
  coord_flip() +
  labs(x = 'Songs', y = 'Popularity') +
  theme_bw()

dt <- spot %>% 
  select(energy,  
          loudness, speechiness, liveness, duration_ms, 
          acousticness) %>% 
  tidyr::gather(group, vars) 

dt$danceability <- spot$danceability

# Danceability x the other features
dt %>% 
ggplot(aes(danceability, vars)) +
  geom_point(colour = "darksalmon") +  
  geom_smooth(method  = "lm", colour = "dodgerblue4") +
  labs(x = "Danceability", y = "Variables") +
  facet_wrap(~group, scales = "free") +
  theme_bw()

#-----------------------------------------------------------------------
# Modelling part 
#-----------------------------------------------------------------------
library(randomForest)
# Creating a response variable 
library(randomForest)

spot <- spot %>% 
  mutate(pop_class = ifelse(
    popul < quantile(popul, 0.25), "unpopular",
    ifelse(popul < quantile(popul, 0.55), "neutral", "popular")))

spot %>% 
  janitor::tabyl(pop_class)

# Combining the previous datasets and wrangling
set.seed(1)
model_data <- feat_chords %>% 
  right_join(spot, by = c("song" = "song")) %>% 
  right_join(summ, by = c("song" = "song")) %>% 
  select(-analysis_url, -uri, -id.x, -id.y, -song, 
         -name, -text, -lang, -chord, -long_str,
         -key.x, -song.id, -sus, 
         -popul) %>% 
  mutate(pop_class = as.factor(pop_class)) %>% 
  # Separating into train and test set
  mutate(part = ifelse(runif(n()) > 0.25, "train", "test"))

model_data %>% 
  janitor::tabyl(part)

#  Separating in train set (75%) and test set (25%):

train <- model_data %>% 
  filter(part == "train") %>% 
  select(-part)

test <- model_data %>% 
  filter(part == "test") %>% 
  select(-part)

# The model will be like:
#   
#   ` pop_class ~ minor + dimi + augm + seventh + seventh_M + sixth + fourth + fifth_aug + fifth_dim + ninth + bass + danceability + energy + key.y + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + tempo + duration_ms + time_signature + mean_pol `

m0 <- randomForest(pop_class ~ ., data = train, 
                   ntree = 1000)
m0

# Visualizing the variable importance:
imp0 <- randomForest::importance(m0)
imp0 <- data.frame(var = dimnames(imp0)[[1]], 
                   value = c(imp0))

imp0 %>% 
  arrange(var, value) %>% 
  mutate(var = fct_reorder(factor(var),  value,  min)) %>% 
  ggplot(aes(var, value)) +
  geom_point(size = 3.5, colour = "darksalmon") +
  coord_flip() +
  labs(x = "Variables", y = "Decrease in Gini criteria") +
  theme_bw(14)

corrplot::corrplot(cor(train %>% select_if(is.numeric), 
                       method = "spearman"))

#  Redoing the model with the best variables
  
vars <- imp0 %>% 
  arrange(desc(value)) %>% 
  slice(1:10) %>% 
  pull(var)

form <- paste0("pop_class ~ ", paste0(vars, collapse = '+')) %>% 
  as.formula()

m1 <- randomForest(form, data = train, 
                   ntree = 1000, mtry = 5)
m1

# Measuring the accuracy in the test set

pred <- predict(m0, test)

sum(pred == test$pop_class)/nrow(test)
mean(m0$err.rate[,1])
#-----------------------------------------------------------------------
