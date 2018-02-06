
# Setup ----------------------------------
library(dplyr)
library(magrittr)
library(future.apply)
library(ggplot2)
library(stringr)
library(proxy)

plan(multiprocess)

# Read files -----------------------------
cols <- scan("../opname_csv_gb/DOC/OS_Open_Names_Header.csv", sep=',', what="")
files <- list.files("../opname_csv_gb/DATA", full.names = TRUE)
out <- future_lapply(files,FUN=readr::read_csv,col_names=cols) %>% bind_rows
rm(cols, files)

# Select London --------------------------
lon <- out %>% 
  filter(REGION=="London") %>% 
  filter(stringr::str_detect(POSTCODE_DISTRICT,"^(E|EC|WC|N|NW|W|SE|SW)\\d")) %>%
  filter(LOCAL_TYPE %in% c("Named Road","Section Of Named Road"))


# lon %>% group_by(POSTCODE_DISTRICT) %>% count(sort=F) %>% View
# lon %>% filter(stringr::str_detect(NAME1,"Perrymead")) %>% View

# lon %>%
#   group_by(COUNTY_UNITARY,DISTRICT_BOROUGH,POPULATED_PLACE) %>% 
#   count(sort=T) %>% 
#   View

# lon %>% group_by(LOCAL_TYPE) %>% count()
# lon %>% filter(LOCAL_TYPE=="Section Of Named Road") %>% View

# lon %>% group_by(NAME1) %>% filter(n()>1) %>% arrange(NAME1) %>%  View


# Are names the same ? -------------------
lon %<>% mutate(ROAD_TYPE = NAME1 %>% stringr::str_replace("\\s\\([\\w\\s]+\\)","") %>% stringr::word(-1)) 
# lon %>% group_by(ROAD_TYPE) %>% count(sort=T) %>% View
# lon %>% filter(ROAD_TYPE=="Cedars") %>% select(NAME1)

road_types <- lon %>% 
  group_by(ROAD_TYPE) %>% 
  count(sort=T) %>% 
  filter(n>=10) %>% 
  pull(ROAD_TYPE) %>% 
  stringr::str_to_lower() %>% 
  setdiff(c("east","west","north","south"))

road_types


tokens_unf <- lon %>% 
  select(NAME1, POPULATED_PLACE, DISTRICT_BOROUGH, LOCAL_TYPE) %>% 
  tidytext::unnest_tokens(word, NAME1) %>% 
  group_by(word) %>% 
  count(sort=T)

tokens <- tokens_unf %>% 
  filter(!word %in% c(road_types,"the"))

tokens %>% print(n=100)

tokens %>% 
  ungroup %>% 
  top_n(50) %>% 
  mutate(word = factor(word, levels = rev(word))) %>% 
  ggplot+
  geom_col(aes(x=word, y=n))+
  geom_label(aes(x=word,y=n, label=n))+
  coord_flip()


lon %>% filter(str_detect(NAME1,"^St ")) %>% pull(NAME1)
lon %>% filter(str_detect(NAME1,"Hall \\w")) %>% pull(NAME1)


# Word clustering ------------------------
# files <- list.files("../glove.6B", full.names = TRUE)
# word_vec <- data.table::fread("../glove.6B/glove.6B.300d.txt") %>% tibble::as.tibble()
word_vec <- data.table::fread("../", skip=1L) %>% tibble::as.tibble()

names(word_vec) <- c("word", paste0("dim_",1:(ncol(word_vec)-1)))


words <- tokens_unf %>% 
  filter(n>5) %>% 
  select(word) %>% 
  inner_join(word_vec)

# words_distances <- distances::distances(words, id_variable = "word")
words_distances <- dist(words %>% tibble::column_to_rownames(var="word"), method="cosine")
# dim(as.matrix(word_distances))
words_cluster <- hclust(words_distances, method="complete")
plot(words_cluster, hang = -1, cex = 0.6)

words_groups <- cutree(words_cluster, h=0.98)
# words_groups %>% unique %>% length
words_grouped <- words %>% select(word) %>% bind_cols(tibble(group=words_groups))

# words_kmeans <- kmeans(words %>% ungroup %>% select(-word),30, iter.max = 30)
# words_grouped <- words %>% select(word) %>% bind_cols(tibble(group=words_kmeans$cluster))


words_grouped %>% 
  group_by(group) %>% 
  count(sort=T) %>%  
  ggplot +
  geom_col(aes(x=group,y=n))

# tmp <- words_grouped %>% filter(word=="queen") %>% pull(group)
# words_grouped %>% 
#   select(word, group) %>% 
#   filter(group==tmp) %>% 
#   pull(word)

# tmp <- words_grouped %>% filter(word=="road") %>% pull(group)
# words_grouped %>% 
#   select(word, group) %>% 
#   filter(group==tmp) %>% 
#   pull(word)

words_grouped %>% split(., .[,"group"]) %>% purrr::map(pull,word)

words_grouped %>% 
  filter(word %in% road_types) %>% 
  View

## Testing PCA
words_pca <- prcomp(words %>% tibble::column_to_rownames(var="word"))

words_pca_tbl <- data.frame(word=words$word,group=words_groups,words_pca$x) %>% tibble::as.tibble()

words_pca_tbl %>% 
  ggplot(aes(x=PC1,y=PC2))+
  geom_label(aes(label=word, fill=as.factor(group)))


# PARIS ----------------------------

# readLines("../BAN_licence_gratuite_repartage_75/BAN_licence_gratuite_repartage_75.csv",2)

par <- readr::read_csv2("../BAN_licence_gratuite_repartage_75/BAN_licence_gratuite_repartage_75.csv")

par %<>% select(nom_voie) %>% 
  distinct %>% 
  mutate(road_type = word(nom_voie,1))

par %>% group_by(road_type) %>% 
  count(sort=T) %>% 
  filter(n>5) %>% 
  ungroup %>% 
  mutate(road_type = factor(road_type, levels=rev(road_type))) %>% 
  ggplot+
  geom_col(aes(x=road_type,y=n))+
  geom_label(aes(x=road_type,y=n, label=n))+
  coord_flip()

par_road_types <-  par$road_type %>% unique

par_tokens_unf <- par %>%
  tidytext::unnest_tokens(word,nom_voie) %>% 
  group_by(word) %>% 
  count(sort= T)

par_tokens <- par_tokens_unf %>% 
  filter(!word %in% c(par_road_types,"du","de","des","la","le","et","aux","à","au"))

par_tokens %>% 
  ungroup %>% 
  top_n(50) %>% 
  mutate(word = factor(word, levels = rev(word))) %>% 
  ggplot+
  geom_col(aes(x=word, y=n))+
  geom_label(aes(x=word,y=n, label=n))+
  coord_flip()
