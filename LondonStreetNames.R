
# Setup ----------------------------------
library(dplyr)
library(magrittr)
library(future.apply)
library(ggplot2)
library(stringr)

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
glove <- data.table::fread("../glove.6B/glove.6B.100d.txt") %>% tibble::as.tibble()
names(glove) <- c("word", paste0("dim_",1:100))


words <- tokens_unf %>% 
  filter(n>5) %>% 
  select(word) %>% 
  inner_join(glove)

# words_distances <- distances::distances(words, id_variable = "word")
words_distances <- dist(words, method="euclidian")
# dim(as.matrix(word_distances))
words_cluster <- hclust(words_distances, method="ward.D2")
plot(words_cluster, hang = -1, cex = 0.6)

words_groups <- cutree(words_cluster, k=10)
# words_groups %>% unique %>% length
words_grouped <- words %>% select(word) %>% bind_cols(tibble(group=words_groups))
rm(words_groups)

words_kmeans <- kmeans(words %>% ungroup %>% select(-word),10, iter.max = 30)
words_grouped <- words %>% select(word) %>% bind_cols(tibble(group=words_kmeans$cluster))


words_grouped %>% 
  group_by(group) %>% 
  count(sort=T) %>%  
  ggplot +
  geom_col(aes(x=group,y=n))

tmp <- words_grouped %>% filter(word=="queen") %>% pull(group)
words_grouped %>% 
  select(word, group) %>% 
  filter(group==tmp) %>% 
  pull(word)

tmp <- words_grouped %>% filter(word=="road") %>% pull(group)
words_grouped %>% 
  select(word, group) %>% 
  filter(group==tmp) %>% 
  pull(word)

words_grouped %>% 
  select(word, group) %>% 
  filter(group==10) %>% 
  pull(word)

words_grouped %>% 
  filter(word %in% road_types) %>% 
  View
