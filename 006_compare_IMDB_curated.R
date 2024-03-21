library(tidyverse)

setwd("/Users/veronica/Documents/Genetics in Media/")

IMDB_keep <- read.delim("004_IMDB_keep.tsv")
IMDB_tv <- read_delim("title.episode.tsv")

IMDB_keep <- IMDB_keep %>%
    mutate(Medium = tconst %in% IMDB_tv$parentTconst |
               tconst %in% IMDB_tv$tconst)

curated_data <- read.csv("Genetics_in_media_20240229.csv")
curated_data[which(curated_data$Medium %in% 
                        c("TV Ep", "TV series")),"Medium"] <- "TV"
curated_data[which(curated_data$Medium %in% 
                        c("Film", "TV movie", "Movie", "TV Movie")),"Medium"] <- "Film"


# plot number of films and tv shows per year all
pdf("figs/006_hist_all.pdf", height = 2, width  = 3)
ggplot(IMDB_keep,
       aes(x = startYear,
           fill = Medium,
           color = Medium)) +
    geom_step(stat = "bin", direction = "mid") +
    geom_histogram(alpha = 0.3, 
                   position = "identity",
                   color = NA) + 
    theme_classic() +
    labs(x = NULL) +
    scale_fill_manual(values = c("red", "blue"), guide = "none")  +
    scale_color_manual(values = c("red", "blue"), guide = "none") +
    scale_x_continuous(limits = c(1900, NA), breaks = c(1900, 1925, 1950, 1975, 2000, 2025))
dev.off()

pdf("figs/006_hist_curated.pdf", height = 2, width  = 3)
ggplot(curated_data,
       aes(x = Year,
           fill = Medium,
           color = Medium)) +
    geom_step(stat = "bin", direction = "mid") +
    geom_histogram(alpha = 0.3, 
                   position = "identity",
                   color = NA) + 
    theme_classic() +
    labs(x = NULL) +
    scale_fill_manual(values = c("red", "blue"), guide = "none")  +
    scale_color_manual(values = c("red", "blue"), guide = "none") +
    scale_x_continuous(limits = c(1900, NA), breaks = c(1900, 1925, 1950, 1975, 2000, 2025))
dev.off()


# Genres
# 

genres_curated <- curated_data$Genre %>% strsplit(",") %>% unlist() %>% unique()
count_genres_curated <- matrix(0, nrow = length(genres_curated), ncol = 4)
rownames(count_genres_curated) <- genres_curated
for(g in genres_curated){
    hold <- curated_data[grepl(g, curated_data$Genre),]
    count_genres_curated[g, 1] <- nrow(filter(hold, Medium == "Film"))
    count_genres_curated[g, 2] <- count_genres_curated[g, 1]/nrow(filter(curated_data, Medium == "Film")) * 100
    count_genres_curated[g, 3] <- nrow(filter(hold, Medium == "TV"))
    count_genres_curated[g, 4] <- count_genres_curated[g, 3]/nrow(filter(curated_data, Medium == "TV")) * 100
}

plot_genre_curated <- count_genres_curated %>%
    as.data.frame() %>%
    mutate(Genre = row.names(.)) %>%
    select(Genre, V1, V3) %>%
    gather("Medium", "count", -Genre)
    

pdf("figs/006_genre_count_curated.pdf", height = 3, width = 5)
ggplot(plot_genre_curated,
       aes(x = Genre,
           y = count,
           fill = Medium)) +
    geom_col(position = "dodge") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 60, h = 1)) +
    scale_fill_manual(values = c("red", "blue"), guide = "none") +
    labs(x = NULL)
dev.off()


