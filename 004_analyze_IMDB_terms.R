library(tidyverse)
library(vctrs)
library(gridExtra)

setwd("/Users/veronica/Documents/Genetics in Media/")

IMDB <- read.csv("003_IMDB.csv")
IMDB_tv <- read_delim("title.episode.tsv")

load("003_list_IMDB_keywords.RData")

genetic_terms <- list.files("004_keywords/")
select_keywords <- list()
for(f in genetic_terms){
    select_keywords[[gsub(".txt", "", f)]] <- c(read.delim(paste0("004_keywords/", f),  
                                                         header = F)) %>%
                                            unlist() %>% 
                                            as.vector()
}


####  pull keywords
all_keywords <- do.call(c, list_keywords)
all_keywords <- gsub("reference to ", "", all_keywords) # remove keywords that are references to something
# all_keywords <- grep("[0-9]+", # remove keywords that include numbers
#                      all_keywords,
#                      invert = TRUE,
#                      value = TRUE)
# filter for keywords that occur at least twice, make a table
table_keywords <- table(all_keywords) %>% 
    as.data.frame() %>%
    filter(Freq > 1)
#write.csv(table_keywords, "004_table_keyword_freq.csv", row.names = F)


list_keywords <- list_drop_empty(list_keywords) # ignore IMDB entries without keywords associated
IMDB_keep <- filter(IMDB, tconst %in% names(list_keywords))
IMDB_keep <- IMDB_keep[order(match(IMDB_keep$tconst, names(list_keywords))),]

##############################################
## search for specific keywords in the data ##
##############################################

for(k in names(select_keywords)){
    IMDB_keep[,k] <- lapply(list_keywords, function(x){
        return(sum(grepl(paste0("^",select_keywords[[k]],"$", collapse = "|"), x)) > 0) }
        ) %>% 
        unlist() %>% as.integer()
}

IMDB_keep$scientist <- lapply(list_keywords, function(x){
    return(sum(grepl("scientist", x)) > 0) }) %>%
    unlist() %>% as.integer()
IMDB_keep$nanotech <- lapply(list_keywords, function(x){
    return(sum(grepl("^nano", x)) > 0) }) %>% 
    unlist() %>% as.integer()
IMDB_keep$alien <- lapply(list_keywords, function(x){
    return(sum(grepl("alien", x)) > 0) }) %>% 
    unlist() %>% as.integer()
IMDB_keep$superhero <- lapply(list_keywords, function(x){
    return(sum(grepl("superhero", x)) > 0) }) %>% 
    unlist() %>% as.integer()

write.table(IMDB_keep, "004_IMDB_keep.tsv", 
            quote = F, row.names = F, sep = "\t")


#### identify TV episodes vs film media
IMDB_tv_filt <- IMDB_keep %>%
    filter(tconst %in% IMDB_tv$parentTconst |
               tconst %in% IMDB_tv$tconst)
IMDB_film_filt <- IMDB_keep %>%
    filter(!(tconst %in% IMDB_tv$parentTconst) &
               !(tconst %in% IMDB_tv$tconst))

# plot: percent of movies with genetic theme as percentage per year over time
IMDB_year <- table(IMDB_keep$startYear) %>%
    as.data.frame()
genetics_year_tv <- table(filter(IMDB_keep, Genetics == 1 & 
                                  (tconst %in% IMDB_tv$parentTconst |
                                  tconst %in% IMDB_tv$tconst))$startYear) %>%
    as.data.frame()
genetics_year_film <- table(filter(IMDB_keep, Genetics == 1 & 
                                     !(tconst %in% IMDB_tv$parentTconst |
                                      tconst %in% IMDB_tv$tconst))$startYear) %>%
    as.data.frame()
compare_year <- merge(IMDB_year, genetics_year_tv,
                      by = "Var1",
                      all.x = T) %>%
    merge(genetics_year_film,
          by = "Var1",
          all.x = T) %>%
    mutate(percent_tv = Freq.y/Freq.x * 100,
           percent_film = Freq/Freq.x * 100)
compare_year[is.na(compare_year)] <- 0

# ggplot(compare_year,
#        aes(x = as.integer(as.character(Var1)),
#            y = Freq.y)) +
#     geom_point() +
#     #geom_smooth(formula = "y~x", method = "lm") +
#     theme_classic() +
#     scale_y_log10()

pdf("figs/004_time_proportion_genetics.pdf", height = 2, width  = 3)
ggplot(compare_year,
       aes(x = as.integer(as.character(Var1)))) +
    geom_vline(xintercept = 1999, linetype = 2, color = "grey") +
    geom_point(aes(y = percent_tv), color = "blue", alpha = 0.4) +
    geom_smooth(aes(y = percent_tv), color = "blue", span = 0.3, se = F) +
    geom_point(aes(y = percent_film), color = "red", alpha = 0.4) +
    geom_smooth(aes(y = percent_film), color = "red", span = 0.3, se = F) +
    theme_classic() +
    labs(y = "Percent", x = NULL) +
    scale_x_continuous(breaks = c(1900, 1925, 1950, 1975, 2000, 2025)) +
    scale_y_continuous(breaks = c(0, 0.5, 1))
dev.off()

# pdf("figs/004_time_occuring.pdf", height = 2, width  = 3)
# for(f in c("BioWarTerror","ChimeraTransGen","Cloning",
#            # "Conservation","Evolution","GeneEngHu","GeneTherapy",
#            "DNAtoID", "Eugenics","GeneDisease" ,"GeneScreen",
#            "Genetics","GMOs","Mutation","OrganHarvest","Sex")){
#     print( ggplot(IMDB_tv_filt[which(IMDB_tv_filt[,f] == 1),], 
#            aes(x = as.integer(as.character(startYear)))) +
#         stat_ecdf(color = "blue") +
#         stat_ecdf(data = IMDB_film_filt[which(IMDB_film_filt[,f] == 1),],
#                   color = "red") +
#         theme_classic()  +
#         lims(x = c(1916, 2023)) +
#         labs(title = f, x = NULL, y = NULL) )
# }
# dev.off()

pdf("figs/004_time_occuring.pdf", height = 2, width  = 3)
print( ggplot(IMDB_tv_filt[which(IMDB_tv_filt$GMOs == 1),], 
              aes(x = as.integer(as.character(startYear)))) +
           geom_vline(xintercept = 1992, linetype = 2, color = "grey") +
           geom_vline(xintercept = 1973, linetype = 2, color = "grey") +
           stat_ecdf(color = "blue") +
           stat_ecdf(data = IMDB_film_filt[which(IMDB_film_filt$GMOs == 1),],
                     color = "red") +
           theme_classic()  +
           lims(x = c(1916, 2023)) +
           labs(title = "GMOs", x = NULL, y = NULL) )
# print( ggplot(IMDB_tv_filt[which(IMDB_tv_filt$GeneDisease == 1),], 
#               aes(x = as.integer(as.character(startYear)))) +
#            geom_vline(xintercept = 1969, linetype = 2, color = "grey") +
#            geom_vline(xintercept = 1994, linetype = 2, color = "grey") +
#            geom_vline(xintercept = 2003, linetype = 2, color = "grey") +
#            stat_ecdf(color = "blue") +
#            stat_ecdf(data = IMDB_film_filt[which(IMDB_film_filt$GeneDisease == 1),],
#                      color = "red") +
#            theme_classic()  +
#            lims(x = c(1916, 2023)) +
#            labs(title = "Genetic Disease", x = NULL, y = NULL) )
print( ggplot(IMDB_tv_filt[which(IMDB_tv_filt$DNAtoID == 1),], 
              aes(x = as.integer(as.character(startYear)))) +
           geom_vline(xintercept = 1984, linetype = 2, color = "grey") +
           geom_vline(xintercept = 2007, linetype = 2, color = "grey") +
           stat_ecdf(color = "blue") +
           stat_ecdf(data = IMDB_film_filt[which(IMDB_film_filt$DNAtoID == 1),],
                     color = "red") +
           theme_classic()  +
           lims(x = c(1916, 2023)) +
           labs(title = "DNA to ID", x = NULL, y = NULL) )
print( ggplot(IMDB_tv_filt[which(IMDB_tv_filt$Eugenics == 1),], 
              aes(x = as.integer(as.character(startYear)))) +
           geom_vline(xintercept = 1963, linetype = 2, color = "grey") +
           geom_vline(xintercept = 1945, linetype = 2, color = "grey") +
           stat_ecdf(color = "blue") +
           stat_ecdf(data = IMDB_film_filt[which(IMDB_film_filt$Eugenics == 1),],
                     color = "red") +
           theme_classic()  +
           lims(x = c(1916, 2023)) +
           labs(title = "Eugenics", x = NULL, y = NULL) )
print( ggplot(IMDB_tv_filt[which(IMDB_tv_filt$Cloning == 1),], 
              aes(x = as.integer(as.character(startYear)))) +
           geom_vline(xintercept = 1996, linetype = 2, color = "grey") +
           stat_ecdf(color = "blue") +
           stat_ecdf(data = IMDB_film_filt[which(IMDB_film_filt$Cloning == 1),],
                     color = "red") +
           theme_classic()  +
           lims(x = c(1916, 2023)) +
           labs(title = "Cloning", x = NULL, y = NULL) )
dev.off()

pdf("figs/004_time_occuring_ctrls.pdf", height = 2, width  = 3)
print( ggplot(IMDB_tv_filt[which(IMDB_tv_filt$Coronavirus == 1),], 
              aes(x = as.integer(as.character(startYear)))) +
           geom_vline(xintercept = 2019, linetype = 2, color = "grey") +
           geom_vline(xintercept = 2002, linetype = 2, color = "grey") +
           stat_ecdf(color = "blue") +
           stat_ecdf(data = IMDB_film_filt[which(IMDB_film_filt$Coronavirus == 1),],
                     color = "red") +
           theme_classic()  +
           lims(x = c(1916, 2023)) +
           labs(title = "Coronavirus", x = NULL, y = NULL) )
print( ggplot(IMDB_tv_filt[which(IMDB_tv_filt$HIV == 1),], 
              aes(x = as.integer(as.character(startYear)))) +
           geom_vline(xintercept = 1987, linetype = 2, color = "grey") +
           geom_vline(xintercept = 1981, linetype = 2, color = "grey") +
           stat_ecdf(color = "blue") +
           stat_ecdf(data = IMDB_film_filt[which(IMDB_film_filt$HIV == 1),],
                     color = "red") +
           theme_classic()  +
           lims(x = c(1916, 2023)) +
           labs(title = "HIV", x = NULL, y = NULL) )
dev.off()

# film overall gets started earlier
pdf("figs/004_time_occuring_all_media_compare.pdf", height = 2, width = 3)
ggplot(IMDB_tv_filt, 
       aes(x = as.integer(as.character(startYear)))) +
    stat_ecdf(color = "blue") +
    stat_ecdf(data = IMDB_film_filt,
              color = "red") +
    stat_ecdf(data = IMDB_film_filt[which(IMDB_film_filt[,"Genetics"] == 1),],
              color = "red", linetype = 2) +
    stat_ecdf(data = IMDB_tv_filt[which(IMDB_tv_filt[,"Genetics"] == 1),],
              color = "blue", linetype = 2) +
    theme_classic()  +
    scale_x_continuous(breaks = c(1900, 1925, 1950, 1975, 2000, 2025)) +
    labs(x = NULL, y = NULL)
dev.off()


############################
### CORRELATION OF TERMS ###
############################
test <- IMDB_keep %>% filter(Genetics == 1)
# mutation correlates with radiation
chisq.test(matrix(c(
    nrow(filter(test, Radiation == 0 & Mutation == 0)),
    nrow(filter(test, Radiation == 1 & Mutation == 0)),
    nrow(filter(test, Radiation == 0 & Mutation == 1)),
    nrow(filter(test, Radiation == 1 & Mutation == 1))
), ncol  = 2),
simulate.p.value = T)
#       [,1] [,2]
# [1,] 2687  630
# [2,]   11   32
# p-value = 0.0004998 (simulated)


# mutation correlates with monster
chisq.test(matrix(c(
    nrow(filter(test, Monster == 0 & Mutation == 0)),
    nrow(filter(test, Monster == 1 & Mutation == 0)),
    nrow(filter(test, Monster == 0 & Mutation == 1)),
    nrow(filter(test, Monster == 1 & Mutation == 1))
), ncol  = 2),
simulate.p.value = T)
#       [,1] [,2]
# [1,] 2626  490
# [2,]   72  172
# p-value = 0.0004998 (simulated)


# Cloning anti-correlates with Mutation
chisq.test(matrix(c(
    nrow(filter(test, Cloning == 0 & Mutation == 0)),
    nrow(filter(test, Cloning == 1 & Mutation == 0)),
    nrow(filter(test, Cloning == 0 & Mutation == 1)),
    nrow(filter(test, Cloning == 1 & Mutation == 1))
), ncol  = 2),
simulate.p.value = T)
#       [,1] [,2]
# [1,] 2390  653
# [2,]  308    9
# p-value = 0.0004998 (simulated)

# # DNAtoID anti-correlates with GeneDisease
# chisq.test(matrix(c(
#     nrow(filter(test, DNAtoID == 0 & GeneDisease == 0)),
#     nrow(filter(test, DNAtoID == 1 & GeneDisease == 0)),
#     nrow(filter(test, DNAtoID == 0 & GeneDisease == 1)),
#     nrow(filter(test, DNAtoID == 1 & GeneDisease == 1))
# ), ncol  = 2),
# simulate.p.value = T)
# 
# # DNAtoID anti-correlates with Sex
# chisq.test(matrix(c(
#     nrow(filter(test, DNAtoID == 0 & Sex == 0)),
#     nrow(filter(test, DNAtoID == 1 & Sex == 0)),
#     nrow(filter(test, DNAtoID == 0 & Sex == 1)),
#     nrow(filter(test, DNAtoID == 1 & Sex == 1))
# ), ncol  = 2),
# simulate.p.value = T)

# Eugenics correlates with nazi
chisq.test(matrix(c(
    nrow(filter(test, Eugenics == 0 & Nazi == 0)),
    nrow(filter(test, Eugenics == 1 & Nazi == 0)),
    nrow(filter(test, Eugenics == 0 & Nazi == 1)),
    nrow(filter(test, Eugenics == 1 & Nazi == 1))
), ncol  = 2),
simulate.p.value = T)
#      [,1] [,2]
# [1,] 3234   27
# [2,]   78   8
# p-value = 0.0004998 (simulated)

# Mutation does not correlate with nazi (despite X-Men)
chisq.test(matrix(c(
    nrow(filter(test, Mutation == 0 & Nazi == 0)),
    nrow(filter(test, Mutation == 1 & Nazi == 0)),
    nrow(filter(test, Mutation == 0 & Nazi == 1)),
    nrow(filter(test, Mutation == 1 & Nazi == 1))
), ncol  = 2),
simulate.p.value = T)
#      [,1] [,2]
# [1,] 2653   29
# [2,]  656    6
# p-value = 0.8221 (simulated)

# Cloning does not correlate with nazi (despite Boys from Brazil's popularity)
chisq.test(matrix(c(
    nrow(filter(test, Cloning == 0 & Nazi == 0)),
    nrow(filter(test, Cloning == 1 & Nazi == 0)),
    nrow(filter(test, Cloning == 0 & Nazi == 1)),
    nrow(filter(test, Cloning == 1 & Nazi == 1))
), ncol  = 2),
simulate.p.value = T)
#      [,1] [,2]
# [1,] 2995   31
# [2,]  313    4
# p-value = 0.7741

######################
### observe genre ####
######################
# only look at IMDB entires with any keywords, since there may be a skew
#film
genres_film <- IMDB_film_filt$genres %>% strsplit(",") %>% unlist() %>% unique()
count_genres_film <- matrix(0, nrow = length(genres_film), ncol = 2)
rownames(count_genres_film) <- genres_film
for(g in genres_film){
    hold <- IMDB_film_filt[grepl(g, IMDB_film_filt$genres),]
    count_genres_film[g, 1] <- nrow(hold)
}
IMDB_test_film <- filter(IMDB_film_filt, Genetics == 1) # change for subsetting
for(g in genres_film){
    hold <- IMDB_test_film[grepl(g, IMDB_test_film$genres),]
    count_genres_film[g, 2] <- nrow(hold)
}
count_genres_film_plot <- count_genres_film %>%
    as.data.frame() %>%
    mutate(percent = V2/V1 * 100)
count_genres_film_plot$genre <- rownames(count_genres_film_plot)
f <- ggplot(count_genres_film_plot,
       aes(x = genre,
           y = percent)) +
    geom_col(fill = "white", color = "black") +
    theme_classic() +
    theme(axis.text.x = element_blank()) +
    lims(y = c(0, 8.1)) + labs(x = NULL)

#tv
genres_tv <- IMDB_tv_filt$genres %>% strsplit(",") %>% unlist() %>% unique()
count_genres_tv <- matrix(0, nrow = length(genres_tv), ncol = 2)
rownames(count_genres_tv) <- genres_tv
for(g in genres_tv){
    hold <- IMDB_tv_filt[grepl(g, IMDB_tv_filt$genres),]
    count_genres_tv[g, 1] <- nrow(hold)
}
IMDB_test_tv <- filter(IMDB_tv_filt, Genetics == 1) # change for subsetting
for(g in genres_tv){
    hold <- IMDB_test_tv[grepl(g, IMDB_test_tv$genres),]
    count_genres_tv[g, 2] <- nrow(hold)
}
count_genres_tv_plot <- count_genres_tv %>%
    as.data.frame() %>%
    mutate(percent = V2/V1 * 100)
count_genres_tv_plot$genre <- rownames(count_genres_tv_plot)
t <- ggplot(count_genres_tv_plot,
       aes(x = genre,
           y = percent)) +
    geom_col(fill = "white", color = "black") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 60, h = 1)) +
    lims(y = c(0, 8.1)) + labs(x = NULL)
# plot together
pdf("figs/004_genre_enrichment_genetics.pdf", height = 3, width = 5)
grid.arrange(f,t, heights = c(0.4, 0.6))
dev.off()

# plot absolute number of each
count_genres_plot <- rbind(mutate(count_genres_film_plot, medium = "Film"),
                           mutate(count_genres_tv_plot, medium = "TV"))

pdf("figs/004_genre_count_genetics.pdf", height = 3, width = 5)
ggplot(count_genres_plot,
       aes(x = genre,
           y = V2,
           fill = medium)) +
    geom_col(position = "dodge") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 60, h = 1)) +
    #scale_y_log10() +
    scale_fill_manual(values = c("red", "blue"), guide = F) +
    labs(y = "count", x = NULL)
dev.off()

