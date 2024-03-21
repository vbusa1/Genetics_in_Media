library(tidyverse)
library(betareg)
library(RColorBrewer)

setwd("/Users/veronica/Documents/Genetics in Media/")

curated_data <- read.csv("Genetics_in_media_20240229.csv")
curated_data[which(curated_data$Medium %in% 
                       c("TV Ep", "TV series")),"Medium"] <- "TV"
curated_data[which(curated_data$Medium %in% 
                       c("Film", "TV movie", "Movie", "TV Movie")),"Medium"] <- "Film"

###################
# look at attitude over time
# beta regression for proportion modeling
attitude <- curated_data %>%
    dplyr::select(Year, Medium, Attitude) %>%
    group_by(Medium, Attitude, Year) %>%
    summarize(n = n()) %>%
    spread(Attitude, n, 0) %>%
    rowwise() %>%
    mutate(n = sum(negative, mixed, positive))
# can't have 0 or 1 so must make mini-bins of data
temp <- list()
for(m in c("Film", "TV")){
    hold <- attitude %>% filter(Medium ==  m) %>%
        mutate(startYear = Year)
    hold <- hold[order(hold$Year),]
    for(y in 1:nrow(hold)){
        if(hold[y, "positive"] == hold[y, "n"] | hold[y, "positive"] == 0 |
           hold[y, "negative"] == hold[y, "n"] | hold[y, "negative"] == 0 |
           hold[y, "mixed"] == hold[y, "n"] | hold[y, "mixed"] == 0 ){
            hold[y+1, "startYear"] <- hold[y, "startYear"]
            hold[y+1, 3:6]  <- hold[y+1, 3:6] + hold[y, 3:6]
            hold[y, "n"] <- 0
        }
    }
    temp[[m]] <- hold %>% filter(n > 0)
}
attitude_group <- do.call(rbind, temp) %>%
    rowwise() %>%
    mutate(negative = negative/n,
           mixed = mixed/n,
           positive = positive/n)

attitude_plot <- attitude_group  %>%
    gather(Attitude, prop, -Year, -startYear, -Medium, -n)
pdf("figs/005_attitude_time.pdf", width = 6.5, height = 3)
ggplot(attitude_plot,
       aes(x = Year,
           y = prop,
           color = factor(Attitude, levels = c("negative", "mixed", "positive")),
           fill = factor(Attitude, levels = c("negative", "mixed", "positive")))) +
    facet_wrap(~Medium, nrow = 1) +
    geom_smooth(method="lm",
                alpha = 0.2,
                size = 0,
                formula = y ~ splines::bs(x, knots = c(2010), degree = 1)) +
    geom_point(size = 2) +
    geom_smooth(se = F) +
    theme_classic() +
    theme(legend.position = "none") +
    labs(x = NULL, y = "proportion") +
    lims(x = c(1960,NA))
dev.off()


film_model <- filter(attitude_group, Medium == "Film", Year >= 1960, Year <= 2010)
model.beta_film = betareg(positive ~ Year, 
                          data = film_model)
summary(model.beta_film)
# positive 0.550; mixed 0.001128 (+); negative 0.00108 (-)
film_model <- filter(attitude_group, Medium == "Film", Year > 2010)
model.beta_film = betareg(negative ~ Year, 
                          data = film_model)
summary(model.beta_film)
# positive 0.00119 (+); mixed 0.000908 (-); negative 0.0441 (+)

TV_model <- filter(attitude_group, Medium == "TV", Year >= 1960, Year <= 2010)
model.beta_tv = betareg(negative ~ Year, 
                        data = TV_model)
summary(model.beta_tv)
# positive 0.0873; mixed 0.00608 (+); negative 3.12e-05 (-)
TV_model <- filter(attitude_group, Medium == "TV", Year > 2010)
model.beta_tv = betareg(negative ~ Year, 
                        data = TV_model)
summary(model.beta_tv)
# positive 0.0574; mixed 0.109; negative  0.891

# test: bin by years (e.g. 5-yr) and look at effect across genres


###################
# look at topics
deck_topic <- c("Biobank",
                "BioWar/Terror",
                "Chimera/TransGen",
                "Cloning",
                "Conservation",
                "DataPrivacy",
                "DNAtoID",
                "Essentialism",
                "Eugenics",
                "Evolution",
                "Forensics",
                "GeneDisease",
                "GeneEngHu",
                "GeneScreen",
                "GeneTherapy",
                "GMOs",
                "Monoculture",
                "Mutation",
                "OrganHarvest",
                "Sex",
                "WorldHunger")
for(d in deck_topic){
    curated_data[,d] <- grepl(d, curated_data$Genetic.Topic.Issue)
}

pdf("figs/005_time_occuring_curated.pdf", height = 2, width = 3)
for(d in deck_topic){
    print( ggplot(curated_data[which(curated_data[,d] == T),], 
                  aes(x = as.integer(as.character(Year)),
                      color = Medium)) +
               stat_ecdf() +
               theme_classic()  +
               lims(x = c(1916, 2023)) +
               labs(title = d, x = NULL, y = NULL) +
               scale_color_manual(values = c("red", "blue"), 
                                  guide = "none"))
}
dev.off()
# note: nothing amazing worth including in figures other than confirmation
# for recapitulation of crowd-sourced data; esp. Eugenics 


###################
# look at organism and who
deck_organism <- c("animal", "human", "microbe", "plant")
deck_who <- c("Government", "Private", "Academic", "Accident", "Corporation", "Medical")

for(d in deck_organism){
    curated_data[,d] <- grepl(d, curated_data$Organism.affected)
}
for(d in deck_who){
    curated_data[,d] <- grepl(d, curated_data$Who.is.doing.the.manipulation.)
}

# calculate chi-sq and plot the Pearson residuals, (observed - expected) / sqrt(expected)
Effect <- curated_data[,c("Medium", deck_organism, deck_who)]
Effect <- gather(Effect, "who", "affecter", -c("animal", "human", "microbe", "plant", "Medium")) %>%
    filter(affecter == T) %>%
    gather("organism", "affected", -c("who", "affecter", "Medium")) %>%
    filter(affected == T) %>%
    select(-affecter, -affected)
Effect_film <- table(select(filter(Effect, Medium == "Film"), -Medium))
test_film <- chisq.test(Effect_film)$residuals


test_film_gg <- as.data.frame(test_film) %>%
    mutate(count = as.data.frame(Effect_film)$Freq)
pdf("figs/005_chi_resid_film.pdf", height = 2.5, width = 3.5)
ggplot(test_film_gg,
       aes(x = who,
           y = organism)) +
    geom_tile(aes(fill = Freq))+
    geom_text(aes(label = count)) +
    scale_fill_gradient(low = "white", high = "red") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 60, h = 1)) +
    labs(x = NULL, y = NULL, fill = "residual")
dev.off()


Effect_tv <- table(select(filter(Effect, Medium == "TV"), -Medium))
test_tv <- chisq.test(Effect_tv)$residuals

test_tv_gg <- as.data.frame(test_tv) %>%
    mutate(count = as.data.frame(Effect_tv)$Freq)
pdf("figs/005_chi_resid_tv.pdf", height = 2.5, width = 3.5)
ggplot(test_tv_gg,
       aes(x = who,
           y = organism)) +
    geom_tile(aes(fill = Freq))+
    geom_text(aes(label = count)) +
    scale_fill_gradient(low = "white", high = "blue") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 60, h = 1)) +
    labs(x = NULL, y = NULL, fill = "residual")
dev.off()

# is the organism a monster?
Effect_monster <- curated_data[,c("Medium", deck_organism, deck_who, "Mutant.is.monster.")]
Effect_monster <- gather(Effect_monster, "who", "affecter", -c("animal", "human", "microbe", "plant", "Medium", "Mutant.is.monster.")) %>%
    filter(affecter == T) %>%
    gather("organism", "affected", -c("who", "affecter", "Medium", "Mutant.is.monster.")) %>%
    filter(affected == T) %>%
    select(-affecter, -affected)
Effect_monster <- table(Effect_monster) %>% as.data.frame()

pdf("figs/005_organism_is_monster.pdf", height = 2, width = 4)
ggplot(Effect_monster,
       aes(x = Medium,
           y  = Freq,
           fill = factor(Mutant.is.monster., levels = c("No", "Mixed", "Yes"))))  +
    facet_wrap(~ organism, nrow = 1) +
    geom_col(position = "fill") +
    labs(fill = NULL, x = NULL) +
    theme_classic() 
dev.off()

###################
# look at theme
deck_theme <- c("Adoption/NonBioParent",
              "Autonomy",
              "Bioethics",
              "Childhood",
              "Disability",
              "Discrimination",
              "Exploitation",
              "Forensics",
              "Galatea",
              "SexGender",
              "Genealogy",
              "Individualism",
              "Interdependence",
              "LGBTQIA",
              "Medicine/neg",
              "Medicine/pos",
              "Monsters",
              "Mortality",
              "NatureVNurture",
              "Posthumanism",
              "Prometheus",
              "Race",
              "Religion",
              "Sacrifice",
              "SciCold",
              "SciDedicated",
              "SciMad",
              "SciNazi",
              "Vulnerability")

for(d in deck_theme){
    curated_data[,d] <- grepl(d, curated_data$Theme)
}

# calculate chi-sq and plot the Pearson residuals, (observed - expected) / sqrt(expected)
Effect <- curated_data[,c("Medium", deck_topic, deck_theme)]
Effect <- gather(Effect, "topic", "present", -c(deck_theme, "Medium")) %>%
    filter(present == T) %>%
    gather("theme", "present2", -c("topic", "present", "Medium")) %>%
    filter(present2 == T) %>%
    select(-present, -present2)
Effect_film <- table(select(filter(Effect, Medium == "Film"), -Medium))
test_film <- chisq.test(Effect_film)$residuals


test_film_gg <- as.data.frame(test_film) %>%
    mutate(count = as.data.frame(Effect_film)$Freq)
test_film_gg[which(test_film_gg$Freq > 10), "Freq"] <- 10

pdf("figs/005_chi_resid_film_theme_topic.pdf", height = 5, width = 6)
ggplot(test_film_gg,
       aes(x = topic,
           y = theme)) +
    geom_tile(aes(fill = Freq))+
    # geom_text(aes(label = count)) +
    scale_fill_gradient(low = "white", high = "red") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, h = 1, v = .3)) +
    labs(x = NULL, y = NULL, fill = "residual")
dev.off()


Effect <- curated_data[,c("Medium", deck_topic, deck_theme)]
Effect <- gather(Effect, "topic", "present", -c(deck_theme, "Medium")) %>%
    filter(present == T) %>%
    gather("theme", "present2", -c("topic", "present", "Medium")) %>%
    filter(present2 == T) %>%
    select(-present, -present2)
Effect_tv <- table(select(filter(Effect, Medium == "TV"), -Medium))
test_tv <- chisq.test(Effect_tv)$residuals


test_tv_gg <- as.data.frame(test_tv) %>%
    mutate(count = as.data.frame(Effect_tv)$Freq)
test_tv_gg[which(test_tv_gg$Freq > 10), "Freq"] <- 10
pdf("figs/005_chi_resid_tv_theme_topic.pdf", height = 5, width = 6)
ggplot(test_tv_gg,
       aes(x = topic,
           y = theme)) +
    geom_tile(aes(fill = Freq))+
    # geom_text(aes(label = count)) +
    scale_fill_gradient(low = "white", high = "blue") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, h = 1, v = .3)) +
    labs(x = NULL, y = NULL, fill = "residual")
dev.off()

test_all <- merge(test_tv_gg, test_film_gg,
                  by = c("topic", "theme"))
colnames(test_all) <- c("topic", "theme", "tv_freq", "tv_count", "film_freq", "film_count")


# pdf("005_test_freq_corr.pdf", height = 20, width = 20)
# ggplot(test_all,
#        aes(x = tv_freq,
#            y = film_freq)) +
#     geom_abline(slope = 1, color = "grey") +
#     geom_vline(xintercept = 0, color = "grey") +
#     geom_hline(yintercept = 0, color = "grey") +
#     geom_point() +
#     ggrepel::geom_text_repel(aes(label = paste(topic, theme))) +
#     theme_classic()  +
#     lims(x = c(-4.62, 10), y = c(-4.62, 10))
# dev.off()


label_subset <- test_all  %>%
    filter((topic == "Forensics.1" & theme == "Forensics") |
               (topic == "Cloning" & theme == "Individualism") |
               (topic == "GeneDisease" & theme == "Disability") |
               (topic == "DNAtoID" & theme == "Forensics") |
               (topic == "Mutation" & theme == "Monsters") |
               (topic == "GeneEngHu" & theme == "Posthumanism") |
               (topic == "Eugenics" & theme == "Discrimination") |
               (topic == "Essentialism" & theme == "NatureVNurture") |
               (topic == "Chimera/TransGen" & theme == "Monsters") |
               (topic == "Eugenics" & theme == "SciNazi") |
               (topic == "Sex" & theme == "LGBTQIA") |
               (topic == "Conservation" & theme == "Interdependence") |
               (topic == "GeneTherapy" & theme == "Medicine/pos") |
               (topic == "GeneTherapy" & theme == "Medicine/neg") |
               (topic == "Cloning" & theme == "Monsters") |               
               (topic == "GMOs" & theme == "SciMad") |
               (topic == "Chimera/TransGen" & theme == "Prometheus") |
               (topic == "GeneEngHu" & theme == "Galatea")  |
               (topic == "GeneEngHu" & theme == "Forensics"))

pdf("figs/005_freq_corr.pdf", height = 4.5, width = 10)
ggplot(test_all,
       aes(x = tv_freq,
           y = film_freq)) +
    geom_abline(slope = 1, color = "grey", linetype = 2) +
    geom_vline(xintercept = 0, color = "grey", linetype = 2) +
    geom_hline(yintercept = 0, color = "grey", linetype = 2) +
    geom_point(size = 3, color = "grey70", shape = 1) +
    geom_point(data = label_subset, size = 3, color = "grey50") +
    ggrepel::geom_text_repel(data = label_subset, 
                             aes(label = paste(topic, theme, sep = ":"), 
                                 fontface = "bold"),
                             force = 3,
                             box.padding = 0.5) +
    theme_classic()  +
    lims(x = c(-4.62, 10), y = c(-4.62, 10))
dev.off()


###################
# look at accuracy vs attitude
accuracy <- curated_data %>%
    dplyr::select(Medium, Attitude, Accuracy) %>%
    group_by(Medium, Attitude, Accuracy) %>%
    summarize(n = n()) %>%
    group_by(Medium, Attitude) %>%
    mutate(n2 = sum(n))
pdf("figs/005_attitude_accuracy.pdf", height = 2, width = 4.5)
ggplot(accuracy,
       aes(x = factor(Attitude,levels = c("negative", "mixed", "positive")),
           y =  n,
           fill = factor(Accuracy, levels = c("low", "mid", "high")))) +
    facet_wrap(~Medium)  +
    geom_col(position = "fill") +
    theme_classic() +
    labs(fill = "Accuracy", x = "Attitude", y = "proportion")
dev.off()

beta_accuracy <- accuracy %>% 
    mutate(n = n/n2,
           Attitude = recode(Attitude, negative = 0, mixed = 1, positive = 2)) %>%
    filter(Accuracy == "low")
model.beta_film = betareg(n ~ Attitude + Medium, 
                          data = beta_accuracy)
summary(model.beta_film)
#              Estimate Std. Error   z value      Pr(>|z|)
# (Intercept)  1.5982136 0.07486818  21.34703 4.154933e-101
# Attitude    -0.4399456 0.04444577  -9.89848  4.226516e-23
# MediumTV    -1.2535756 0.07279537 -17.22054  1.862441e-66

beta_accuracy <- accuracy %>% 
    mutate(n = n/n2,
           Attitude = recode(Attitude, negative = 0, mixed = 1, positive = 2)) %>%
    filter(Accuracy == "high")
model.beta_film = betareg(n ~ Attitude + Medium, 
                          data = beta_accuracy)
summary(model.beta_film)
#              Estimate Std. Error  z value  Pr(>|z|)
# (Intercept)  -2.8073     0.2050 -13.693  < 2e-16
# Attitude      0.4642     0.1115   4.164 3.13e-05
# MediumTV      0.9340     0.1862   5.016 5.26e-07


###################
# look at genre
genre <- c("Animation",
            "Comedy",
            "Sci-Fi",
            "Fantasy",
            "Horror",
            "Adventure",
            "Action",
            "Drama",
            "Thriller",
            "Romance",
            "Family",
            "Mystery",
            "Crime")

for(d in genre){
    curated_data[,d] <- grepl(d, curated_data$Genre)
}

genre_monster <- curated_data[,c("Medium", genre, "Mutant.is.monster.")]
genre_monster <- gather(genre_monster, "genre", "relevant", -c("Medium", "Mutant.is.monster.")) %>%
    filter(relevant == T,
           !is.na(Mutant.is.monster.)) %>%
    select(-relevant)
genre_monster <- table(genre_monster) %>% 
    as.data.frame() %>%
    group_by(genre, Medium) %>%
    mutate(n = sum(as.numeric(Freq)),
           prop = Freq/n)

# establish order:
with(filter(genre_monster, Mutant.is.monster. == "Yes", Medium == "Film"), reorder(genre, prop, max))
pdf("figs/005_genre_is_monster.pdf", height = 2, width = 6)
ggplot(genre_monster,
       aes(x = factor(genre, levels = c("Romance",   "Animation","Family",
                                        "Crime", "Comedy", "Drama","Mystery", "Action",
                                        "Adventure", "Fantasy", "Thriller", "Sci-Fi", "Horror")),
           y = Freq,
           fill = factor(Mutant.is.monster., levels = c("No", "Mixed", "Yes"))))  +
    facet_wrap(~Medium) +
    geom_col(position = "fill") +
    labs(fill = "is Monster", x = NULL, y = NULL)  +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 60, h = 1))
dev.off()

genre_attitude <- curated_data[,c("Medium", genre, "Attitude")]
genre_attitude <- gather(genre_attitude, "genre", "relevant", -c("Medium", "Attitude")) %>%
    filter(relevant == T) %>%
    select(-relevant)
genre_attitude <- table(genre_attitude) %>% 
    as.data.frame() %>%
    group_by(genre, Medium) %>%
    mutate(n = sum(as.numeric(Freq)),
           prop = Freq/n)

#########################
## centrality of genetics

centrality_accuracy <- curated_data %>%
    dplyr::select(Medium, Accuracy, Centrality.of.genetics) %>%
    group_by(Medium, Accuracy, Centrality.of.genetics) %>%
    summarize(n = n()) %>%
    group_by(Medium, Accuracy) %>%
    mutate(n2 = sum(n))
pdf("figs/005_accuracy_centrality.pdf", height = 2, width = 5)
ggplot(centrality_accuracy,
       aes(x = factor(Accuracy,levels = c("low", "mid", "high")),
           y =  n,
           fill = Centrality.of.genetics)) +
    facet_wrap(~Medium)  +
    geom_col(position = "fill") +
    theme_classic() +
    labs(fill = "Centrality of genetics", x = "Accuracy", y = "proportion")
dev.off()

beta_centrality_accuracy <- centrality_accuracy %>% 
    mutate(n = n/n2,
           Accuracy = recode(Accuracy, low = 0, mid = 1, high = 2)) %>%
    filter(Centrality.of.genetics == "central")
model.beta_film = betareg(n ~ Accuracy + Medium, 
                          data = beta_centrality_accuracy)
summary(model.beta_film)
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -0.8867     0.2720  -3.260  0.00111 ** 
# Accuracy      0.9832     0.1863   5.279  1.3e-07 ***
# MediumTV     -0.6566     0.2918  -2.250  0.02445 *  

centrality_attitude <- curated_data %>%
    dplyr::select(Medium, Attitude, Centrality.of.genetics) %>%
    group_by(Medium, Attitude, Centrality.of.genetics) %>%
    summarize(n = n()) %>%
    group_by(Medium, Centrality.of.genetics) %>%
    mutate(n2 = sum(n))
pdf("figs/005_attitude_centrality.pdf", height = 2, width = 4)
ggplot(centrality_attitude,
       aes(fill = factor(Attitude,levels = c("negative", "mixed", "positive")),
           y =  n,
           x = Centrality.of.genetics)) +
    facet_wrap(~Medium)  +
    geom_col(position = "fill") +
    theme_classic() +
    labs(x = "Centrality of genetics", fill = "Attitude", y = "proportion")
dev.off()

beta_centrality_attitude <- centrality_attitude %>% 
    mutate(n = n/n2,
           Centrality.of.genetics = recode(Centrality.of.genetics, peripheral = 0, central = 1)) %>%
    filter(Attitude == "mixed")
model.beta_film = betareg(n ~ Centrality.of.genetics + Medium, 
                          data = beta_centrality_attitude)
summary(model.beta_film)
#                         Estimate Std. Error    z value     Pr(>|z|)
# (Intercept)            -0.9045129 0.05759858 -15.703736 1.425920e-55
# Centrality.of.genetics  0.6883103 0.06418398  10.724020 7.851487e-27
# MediumTV                0.1859924 0.06400130   2.906072 3.659966e-03


beta_centrality_attitude <- centrality_attitude %>% 
    mutate(n = n/n2,
           Centrality.of.genetics = recode(Centrality.of.genetics, peripheral = 0, central = 1)) %>%
    filter(Attitude == "negative")
model.beta_film = betareg(n ~ Centrality.of.genetics + Medium, 
                          data = beta_centrality_attitude)
summary(model.beta_film)
#                         Estimate Std. Error   z value     Pr(>|z|)
# (Intercept)             0.4099403 0.07317423  5.602250 2.115872e-08
# Centrality.of.genetics -0.7326212 0.08502224 -8.616818 6.884014e-18
# MediumTV               -0.5063993 0.08498844 -5.958450 2.546418e-09

beta_centrality_attitude <- centrality_attitude %>% 
    mutate(n = n/n2,
           Centrality.of.genetics = recode(Centrality.of.genetics, peripheral = 0, central = 1)) %>%
    filter(Attitude == "negative")
model.beta_film = betareg(n ~ Centrality.of.genetics + Medium, 
                          data = beta_centrality_attitude)
summary(model.beta_film)
#                       Estimate Std. Error z value Pr(>|z|)    
# (Intercept)            -2.02964    0.07765 -26.138  < 2e-16
# Centrality.of.genetics  0.11389    0.08212   1.387    0.165    
# MediumTV                0.58596    0.08370   7.001 2.55e-12
