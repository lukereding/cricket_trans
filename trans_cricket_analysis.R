

## read in the data
setwd("~/Documents/cricket_trans/data/")
files <- list.files()
files <- files[!files %in% "README.md"]

require(tidyverse)
require(magrittr)


for(i in 1:length(files)){
  df <- read_csv(files[i])
  print(files[i])
  df$female <- rep(1:(nrow(df)/3), each = 3)
  df$trial <- rep(c("a_vs_b", "b_vs_c", "a_vs_c"), (nrow(df)/3))
  names(df)[1] <- "pref"
  df$trial <- factor(df$trial, levels = c("a_vs_b", "b_vs_c", "a_vs_c"))
  df$exp <- files[i]
  # 
  df %<>% spread(trial, pref)
  # 
  here <- return.transitivity.directed.plots(df)
  here <- here[[2]]
  here$exp <- files[i]
  if(i == 1){
    final <- here
    df_total <- df
  }
  else{
    final <- bind_rows(final, here)
    df_total <- bind_rows(df_total, df)
  }
}

# df_total$exp %<>% factor(levels = c("pulse and loudness 1", "pulse and loudness 2", "pulse and loudness 3", "chirp and loudness 1", "chirp and loudness 2", "chirp and loudness 3", "chirp and pulse 1", "chirp and pulse 2", "chirp and pulse 3"))

final$pattern %<>% factor(levels = c( "lisl", "sils", "sli" , "lsi",  "sil" , "ils" ,  "isl"  ,"lis"))

## make sure boxplots look right
df_total %>%
  gather(trial, pref, a_vs_b:a_vs_c) %>%
  {
    .$trial <- factor(.$trial, levels = c("a_vs_b", "b_vs_c", "a_vs_c"))
    .
  } %>%
  ggplot(aes(trial, pref)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_boxplot(coef = 100) +
  geom_line(aes(group = female), col = "#00000050") +
  facet_wrap(~exp, nrow = 3, ncol = 3)

## individual data
require(ggthemes)
df_total %>%
  gather(trial, pref, a_vs_b:a_vs_c) %>%
  {
    .$trial <- factor(.$trial, levels = c("a_vs_b", "b_vs_c", "a_vs_c"))
    .
  } %>%
  ggplot(aes(trial, pref)) +
  geom_jitter(height = 0, width = 0.1, aes(color = factor(female))) +
  facet_wrap(~exp, nrow = 3, ncol = 3)+
  geom_hline(yintercept = 0, col = "grey50") +
  scale_colour_tableau(guide = F)



require(treemapify)
require(ggthemes)
require(viridis)
final %>% 
  group_by(exp, pattern) %>% 
  tally %>%
ggplot(aes(area = n, fill = pattern, label = pattern)) +
  geom_treemap() +
  facet_wrap(~exp) +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    grow = TRUE
  ) +
  scale_fill_manual(values = c("#E84F22","orange", viridis(6)))

final %>% 
  group_by(exp, transitive_status) %>% 
  tally %>%
  ggplot(aes(area = n, fill = transitive_status, label = transitive_status)) +
  geom_treemap() +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    grow = TRUE
  ) +
  facet_wrap(~exp) +
  scale_fill_manual(values = c("#E84F22","#8B8B8B", viridis(6)))


final %>% 
  group_by(exp, stoch) %>% 
  filter(transitive_status != "intransitive") %>% 
  tally %>%
  ggplot(aes(area = n, fill = stoch, label = stoch)) +
  geom_treemap() +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    grow = TRUE
  ) +
  ggtitle("looking only at transitive gals...") +
  facet_wrap(~exp) +
  scale_fill_manual(values = c("#E84F22","#8B8B8B", viridis(6)))

source("~/Documents/random_scripts/plotting_functions.R")
final %>% 
  filter(transitive_status != "intransitive") %>%
  mutate(class = ifelse(exp %in% c("panel_a_values", "panel_b_values", "panel_c_values"), "one", 
                                   ifelse(exp %in% c("panel_d_values", "panel_e_values", "panel_f_values"), "two", "three"))) %>% {
                                     .$class <- factor(.$class, levels = c("one", "two", "three"))
                                     .
                                   } %>%
  group_by(class, stoch) %>% 
  tally %>%
  ggplot(aes(x = class, y = n)) +
  geom_col(aes(fill = stoch)) +
  facet_wrap(~stoch, ncol = 3) +
  rotate_labels() +
  scale_fill_pen()


final %>% 
  group_by(exp, best_male) %>% 
  tally %>%
  ggplot(aes(area = n, fill = best_male, label = best_male)) +
  geom_treemap() +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    grow = TRUE
  ) +
  facet_wrap(~exp) +
  scale_fill_manual(values = c("#E84F22","#8B8B8B", viridis(6)))

# more / fewer intransitivies than expected by chance?
nested <- final %>%
  group_by(exp) %>%
  nest

nested %<>% mutate(
  sample_size = map_int(data, nrow),
  number_trans = map_int(data, ~ .x %>% filter(transitive_status == "transitive") %>% nrow),
  number_intrans = map_int(data, ~ .x %>% filter(transitive_status == "intransitive") %>% nrow),
  prop_intrans = number_intrans / (number_intrans + number_trans)
)
total_trans <- sum(nested$number_trans)
total_intrans <- sum(nested$number_intrans)

binom.test(total_intrans, n = total_intrans + total_trans, p = 0.25)



# 
# df <- read_csv(files[1])
# 
# 
# df$female <- rep(1:(nrow(df)/3), each = 3)
# df$trial <- rep(c("a_vs_b", "b_vs_c", "a_vs_c"), (nrow(df)/3))
# names(df)[1] <- "pref"
# df$trial <- factor(df$trial, levels = c("a_vs_b", "b_vs_c", "a_vs_c"))
# 
# df %<>% spread(trial, pref)











return.transitivity.directed.plots <- function(df){
  
  library(igraph)
  library(network)
  library(sna)
  library(ggplot2)
  library(GGally)
  library(intergraph)
  library(cowplot)
  
  # allocate a list for the graphs
  plot_list <- vector(mode = "list", length = nrow(df))
  
  # allocate vectors for female x index dataframe
  female_vector <- index_vector <- transitive <- stoch <- pattern <- best_male <- vector(length = nrow(df))
  
  # make all the nodes stay in the same spots across all graphs
  g <- graph( c("S", "I", "I", "L", "S", "L"), directed = TRUE)
  l <- layout.reingold.tilford(g) 
  rm(g)
  
  for(i in 1:nrow(df)){
    
    # get the name of the fish
    name <- df$female[i] %>% as.character
    print(name)
    
    x <- df$a_vs_b[i]
    y <- df$b_vs_c[i]
    z <- df$a_vs_c[i]
    print(c(x,y,z))
    
    
    # determine the direction of preference for each comparison
    # transitive: all x, y, z > 0
    if(x >= 0 && y >= 0 && z >= 0){
      g <- graph( c("S", "I", "I", "L", "S", "L"), directed = TRUE)
      E(g)$weight <- c(df$a_vs_b[i], df$b_vs_c[i], df$a_vs_c[i]) 
      g <- set.graph.attribute(g, "transitivity", "transitive")
      g <- set.graph.attribute(g, "pattern", "lis")
      g <- set.graph.attribute(g, "best_male", "large")
      ss <- ifelse(z <= min(x,y), "violates moderate",
                   ifelse(z <= max(x,y), "violates strong", "neither"))
      g <- set.graph.attribute(g, "ss", ss)
      
    } else if(x < 0 & y >= 0 & z >= 0){
      g <- graph( c("I", "S", "I", "L", "S", "L"), directed = TRUE)
      E(g)$weight <- c((1 - df$a_vs_b[i]), df$b_vs_c[i], df$a_vs_c[i]) 
      g <- set.graph.attribute(g, "transitivity", "transitive")
      g <- set.graph.attribute(g, "best_male", "large")
      g <- set.graph.attribute(g, "pattern", "lsi")
      ss <- ifelse(y <= min(1-x,z), "violates moderate",
                   ifelse(y <= max(1-x,z), "violates strong", "neither"))
      g <- set.graph.attribute(g, "ss", ss)
      
    } else if(x >= 0 & y < 0 & z >= 0){
      g <- graph( c("S", "I", "L", "I", "S", "L"), directed = TRUE)
      E(g)$weight <- c(df$a_vs_b[i], (1-df$b_vs_c[i]), df$a_vs_c[i]) 
      g <- set.graph.attribute(g, "transitivity", "transitive")
      g <- set.graph.attribute(g, "best_male", "intermediate")
      g <- set.graph.attribute(g, "pattern", "ils")
      ss <- ifelse(x <= min(1-y,z), "violates moderate",
                   ifelse(x <= max(1-y,z), "violates strong", "neither"))
      g <- set.graph.attribute(g, "ss", ss)
      
    } else if(x >= 0 & y >= 0 & z < 0){
      g <- graph( c("S", "I", "I", "L", "L", "S"), directed = TRUE)
      E(g)$weight <- c(df$a_vs_b[i], df$b_vs_c[i], (1-df$a_vs_c[i])) 
      g <- set.graph.attribute(g, "transitivity", "intransitive")
      g <- set.graph.attribute(g, "best_male", "none")
      g <- set.graph.attribute(g, "pattern", "lisl")
      g <- set.graph.attribute(g, "ss", "neither")
      
    } else if(x >= 0 & y < 0 & z < 0){
      g <- graph( c("S", "I", "L", "I", "L", "S"), directed = TRUE)
      E(g)$weight <- c(df$a_vs_b[i], (1-df$b_vs_c[i]), (1-df$a_vs_c[i])) 
      g <- set.graph.attribute(g, "transitivity", "transitive")
      g <- set.graph.attribute(g, "best_male", "intermediate")
      g <- set.graph.attribute(g, "pattern", "isl")
      ss <- ifelse(1-y <= min(x, 1-z), "violates moderate",
                   ifelse(1-y <= max(x, 1-z), "violates strong", "neither"))
      g <- set.graph.attribute(g, "ss", ss)
      
    } else if(x < 0 & y < 0 & z < 0){
      g <- graph( c("I", "S", "L", "I", "L", "S"), directed = TRUE)
      E(g)$weight <- c((1-df$a_vs_b[i]), (1-df$b_vs_c[i]), (1-df$a_vs_c[i])) 
      g <- set.graph.attribute(g, "transitivity", "transitive")
      g <- set.graph.attribute(g, "best_male", "small")
      g <- set.graph.attribute(g, "pattern", "sil")
      ss <- ifelse(1-z <= min(1-x, 1-y), "violates moderate",
                   ifelse(1-z <= max(1-x, 1-y), "violates strong", "neither"))
      g <- set.graph.attribute(g, "ss", ss)
      
    } else if(x < 0 & y >= 0 & z < 0){
      g <- graph( c("I", "S", "I", "L", "L", "S"), directed = TRUE)
      E(g)$weight <- c((1-df$a_vs_b[i]), df$b_vs_c[i], (1-df$a_vs_c[i])) 
      g <- set.graph.attribute(g, "transitivity", "transitive")
      g <- set.graph.attribute(g, "best_male", "small")
      g <- set.graph.attribute(g, "pattern", "sli")
      ss <- ifelse(1-x <= min(y, 1-z), "violates moderate",
                   ifelse(1-x <= max(y, 1-z), "violates strong", "neither"))
      g <- set.graph.attribute(g, "ss", ss)
      
    } else if(x < 0 & y < 0 & z >= 0){
      g <- graph( c("I", "S", "L", "I", "S", "L"), directed = TRUE)
      E(g)$weight <- c((1-df$a_vs_b[i]), (1-df$b_vs_c[i]), df$a_vs_c[i]) 
      g <- set.graph.attribute(g, "transitivity", "intransitive")
      g <- set.graph.attribute(g, "best_male", "none")
      g <- set.graph.attribute(g, "pattern", "sils")
      g <- set.graph.attribute(g, "ss", "neither")
      
    } else{
      stop("something's gone wrong. check if else statements.")
    } ## end series of if-else statement
    
    # coerse to network class
    g <- asNetwork(g)
    
    network::set.edge.attribute(g, "color", ifelse(g %e% "weight" >= 0, "black", "grey80"))
    network::set.edge.attribute(g, "thickness", g %e% "weight" *2)
    network::set.vertex.attribute(g, "color", ifelse(network::get.network.attribute(g, "transitivity") == "intransitive", "red", "grey50"))
    
    # calculate the index
    # this is a little tricky since in a transitive network, we have to figure out the comparison that doesn't really matter
    if(network::get.network.attribute(g, "transitivity") == "intransitive"){
      index <- g %e% "weight" %>% 
        min %>% 
        subtract(0) %>%
        multiply_by(-1)
    } else if(network::get.network.attribute(g, "best_male") == "large") {
      index <- g %e% "weight" %>% 
        extract(c(2,3)) %>%
        min %>%
        subtract(0)
    } else if(network::get.network.attribute(g, "best_male") == "intermediate"){
      index <- g %e% "weight" %>% 
        extract(c(1,2)) %>%
        min %>%
        subtract(0)
    }
    else if(network::get.network.attribute(g, "best_male") == "small"){
      index <- g %e% "weight" %>% 
        extract(c(1,3)) %>%
        min %>%
        subtract(0)
    } else{
      stop(paste0("trouble calcualting intransitivity index for ", name))
    }
    # if(i == 7){
    #   browser()
    # }
    # 
    plot_list[[i]] <- ggnet2(g,
                             label = TRUE,
                             layout.exp = .3,
                             arrow.size = 12,
                             arrow.gap = 0.1,
                             edge.label = "weight",
                             edge.size = "thickness",
                             edge.color = "color",
                             node.color = "color",
                             size = 15,
                             mode = l
    ) + ggtitle(name)
    
    # name the vector element
    names(plot_list)[i] <- name
    print(i)
    
    # add the female's name and the index to vectors
    female_vector[i] <- name
    index_vector[i] <- index
    transitive[i] <- network::get.network.attribute(g, "transitivity")
    pattern[i] <- network::get.network.attribute(g, "pattern")
    stoch[i] <- network::get.network.attribute(g, "ss")
    best_male[i] <- network::get.network.attribute(g, "best_male")
    
  } ## end for loop
  
  # make a dataframe from the female and index vectors
  d <- data.frame(female = female_vector, index = index_vector, transitive_status = transitive, pattern = pattern, stoch = stoch, best_male = best_male)
  
  return(list(plot_list, d))
} # end function
