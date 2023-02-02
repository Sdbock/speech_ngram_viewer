


# Creating visualization functions ------

# setting some sizes 
line_size <- 1.75
label_size <- line_size * 2.5
nudge = 3
direction = "both"

# base plotting settings
plot_base <- 
  function(data, terms, years = c(1952,2020), ...) {
    data %>%
      ungroup() %>%
      filter(word %in% terms,
             between(election,years[1],years[2])) %>%
      ggplot(aes(x = election, y = frequency, group = factor(word), linetype = factor(word))) +
      scale_x_continuous(limits = c(years[1],years[2] + 4),breaks = seq(years[1],years[2],8)) +
      scale_y_continuous(labels = scales::percent_format()) +
      ggthemes::theme_clean(base_size = 18, base_family = "Helvetica") +
      labs(
        y = "Frequency",
        x = "Election year",
        linetype = NULL
      ) +
      expand_limits(x = 2060) +
      theme(
        plot.background = element_rect(color = "white"),
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom",
        legend.justification = "right",
        legend.background = element_rect(color = "grey", 
                                         size = .5))
        
      
}

plot_overall <- function(...) {
  plot_base(data = ngrams_overall,...) +
    geom_line(aes(color = word), 
              show.legend = FALSE,
              size = line_size) +
    labs(color = NULL,
         linetype = NULL) +
    ggrepel::geom_label_repel(data = . %>% filter(election == max(election)),
                             aes(label = word, color = word),
                             arrow = grid::arrow(angle = 30, length = unit(0.07, "inches") ),
                             segment.color = "black",
                             segment.size = .25,
                             size = label_size,
                             min.segment.length = Inf,
                             hjust = 0,
                             nudge_x = nudge,
                             direction = "both",
                             show.legend = FALSE,
                             segment.curvature = -0.1,
                             segment.ncp = 3,
                             segment.angle = 20) +
    ggthemes::scale_color_economist() 
}


plot_party <- function(...) {
  
  dems <- "#016a8e"
  reps <- "#b1283a"
  
  plot_base(data = ngrams_byparty,...) +
    geom_line(data = . %>% filter(party == "Democrat"), aes(color  = party), size = line_size) +
    geom_line(data = . %>% filter(party == "Republican"), aes(color = party), size = line_size) +
    scale_color_manual(values = c(dems,reps)) +
    ggrepel::geom_label_repel(data = . %>% group_by(party, word) %>% filter(election == max(election)),
                             aes(label = word,
                                 color = party),
                                 arrow = grid::arrow(angle = 30, length = unit(0.07, "inches") ),
                                 segment.color = "black",
                                 segment.size = .25,
                                 size = label_size,
                                 min.segment.length = Inf,
                                 hjust = 0,
                                 nudge_x = nudge,
                                 direction = "both",
                                 show.legend = FALSE,
                                 segment.curvature = -0.1,
                                 segment.ncp = 3,
                                 segment.angle = 20) +
    labs(
      color = NULL,
    ) 

}


plot_ngrams <- 
  function(byparty = TRUE, facet_ngram = FALSE, facet_party = FALSE,...) {
  
    if(byparty == TRUE) {
      plot <- plot_party(...)
    }
    
    else{
      plot <- plot_overall(...)
    }
    
    if(facet_ngram == FALSE) {
      plot <- plot
    }
    
    else {
      plot <- plot +
        facet_wrap(~word, ncol = 2) +
        theme(
          strip.text.x = element_blank() 
        )
    } 
    
    if(byparty == TRUE & facet_party ==  TRUE){
      plot <- plot +
        facet_wrap(~party,ncol = 2) +
        theme(
          strip.text.x = element_blank()
        )
    } 
    else{
      plot <- plot
    }
   if(byparty == TRUE & facet_party == TRUE & facet_ngram == TRUE) {
     plot <- plot +
       facet_grid(word ~ party) +
       theme(
         strip.text.x = element_blank(),
         strip.text.y = element_blank()
       )
   }
  plot
}


