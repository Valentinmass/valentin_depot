library(tidyverse)
library(readxl)
library(scales)
read_excel("Impact quantitatif de la PLF sur la santé des animaux laitiers..xlsx" ,na = "NA") -> impact 

# Transformer en format long
impact %>%
  pivot_longer(cols = c(AV_PLF, AP_PLF),names_to = "Situation",values_to = "Valeur") -> data_long 

data_long %>%
  mutate(Parametre = factor(`...1`, levels = unique(`...1`)),angle = 90 - 360 * (as.numeric(Parametre)-1) / length(unique(Parametre))) ->radial_data 


radial_data %>%
  ggplot +
  aes(x = Parametre, y = Valeur, fill = Situation) +
  
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  
  coord_polar(start = 0) +
  geom_text(aes(label = Valeur),position = position_dodge(width = 0.8),vjust = -0.5, size = 4) +
 
  scale_fill_manual(values = c("AV_PLF" = alpha("red",0.5), "AP_PLF" = alpha("blue",0.5)),labels = c("AV_PLF" = "Avant PLF", "AP_PLF" = "Après PLF")) +
  labs(title = "Impact quantitatif de la PLF sur la santé des animaux laitiers",
       subtitle = "Comparaison Avant PLF et Après PLF",
       y = "Valeur", x = "") +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")




# Transformer AMELIO en numérique si ce n'est pas déjà
impact %>%
  select(...1, AMELIO) %>%
  mutate(AMELIO = as.numeric(AMELIO), Parametre = factor(`...1`, levels = unique(`...1`))) ->impact_pct 

impact_pct %>%
  ggplot +
  aes(x = reorder(Parametre, AMELIO), y = AMELIO, fill = AMELIO) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(AMELIO,1), "%")), hjust = -0.1, size = 4) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Pourcentage d'amélioration entre Avant PLF et Après PLF", subtitle = "Colonne AMELIO = amélioration (%)",
    y = "Amélioration (%)", x = "") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")



