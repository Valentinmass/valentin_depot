library(tidyverse)
library(dplyr)
library(stringi)
library(ggplot2)
# read_table (pour les txt)
read_csv2("subve.csv") -> subve #pour CSV en tidy
#View(subve)
subve

#remettre les caractères correectement
subve <- subve %>%
  mutate(
    REGION = stri_trans_general(REGION, "Latin-ASCII")
  )

## les pivots ils ne servent à rien 

subve %>%
  pivot_longer(cols = c(SUBV, RBE, PROD)) -> subve_long 
subve_long

## création indicateurs
subve %>%
  mutate(subv_par_expl = SUBV / NBEX,
         subv_par_hab  = SUBV / POPULATION,
         subv_par_km2  = SUBV / SUPERFICIE,
         rbe_par_expl  = RBE / NBEX,
         prod_par_expl = PROD / NBEX)

## situation agricole par région

subve %>% 
  group_by(REGION) %>%
  summarise(SUBV_moy = mean(SUBV),
            RBE_moy  = mean(RBE),
            NBEX_moy = mean(NBEX),
            DENSITE  = mean(DENSITE)) -> agri_region
subve %>% 
  group_by(REGION) %>%
  summarise(across(c(SUBV,RBE,NBEX,DENSITE),mean))%>%
  rename(SUBV_mean= SUBV,
         RBE_mean = RBE,
         NBEX_mean= NBEX,
         DENSITE_mean = DENSITE) -> agri_region2

## si on fais tout d'un coup les indicateur par région:
subve %>%
  group_by(REGION) %>%
  summarise(across(c(SUBV, NBEX ,RBE, PROD, POPULATION, SUPERFICIE ),sum)) %>%
  mutate(subv_par_expl = SUBV / NBEX,
        subv_par_hab  = SUBV / POPULATION,
        subv_par_km2  = SUBV / SUPERFICIE,
        rbe_par_expl  = RBE / NBEX,
        prod_par_expl = PROD / NBEX) -> agri_region_indi
agri_region_indi


##gg plot test
library(ggplot2)


subve %>% #data
  
  ggplot + #mapping
  aes (x=NBEX,y=SUBV, color=REGION, fill=REGION) + #mapping: quels sont les lien entre variables (qui est var réponces, qui expli...)
  #size redire la taille
  geom_point () + # layers 
  geom_smooth(method = "lm") + # layers
  geom_density2d(size=0.2) + # layers
  
  #couleur se gère comme une echelle (scale)
  scale_color_discrete(palette = "Dark2") +
  scale_fill_discrete(palette = "Dark2") +
  #scale_x_continuous(limits=c(30,60)) + #scales
  #scale_y_continuous(limits=c(12,23))  + #scales
  
  #facet_grid(row=vars(species)) +# facet
  
  xlab("Nombre d'exploitation") + #obtion (légende axes)
  ylab("Montant subvention") +
  ggtitle("Repartition") +
  
  #coord_equal() #harmonise l'échel , bien pour variable de meme unité
  #coord_cartesian() +
  
  #theme(aspect.ratio=1/1.5) +# taille graphique
  theme_bw() #theme



### Productivité agricole par exploitation et contrainte relief
subve %>%
  mutate(prod_par_expl = PROD / NBEX) %>%
  
  ggplot +
  aes(x= MONT,y= prod_par_expl,color = REGION,size = DENSITE)+
  
  geom_point() +
  geom_smooth(method = "lm") +
  
  scale_color_discrete(palette = "Dark2") +
  scale_size_continuous(name = "Densité de population") +
  scale_color_discrete(name = "Région") +
  
  xlab("Part zone de montagne (%)") + 
  ylab("Production par exploitation") + 
  ggtitle("Productivité agricole par exploitation et contrainte relief") +
  
  theme_bw() 

### Productivité agricole par exploitation et contrainte relief par régions
subve %>%
  mutate(prod_par_expl = PROD / NBEX) %>%
  
  ggplot +
  aes(x= MONT,y= prod_par_expl,color = REGION,size = DENSITE)+
  
  geom_point() +
  geom_smooth(method = "lm") +
  
  scale_color_discrete(palette = "Dark2") +
  scale_size_continuous(name = "Densité de population") +
  scale_color_discrete(name = "Région") +
  
  facet_wrap(~REGION)
  
  xlab("Part zone de montagne (%)") + 
  ylab("Production par exploitation") + 
  ggtitle("Productivité agricole par exploitation et contrainte relief") +
  
  theme_bw() 
  
  
#### Typologie des régions agricoles françaises
  
agri_region_indi %>%
  ggplot +
  aes(x = subv_par_expl,y = rbe_par_expl,color = REGION, size = NBEX) +
  
  geom_point(alpha = 0.5) +
 
  scale_size_continuous(name = "Nombre d'exploitations",range = c(6, 20)) +
  scale_color_viridis_d(name = "Région")+

  xlab("Subventions par exploitation") + 
  ylab("RBE par exploitation") + 
  ggtitle("Typologie des régions agricoles françaises") +
  
  theme_bw() 
  
### Subventions et performance économique par région
agri_region_indi %>%
  ggplot+
  aes(x = subv_par_expl,y = reorder(REGION, subv_par_expl),fill = rbe_par_expl) +
  
  geom_col() +  
  
  scale_fill_gradient(low = "#fde0dd",high = "#c51b8a",name = "RBE par exploitation") +
  
  xlab("Subventions par exploitation") +
  ylab("Région françaises") +
  ggtitle("Subventions et performance économique par région") +
  
  theme_minimal(base_size = 13) 
