
# Pesticidas em países democratas e autocratas ---------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 07/11/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/pesticides ----------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Aplicação de pesticidas em quilogramas por hectare. 

### Uso de pesticidas por hectare em terra de cultivo.

# carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

pest <- read.csv("pesticide-use-per-hectare-of-cropland.csv")
view(pest)
names(pest)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

pest <- pest %>%
  select(-Code) %>%
  rename(uso_pest = Pesticides..total....00001357....Use.per.area.of.cropland...005159....kilograms.per.hectare) %>%
  view()

pest1 <- pest %>%
  filter(Entity %in% c("United States", "Japan", "Germany",
                       "Cuba", "China", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(uso_pest),
            sd = sd(uso_pest), n = n(),
            se = sd/sqrt(n)) %>%
  view()

pest2 <- pest %>%
  filter(Entity %in% c("United States", "Japan", "Germany",
                       "Cuba", "China", "North Korea")) %>%
  view()

pest3 <- pest %>%
  filter(Entity %in% c("United States", "China", "Brazil")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(pest1, aes(x = fct_reorder(Entity, media), 
                  y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.2, size = 0.8) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  scale_x_discrete(labels = c("Estados Unidos", "Alemanha",
                              "China", "Japão")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  labs(x = "Países", y = "Pesticidas (kg)") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(legend.position = "none", 
        axis.text = element_text(colour = "black"))

ggplot(pest2, aes(x = Year, y = uso_pest,
                  group = Entity, color = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499"),
                     labels = c("China", "Alemanha", 
                                "Japão", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Pesticidas (kg)",
       color = "Países") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(axis.text = element_text(colour = "black"))

ggplot(pest3, aes(x = Year, y = uso_pest,
                  group = Entity, color = Entity)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('#1B9E77', '#999999','#E69F00'),
                     labels = c("Brasil", "China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Pesticidas (kg)",
       color = "Países") +
  theme_light() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))
