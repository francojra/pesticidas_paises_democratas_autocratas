
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





