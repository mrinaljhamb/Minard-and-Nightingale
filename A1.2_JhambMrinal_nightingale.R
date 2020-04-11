library(knitr)
library(HistData)
library(tidyverse)
library(magrittr)
library(grid)
library(gridExtra)
kable(Nightingale)
Night <- Nightingale %>%
  as_tibble %>%
  subset(select = c(1, 8:10))
kable(Night)
Night %<>%
  gather(key = "Cause", value = "Deaths", -Date) %>%
  mutate(Month = gl(12, 1, 72, labels = month.name[c(4:12, 1:3)])) %>%
  mutate(Regime = gl(2, 12, 72, labels = c("Before", "After"), ordered = TRUE)) 
kable(Night)
Night$Cause %<>% 
  sub("\\.rate", "", .)
Night$Cause %<>% 
  factor(levels = c("Disease", "Wounds", "Other"))
Night %>%
  kable(align = c("c", "c", "r", "c", "c"))

cxc_b <- ggplot(data = Night,
                aes(x = factor(Date), y = Deaths, fill = Cause)) +
  geom_bar(width = 0.8, stat = "identity", position = "stack", colour = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks.x = element_blank(),
        legend.position = c(0.8, 0.8)) +
  scale_fill_brewer(type = "qual", palette = "Pastel1")
cxc_b
cxc1 <- ggplot(data = Night %>% subset(Regime == "Before"),
               aes(x = Month, y = Deaths, fill = Cause)) +
  geom_bar(width = 1, stat = "identity", position = "stack", colour = "black") +
  scale_y_sqrt() +
  coord_polar(start = 3 * pi / 2)
cxc1
cxc2 <- ggplot(data = Night %>% subset(Regime == "After"),
               aes(x = Month, y = Deaths, fill = Cause)) +
  geom_bar(width = 1, stat = "identity", position = "stack", colour = "black") +
  scale_y_sqrt() +
  coord_polar(start = 3 * pi / 2)
cxc2

Regime_lab <- c("Before", "After")
names(Regime_lab) <- c("Before", "After")
cxc_f <- ggplot(data = Night,
                aes(x = Month, y = Deaths, fill = Cause)) +
  geom_bar(width = 1, stat = "identity", position = "stack", colour = "black") +
  scale_y_sqrt() +
  scale_fill_brewer(type = "qual", palette = "Pastel2") +
  facet_grid(. ~ Regime, scales = "fixed", labeller = labeller(Regime = Regime_lab)) +
  coord_polar(start = 3 * pi / 2)
cxc_f
  
