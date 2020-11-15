#amelia
library(tidyverse)
avocado <- read_csv("avocado_data.csv")
mean_prices <- avocado %>%
  subset(region %in% c("Chicago", "Portland", "Denver")) %>%
  group_by(region) %>%
  mutate(mean_price = mean(price_per))

ggplot(mean_prices, aes(x=region, y=mean_price)) +
  geom_col() +
  labs(x="region",
       y="mean price ($)",
       title="Amelia")

geom_