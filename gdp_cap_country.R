library(tidyverse)
options(stringsAsFactors = F)

df <- read.csv(file.path(getwd(), 'API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_9984840.csv'), skip = 4)
regions <- read.csv(file.path(getwd(), 'Metadata_Country_API_NY.GDP.PCAP.PP.CD_DS2_en_csv_v2_9984840.csv'), fileEncoding="UTF-8-BOM")

regions <- regions %>% filter(Region == "Europe & Central Asia")
df <- df %>% filter(Country.Code %in% regions$Country.Code)

df <- df %>% select(Country.Name, X1997, X2017) %>% filter(complete.cases(.))

df$rank_2017 <- rank(-df$X2017)
df$rank_1997 <- rank(-df$X1997)

df <- df %>%
  mutate(change_color = ifelse(rank_1997 - rank_2017 < 0, 'blue', 'red')) %>%
  mutate(change_color = ifelse(abs(rank_1997 - rank_2017) <= 2, 'grey', change_color))

ggplot(data = df, aes(x = rank_1997, y = rank_2017, color = change_color)) + 
  theme_bw() +
  geom_point() + 
  geom_text(aes(label=Country.Name),hjust=0, vjust=0) +
  theme(legend.position = 'none') +
  scale_x_reverse(breaks = seq(1, 50, 2)) +
  scale_y_reverse(breaks = seq(1, 50, 2)) +
  geom_abline(slope=1, intercept=0)
