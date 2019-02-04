


### organize data from separate sessions into one file

library(tidyverse)

p41_1 <- readr::read_csv("data/parl-41-1-raw.txt") %>% 
  add_column(parl = "41-1", .before = 1)

p41_2 <- readr::read_csv("data/parl-41-2-raw.txt") %>% 
  add_column(parl = "41-2", .before = 1)

p42_1 <- readr::read_csv("data/parl-42-1-raw.txt") %>% 
  add_column(parl = "42-1", .before = 1)

dat <- rbind(p41_1, p41_2, p42_1)

# add padding where Parliament took long breaks (e.g. during election)
date_pad1 <- seq.Date(as.Date("2013-09-01"), as.Date("2013-09-12"), by = 1)
date_pad2 <- seq.Date(as.Date("2015-09-01"), as.Date("2015-10-10"), by = 1)
date_pad <- c(date_pad1, date_pad2)

# unique dates
df_date <- dat %>% 
  select(date) %>% 
  unique() %>%
  rbind(tibble(date = date_pad)) %>% 
  arrange(date) %>% 
  mutate(date_num = as.integer(as.factor(date)))

dat_full <- left_join(dat, df_date, by = "date")

# write to file
write.csv(dat_full, file = "data/parl_full.csv", row.names = FALSE)


