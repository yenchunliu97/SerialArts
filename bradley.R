library(dplyr)
library(BradleyTerry2)

dat = read.csv("data.csv", sep=",")
dat$preference = ifelse(dat$preference=="left",1,0)

all_designs <- sort(unique(c(dat$left_design, dat$right_design)))

dat <- dat %>%
  mutate(
    player1 = factor(left_design, levels = all_designs),
    player2 = factor(right_design, levels = all_designs)
  )

summary_df <- dat %>%
  group_by(player1, player2) %>%
  summarise(
    A_wins = sum(preference == 1),
    B_wins = sum(preference == 0),
    n = n(),
    .groups = "drop"
  )

model = BTm(cbind(A_wins, B_wins), player1, player2, data=summary_df)

