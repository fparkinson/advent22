#formatting for number scores
advent <- advent2input %>%
  mutate(player = case_when(player == "A" ~ 1,player == "B" ~ 2,player == "C" ~ 3),
         strategy = case_when(strategy == "X" ~ 1,strategy == "Y" ~ 2,strategy == "Z" ~ 3))

player <- advent$player
strategy <- advent$strategy

## Part 1
score <- c(length(strategy))
for (i in seq(length(strategy))) {
  winlose_score <- case_when(strategy[i]== 3 & player[i] == 1  ~ 0,
                             strategy[i]== 1 & player[i] == 3  ~ 6,
                             strategy[i] == player[i] ~ 3,
                             strategy[i] > player[i] ~ 6,
                             strategy[i] < player[i] ~ 0)
  
  score[i] <- strategy[i] + winlose_score
}
Reduce('+',score)

## Part 2
new_score <- c(length(strategy))
for (i in seq(length(strategy))) {
  
  winlose_score <- case_when(strategy[i]== 1 ~ 0,
                             strategy[i]== 2 ~ 3,
                             strategy[i]== 3 ~ 6)
  
  strategy_score <- case_when(winlose_score == 6 & player[i] == 3  ~ 1,
                              winlose_score == 0 & player[i] == 1  ~ 3,
                              winlose_score == 3 ~ player[i],
                              winlose_score == 6 ~ player[i]+1,
                              winlose_score == 0 ~ player[i]-1)
  
  new_score[i] <- strategy_score + winlose_score
}
Reduce('+',new_score)
