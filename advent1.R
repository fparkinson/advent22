#I replaced any empty in csv with 0
#cumulative add calories except when 0
calories <- advent1input$Calories
calories_cumulative <- c(length(calories))
for (i in seq(length(calories))) {
  if (i == 1) {
    calories_cumulative[i] <-calories[1]
  }
  else{
  calories_cumulative[i] <- case_when(calories[i] == 0 ~ 0,
                                      TRUE ~ calories[i] + calories_cumulative[i-1])
}}

calories_cumulative <- calories_cumulative %>%
  sort(decreasing=TRUE) %>%
  head(3) %>%
  cumsum()
