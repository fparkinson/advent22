##Part 1
sub_range <- c(length(advent4input$ranges))
for (i in seq(length(advent4input$ranges))) {
  
  ranges <- advent4input$ranges[i]
  
  ranges <- gsub(",","-",ranges)
  ranges <- strsplit(ranges,split="-")
  
 range1_min <- as.numeric(ranges[[1]][1])
 range1_max <- as.numeric(ranges[[1]][2])
 range2_min <- as.numeric(ranges[[1]][3])
 range2_max <- as.numeric(ranges[[1]][4])
  
 sub_range[i] <- case_when(range2_min >= range1_min & range2_max <= range1_max ~ 1,
                           range1_min >= range2_min & range1_max <= range2_max ~ 1,
                           TRUE ~ 0)
}
Reduce('+',sub_range)


##part 2
sub_range <- c(length(advent4input$ranges))
for (i in seq(length(advent4input$ranges))) {
  
  ranges <- advent4input$ranges[i]
  
  ranges <- gsub(",","-",ranges)
  ranges <- strsplit(ranges,split="-")
  
  range1_min <- as.numeric(ranges[[1]][1])
  range1_max <- as.numeric(ranges[[1]][2])
  range2_min <- as.numeric(ranges[[1]][3])
  range2_max <- as.numeric(ranges[[1]][4])
  
  sub_range[i] <- case_when(range1_min >= range2_min & range1_max <= range2_max ~ 1,
                            range2_min >= range1_min & range2_max <= range1_max ~ 1,
                            range1_min >= range2_min & range1_min <= range2_max ~ 1,
                            range2_min >= range1_min & range2_min <= range1_max ~ 1,
                            TRUE ~ 0)
}
Reduce('+',sub_range)