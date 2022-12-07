## part 1

comm2 <- advent6$input
comm <- as.data.frame(strsplit(comm2,split=""))
colnames(comm)[1] <- "comms"
check <- c(length(comm$comms)-4)
for (i in seq(4,length(comm$comms))) {
  
  x <- as.data.frame(c(comm[i-3,],comm[i-2,],comm[i-1,],comm[i,]))
  colnames(x)[1] <- "latest4comms"
  
  check[i] <- case_when(sum(duplicated(x$latest4comms)) == 0 ~ 1,
                        TRUE ~ 0)
}

match(1, check)

## part 2

check <- c(length(comm$comms)-4)
for (i in seq(4,length(comm$comms))) {
  
  x <- as.data.frame(c(comm[i-13,],comm[i-12,],comm[i-11,],comm[i-10,],
                       comm[i-9,],comm[i-8,],comm[i-7,],comm[i-6,],
                       comm[i-4,],comm[i-3,],comm[i-2,],comm[i-1,],comm[i,]))
  colnames(x)[1] <- "latest4comms"
  
  check[i] <- case_when(sum(duplicated(x$latest4comms)) == 0 ~ 1,
                        TRUE ~ 0)
}

match(1, check)