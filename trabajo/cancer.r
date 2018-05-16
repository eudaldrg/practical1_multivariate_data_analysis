data <- read.csv("./cancer.csv", header = F)
colnames(data) <- c("Id", "Thickness", "Size", "Shape", "Adhesion", "SingleCellSize", "Nuclei", "Chromatin", "Nucleoi", "Mitoses", "Class")
for (i in seq(from = 1, to = nrow(data))) {
  if (i <= 367) {
    data$Day[i] <- 1
  } else if (i <= 367 + 70) {
    data$Day[i] <- 2
  } else if (i <= 367 + 70 + 31) {
    data$Day[i] <- 3
  } else if (i <= 367 + 70 + 31 +  17){
    data$Day[i] <- 4
  } else if (i <= 367 + 70 + 31 +  17 + 48){
    data$Day[i] <- 5
  } else if (i <= 367 + 70 + 31 +  17 + 48 + 49){
    data$Day[i] <- 6
  } else if (i <= 367 + 70 + 31 +  17 + 48 + 49 + 31){
    data$Day[i] <- 7
  } else if (i <= 367 + 70 + 31 +  17 + 48 + 49 + 31 + 86){
    data$Day[i] <- 8
  }
}
day_1[91,]
day_2$Id == 560680
day_1 <- data[data$Day == 1,]
day_2 <- data[data$Day == 2,]
day_3 <- data[data$Day == 3,]
day_4 <- data[data$Day == 4,]
day_5 <- data[data$Day == 5,]
sum(day_1$Id %in% day_2$Id)
sum(day_1$Id %in% day_3$Id)
sum(day_1$Id %in% day_4$Id)
data$Day
data$Class==2