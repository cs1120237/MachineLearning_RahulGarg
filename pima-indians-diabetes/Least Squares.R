library(MASS)

data <- as.matrix(read.table("Data/pima-indians-diabetes.csv", sep = ","))
b <- data[, dim(data)[2]]
A <- data[, c(1:dim(data)[2] - 1)]
A <- cbind(rep(1, length(b)), A)
At <- t(A)
AtA <- At %*% A
AtA_Inv <- ginv(AtA)
ans <- AtA_Inv %*% At %*% b