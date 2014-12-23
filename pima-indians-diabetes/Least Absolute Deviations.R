library(boot)

data <- as.matrix(read.table("Data/pima-indians-diabetes.csv", sep = ","))
data <- data[1:100, ]

u <- rbind(-1 * diag(dim(data)[1]), -1 * diag(dim(data)[1]))
a0 <- rep(-1, dim(data)[1])

A <- data[1:dim(data)[1], c(1 : dim(data)[2] - 1)]
A <- cbind(a0, A)
A <- cbind(A, -1 * A)

A <- rbind(A, -1 * A)

y <- data[1:dim(data)[1], dim(data)[2]]
y <- append(-1 * y, y)

a <- append(rep(1, dim(data)[1]), rep(0, 2 + 2 * (dim(data)[2] - 1)))

A <- cbind(u, A)

A2 <- -1 * A[1: dim(data)[1], ]
b2 <- -1 * y[1:dim(data)[1]]

A1 <- A[(dim(data)[1] + 1) : (dim(data)[1] * 2), ]
b1 <- y[(dim(data)[1] + 1) : (dim(data)[1] * 2)]

result <- simplex(a, A1, b1, A2, b2)

solution <- result$soln[(dim(data)[1] + 1): (dim(data)[1] + dim(data)[2] * 2)]
solution <- solution[1 : (dim(data)[2])] - solution[(dim(data)[2] + 1) : (dim(data)[2] * 2)]

names(solution) <- sub("x", "", names(solution))
names(solution) <- sapply(names(solution), as.numeric)
names(solution) <- sapply(names(solution), function(x){paste("x", (as.numeric(x) - dim(data)[1] - 1), sep = "")})
solution