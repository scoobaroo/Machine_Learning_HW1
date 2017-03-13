library(MASS)
library(caret)
library(party)
x1 <-runif(20,min=4,max=6)
x2 <-runif(20,min=1,max=3)
a <- matrix(c(x1),ncol=1)
b <- matrix(c(x2),ncol=1)
x1 <- rbind(round(b),round(a))
a1 <- runif(20,min=3000,max=4500)
a2 <- runif(20,min=1500,max=3000)
a <- matrix(c(a1),ncol=1)
b <- matrix(c(a2),ncol=1)
x2 <- rbind(round(a),round(b))
a <- matrix(c(runif(20,min=0,max=1)),ncol=1)
b <- matrix(c(runif(20,min=-1,max=0)),ncol=1)
y <- rbind(ceiling(a),floor(b))
trainData <- data.frame(x1,x2,y)
X <- as.matrix(data.frame(x1,x2))
Y <- as.matrix(data.frame(y))
tX <- as.matrix(t(X))
invX <- ginv(tX %*% X)
pX <- invX %*% tX
wlin <- pX %*% Y

#plot(trainData$x1,trainData$x2,pch=as.integer(trainData$y),col=as.integer(trainData$y)+10)
trainData
reg <- lm(formula=y~x1+x2, data = trainData)

#fit <- train(y~., data=trainData, method="mlp")

perceptron <- function(X, Y)
{
  converged <- F
  
  W <- matrix(0, 1, 3)
  X <- cbind(rep(1, N), X)
  
  for (i in 1:10000)
  {
    h.X <- fn.sign(W %*% t(X))
    misclassified.mask <- as.vector(h.X) != as.vector(Y)
    if (sum(misclassified.mask) == 0)
    {
      converged <- T
      break
    }
    else
    {
      misclassified.points <- X[misclassified.mask, , drop = F]
      misclassified.points.Y <- Y[misclassified.mask]
      misclassified.point.index <- sample(dim(misclassified.points)[1], 1)
      misclassified.point <- misclassified.points[misclassified.point.index, , drop = F]
      misclassified.point.Y <- misclassified.points.Y[misclassified.point.index]
      W <- W + misclassified.point.Y %*% misclassified.point
    }
  }
  
  if (converged)
  {
    cat('Converged! Iteration ', i, ' , with final weight : ', W, '\n')
  }
  else
  {
    cat('DID NOT CONVERGE!\n')
  }
  
  return(W)
}

on.which.side <- function(line.separator, point)
{
  values <- (line.separator[2,1] - line.separator[1,1]) * (point[,2] - line.separator[1,2]) -
    (line.separator[2,2] - line.separator[1,2]) * (point[,1] - line.separator[1,1])
  return(fn.sign(values))
}

fn.sign <- function(values)
{
  return(ifelse(values > 0, 1, -1))
}

decision.boundary <- function(W)
{
  X1 <- c(0,6)
  X2 <- -(W[2]/W[3]) * X1 - W[1]/W[3]
  return(matrix(cbind(X1, X2), 2, 2))
}

line.separator <- matrix(runif(4, -1, 1), 2, 2)
line.separator[1,2] <- -2
line.separator[2,2] <- +2

# Initialize N random points, and Y
N <- 40
X <- as.matrix(trainData[,1:2])
Y <- as.matrix(trainData[,3])

# Run perceptron algorithm  
W <- perceptron(X, Y)

# Plot the points according to its actual class
plot(X, pch = Y, xlim = c(0,6), ylim = c(0,6000),xlab="exercise frequency",ylab="calories consumed/day")

# Plot ideal decision boundary
lines(abline(a=500,b=900), col="red")

# Plot learned decision boundary
lines(decision.boundary(W), col="blue")

title(paste("Perceptron training against N = ", N))
legend(x="topright", c("inital target function", "learned decision boundary"), lty=c(1,1), lwd=c(2.5,2.5),col=c("red","blue"))
legend(x="bottomright", legend=c("diabetes","no diabetes"), col=c("black", "blue"), lty=1, cex=1)
summary(reg)
