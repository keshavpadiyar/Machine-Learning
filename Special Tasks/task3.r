set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation

# plot(trva)
# plot(tr)
# plot(va)

w_j <- runif(10, -1, 1)
b_j <- runif(10, -1, 1)
w_k <- runif(10, -1, 1)
b_k <- runif(1, -1, 1)
l_rate <- 1/nrow(tr)^2
n_ite = 25000
error <- rep(0, n_ite)
error_va <- rep(0, n_ite)
for(i in 1:n_ite) {
  # error computation: Your code here

  a_j <- (as.matrix(tr$Var))%*% (w_j) + matrix(rep(b_j,nrow(tr)),ncol=length(b_j),byrow=T)

  #z_j <- 1/(1+exp(-a_j))

  z_j <- tanh(a_j)

  a_k <- (z_j %*% w_o) + matrix(rep(b_k,nrow(tr)),ncol=length(b_k),byrow=T)

  error[i] <- sum((tr$Sin - as.numeric(a_k))^2)



  a_j <- (as.matrix(va$Var))%*% (w_j) + matrix(rep(b_j,nrow(va)),ncol=length(b_j),byrow=T)

  #z_j <- 1/(1+exp(-a_j))

  z_j <- tanh(a_j)

  a_k <- (z_j %*% w_o) + matrix(rep(b_k,nrow(va)),ncol=length(b_k),byrow=T)

  error_va[i] <- sum((va$Sin - as.numeric(a_k))^2)

 cat("i: ", i, ", error: ", error[i]/2, ", error_va: ", error_va[i]/2, "\n")

  flush.console()
  for(n in 1:nrow(tr)) {
    # forward propagation: Your code here

    w_i <- w_j

    w_o <- w_k

    b_i <- b_j #matrix(rep(b_j,nrow(tr)),ncol=length(b_j),byrow=T)

    b_o <- b_k#matrix(rep(b_k,nrow(tr)),ncol=length(b_k),byrow=T)

    x <- tr[n,1]

    a_j <-(w_i * x)+ b_i

    z_j <- tanh(a_j)

    #z_j <- 1/(1+exp(-a_j))

    a_k <- (z_j %*% w_o) + b_o

    # Backpropogation

    y_k <- as.vector(a_k)

    delta_k <- (as.numeric(y_k-tr[n,2]))

    delta_j <-  (1-z_j^2) *  (w_o * delta_k)

    #delta_j <-  (z_j * (1-z_j)) *  (w_o * delta_k)

    E_wi <-delta_j * x

    E_wo <- z_j * delta_k

    w_ni <- w_j - (l_rate * E_wi)

    w_no <- w_k - (l_rate * E_wo)

    w_j <- w_ni

    w_k <- w_no

    b_j <- b_i - (l_rate * delta_j)
    b_k <- b_o - (l_rate * delta_k)


    # backward propagation: Your code here
  }




}
plot(y = tr[,2], x = tr[,1], col = "blue")
points(y = y_k, x = tr[,1], col="red")


# print final weights and errors
w_j
b_j
w_k
b_k
plot(error/2, ylim=c(0, 5))
points(error_va/2, col = "red")


# plot prediction on training data

a_j <- (as.matrix(tr$Var))%*% (w_j) + matrix(rep(b_j,nrow(tr)),ncol=length(b_j),byrow=T)

#z_j <- 1/(1+exp(-a_j))

z_j <- tanh(a_j)

y_k <- (z_j %*% w_k) + matrix(rep(b_k,nrow(tr)),ncol=length(b_k),byrow=T)

pred <- cbind(tr$Var,y_k)

plot(pred)

points(tr, col = "red")
# plot prediction on validation data
a_j <- (as.matrix(va$Var))%*% (w_j) + matrix(rep(b_j,nrow(va)),ncol=length(b_j),byrow=T)

#z_j <- 1/(1+exp(-a_j))

z_j <- tanh(a_j)

y_k <- (z_j %*% w_k) + matrix(rep(b_k,nrow(va)),ncol=length(b_k),byrow=T)

pred <- cbind(va$Var,y_k)

plot(pred)

points(va, col = "red")





