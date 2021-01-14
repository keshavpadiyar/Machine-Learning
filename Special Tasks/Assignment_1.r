##  Library Decleration
library("dplyr")
library("glmnet")
library("boot")

park <- read.csv("parkinsons.csv")
park <- park %>% select(-total_UPDRS) %>% scale() %>% data.frame() # Ignoring the total_UPDRS Field

lambda <- seq(0.05,1,by=0.05)# Generating Lambda

## Function f1
# Function Input: Data, Sampled Ids - Ind, Lambda, weakness parameter- alpha
# returns matrix of lasso coefficients

f1 <- function(data,ind,lambda,alpha){

  data1 <- data[ind,]

  x_train <- as.matrix(data1 %>%select(-motor_UPDRS))

  y_train <- data1$motor_UPDRS

  # generating w_k
  w <- runif(ncol(x_train),alpha,1)

  m1 <- glmnet(x=x_train,y=y_train,lambda=lambda,alpha=1,
               family = "gaussian", panalty.factor = 1/w)

  return(matrix(as.vector(t(coef(m1))),ncol=ncol(x_train)+1,nrow=1))

}# end function f1

## Execute the randomized lasso for different values of lambda
for (l in 1:length(lambda)){

  m2 <- boot(park, f1, R =100,lambda=lambda[l],alpha=0.5)

  coefs<-m2$t

  # Calculate the probabilities of significance for all variables
  if (l ==1){

    max_probabilities <- matrix(colSums
                                (apply(coefs,2
                                       ,function(x) ifelse(x!=0,1,0)))/100,nrow=1)

  }else{

    max_probabilities <- rbind(max_probabilities
                               ,matrix(colSums(apply(coefs,2
                                                     ,function(x) ifelse(x!=0,1,0)))/100,nrow=1))
  }

}

max_probabilities <- max_probabilities[,-1] #ignoring the intercept

# Assigning column and row names to the probability matrix
rownames(max_probabilities) <- lambda

colnames(max_probabilities) <- colnames(park %>% select(-motor_UPDRS ))

print(max_probabilities)

# get the variable wise maximum probability
max_probabilities <- matrix(apply(max_probabilities,2,max),nrow=1,
                            dimnames = list("probability",colnames(park %>% select(-motor_UPDRS ))))

print(max_probabilities)


# Select the variables whose probabilities are greater than 0.7
stable <- max_probabilities[,which(apply(max_probabilities,2,function(x) x>0.7))]

print(stable)
