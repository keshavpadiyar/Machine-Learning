library("dplyr")
library("glmnet")
library("boot")
setwd("C:\\Users\\kpadj\\Documents\\Study\\Liu\\ML\\Labs\\Special Tasks")
park <- read.csv("parkinsons.csv")
park <- park %>% select(-total_UPDRS) %>% scale() %>% data.frame()
lambda <- seq(0.05,1,by=0.05)


f1 <- function(data,ind,lambda,alpha){

  data1 <- data[ind,]

  x_train <- as.matrix(data1 %>%select(-motor_UPDRS))

  #print(colnames(x_train))

  y_train <- data1$motor_UPDRS

  w <- runif(ncol(x_train),alpha,1)

  m1 <- glmnet(x=x_train,y=y_train,lambda=lambda,alpha=1,
               family = "gaussian", panalty.factor = 1/w)

  return(matrix(as.vector(t(coef(m1))),ncol=ncol(x_train)+1,nrow=1))

}

for (l in 1:length(lambda)){

  print(l)

  m2 <- boot(park, f1, R =100,lambda=lambda[l],alpha=0.5)

  coefs<-m2$t

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

max_probabilities <- max_probabilities[,-1]

rownames(max_probabilities) <- lambda

colnames(max_probabilities) <- colnames(park %>% select(-motor_UPDRS ))

print(max_probabilities)

max_probabilities <- matrix(apply(max_probabilities[-1],2,max),nrow=1,
                            dimnames = list("probability",colnames(park %>% select(-motor_UPDRS ))))
print(max_probabilities)
