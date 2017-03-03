sigmoid <- function(z) {
  return(1/(1 + exp(-z)))
}

sgd <- function(X, y, learning_rate, epochs) {

  n_features = ncol(X) 

  w = rep(0, n_features) 
  w0 = 0 
  
    for (epoch in 1:epochs) {
    error = 0 
    
        for (i in 1:nrow(X)) {

            pred = sigmoid(X[i] * w)
            print(pred)
            error = error + (pred - y[i])^2 
            
            print(error)
            w0 = w0 + learning_rate * pred * (1 - pred) * error 
            for (j in 1:n_features) { 
              print(w)
              print(X[i, j])
              
              w[j] = w[j] + learning_rate * pred * (1 - pred) * error * X[i, j] 
            }

        }
    
    }
    
    return(w) 
    
} 


X <- read.csv('linsep-traindata.csv') 
y <- read.csv('linsep-trainclass.csv')
y[y == -1] = 0 
sgd(X, y, 0.1, 10)