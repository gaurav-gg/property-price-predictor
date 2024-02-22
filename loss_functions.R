############  loss functions

logcosh <- function(preds,dtrain){
  labels <- getinfo(dtrain,"label")
  x <- labels - preds
  grad <- tanh(x)
  hess <- 1/(cosh(x)^2)
  
  return(list(grad = grad, hess = hess))
}

huberloss <- function(preds,dtrain){
  labels <- getinfo(dtrain,'label')
  x <- labels - preds
  h <- 1
  scale <- 1 + (x/h)^2
  scale_sqrt <- sqrt(scale)
  grad <- x/scale_sqrt
  hess <- 1/scale/scale_sqrt
  return(list(grad = grad,hess= hess))
}

fairloss <- function(preds,dtrain){
  labels <- getinfo(dtrain,"label")
  x <- labels - preds
  c = 1
  den = abs(x) + c
  grad <- c*x/den
  hess <- c*c / (den^2)
  return(list(grad=grad,hess=hess))
}

logregobj <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  preds <- 1/(1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  return(list(grad = grad, hess = hess))
}

########error function
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- mean(abs(labels-preds)*2/(abs(labels)+abs(preds)))
  return(list(metric = "error", value = err))
}

