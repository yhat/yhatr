

yhat.config <- c(
  username="greg",
  apikey="blah",
  env="http://ec2-54-219-143-236.us-west-1.compute.amazonaws.com/deployer/"
)

model.require <- function() {
  
}

model.transform <- function(df) {
  df
}

model.predict <- function(df) {
  data.frame(geeting="hello!")
}

yhat.deploy("gregsgreeting")