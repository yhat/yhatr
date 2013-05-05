pkgname <- "yhatr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('yhatr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("yhat.deploy")
### * yhat.deploy

flush(stderr()); flush(stdout())

### Name: yhat.deploy
### Title: Deploy a model to Yhat's servers
### Aliases: yhat.deploy
### Keywords: deploy

### ** Examples

iris$Sepal.Width_sq <- iris$Sepal.Width^2
fit <- glm(I(Species)=="virginica" ~ ., data=iris)

model.require <- function() {
 # require("randomForest")
}

model.transform <- function(df) {
 df$Sepal.Width_sq <- df$Sepal.Width^2
 df
}
model.predict <- function(df) {
 data.frame("prediction"=predict(fit, df, type="response"))
}
yhat.login("rtest", "abcd1234")
yhat.deploy("irisModel")



cleanEx()
nameEx("yhat.login")
### * yhat.login

flush(stderr()); flush(stdout())

### Name: yhat.login
### Title: A function for logging into Yhat's api.
### Aliases: yhat.login

### ** Examples

yhat.login("hmardukas", "abcd1234")



cleanEx()
nameEx("yhat.predict")
### * yhat.predict

flush(stderr()); flush(stdout())

### Name: yhat.predict
### Title: Make a prediction using Yhat.
### Aliases: yhat.predict
### Keywords: predict

### ** Examples

yhat.predict("irisModel", 1, iris)



cleanEx()
nameEx("yhat.predict_raw")
### * yhat.predict_raw

flush(stderr()); flush(stdout())

### Name: yhat.predict_raw
### Title: Calls Yhat's REST API and returns a JSON document containing
###   both the prediction and associated metadata.
### Aliases: yhat.predict_raw

### ** Examples

yhat.predict_raw("irisModel", 1, iris)



cleanEx()
nameEx("yhat.scaffolding")
### * yhat.scaffolding

flush(stderr()); flush(stdout())

### Name: yhat.scaffolding
### Title: Quick function for setting up a basic scaffolding of functions
###   for deploying on Yhat.
### Aliases: yhat.scaffolding

### ** Examples

yhat.scaffolding()



cleanEx()
nameEx("yhat.show_models")
### * yhat.show_models

flush(stderr()); flush(stdout())

### Name: yhat.show_models
### Title: Shows which models you have deployed on Yhat.
### Aliases: yhat.show_models

### ** Examples

yhat.show_models()
# some output here
#    username className                  name version
# 1      greg                 MySMSClassifier       1
# 2      greg                 MySMSClassifier       2
# 3      greg                 MySMSClassifier       3
# 4      greg                 MySMSClassifier       4



cleanEx()
nameEx("yhatR-package")
### * yhatR-package

flush(stderr()); flush(stdout())

### Name: yhatR
### Title: A package for deploying statistical models on Yhat
### Aliases: yhatR yhatR
### Keywords: package

### ** Examples

# build a quick model
iris$Sepal.Width_sq <- iris$Sepal.Width^2
fit <- glm(I(Species)=="virginica" ~ ., data=iris)


model.require <- function() {
  require("someLibrary")
}

model.transform <- function(df) {
  df$Sepal.Width_sq <- df$Sepal.Width^2
  df
}

model.predict <- function(df) {
  data.frame("prediction"=predict(fit, df, type="response"))
} 
yhat.deploy("irisModel")




### * <FOOTER>
###
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
