library(httr)
library(rjson)
library(plyr)


YHAT_URL <- "http://api.yhathq.com/"
YHAT_URL = "http://0.0.0.0:8000/"

AUTH <- c(
  username = "rtest",
  apikey = "abcd1234"
)
AUTH <- c(
)

yhat.login <- function() {
  username <- scan(, what="")
  apikey <- scan(, what="")
  yhat.login(username, apikey)
}

#' A function for logging into Yhat's api.
#' 
#' @param username Your Yhat username
#' @param apikey Your Yhat apikey
#' 
#' @examples
#' yhat.login("hmardukas", "abcd1234")
yhat.login <- function(username, apikey) {
  AUTH <<- c(
    username = username,
    apikey = apikey
  )
}

#' Private function for performing a GET request
yhat.get <- function(endpoint, query=c()) {
  if (length(AUTH)==0) {
    stop("You must login. Execute yhat.login(username, apikey).")
  }
  query <- c(query, AUTH)
  query <- paste(names(query), query, collapse="&", sep="=")
  url <- paste(YHAT_URL, endpoint, "?", query, sep="")
  GET(url)
}

#' Private function for performing a POST request
yhat.post <- function(endpoint, query=c(), data) {
  if (length(AUTH)==0) {
    stop("You must login. Execute yhat.login(username, apikey).")
  }
  query <- c(query, AUTH)
  query <- paste(names(query), query, collapse="&", sep="=")
  url <- paste(YHAT_URL, endpoint, "?", query, sep="")
  POST(url, add_headers("Content-Type"="application/json"),
       body = toJSON(list(
         data = data)
        )
  )
}
#' Shows which models you have deployed on Yhat.
#' 
#' This function queries the Yhat API and finds the models that have been deployed
#' for your account.
#' 
#' @examples
#' yhat.show_models()
#' # some output here
#'    username className                  name version
#' 1      greg                 MySMSClassifier       6
#' 2      greg                 MySMSClassifier       7
#' 3      greg                 MySMSClassifier       8
#' 4      greg                 MySMSClassifier       9
yhat.show_models <- function() {
  rsp <- yhat.get("showmodels")
  js <- content(rsp)
  js <- lapply(js$models, function(model) {
    if (is.null(model$clasName)) {
      model$className <- ""
    }
    model
  })
  ldply(js, data.frame)
}

#' Calls Yhat's REST API and returns a JSON document containing both the prediction
#' and associated metadata.
#' 
#' @param model_name the name of the model you want to call
#' @param version the version number of the model you want to call
#' @param data input data for the model
#' 
#' @examples
#' yhat.predict_raw("irisModel", 1, iris) 
yhat.predict_raw <- function(model_name, version, data) {
  rsp <- yhat.post("predict", c(model = model_name, 
                                version = version),
                   data = data)
  content(rsp)
}
#' Make a prediction using Yhat.
#' 
#' This function calls Yhat's REST API and returns a response formatted as a
#' data frame.
#' 
#' @param model_name the name of the model you want to call
#' @param version the version number of the model you want to call
#' @param data input data for the model
#' 
#' @keywords predict
#' @examples
#' yhat.predict("irisModel", 1, iris) 
yhat.predict <- function(model_name, version, data) {
  raw_rsp <- yhat.predict_raw(model_name, version, data)
  data.frame(raw_rsp$prediction)
}

#' Deploy a model to Yhat's servers
#' 
#' This function takes model.transform and model.predict and creates
#' a model on Yhat's servers which can be called from any programming language
#' via Yhat's REST API (see \code{\link{yhat.predict}}).
#' 
#' @param model_name name of your model
#' @keywords deploy
#' @examples
#' iris$Sepal.Width_sq <- iris$Sepal.Width^2
#' fit <- glm(I(species)=="virginica" ~ ., data=iris)
#' model.transform <- function(df) {
#'  df$Sepal.Width_sq <- df$Sepal.Width^2
#'  df
#' }
#' model.predict <- function(df) {
#'  data.frame("prediction"=predict(fit, df, type="response"))
#' } 
#'
#' yhat.deploy("irisModel")
yhat.deploy <- function(model_name) {
  if (length(AUTH)==0) {
    stop("You must login. execute yhat.login(username, apikey).")
  }
  image_file <- ".yhatdeployment.img"
  save.image(image_file)
  query <- AUTH
  query <- paste(names(query), query, collapse="&", sep="=")
  url <- paste(YHAT_URL, "model/R", "?", query, sep="")
  
  rsp <- POST(url,
       body=list(
         "model_image" = upload_file(image_file),
         "modelname" = model_name
         )
  )
  unlink(image_file)
  js <- content(rsp)
  data.frame(js)
}

# 
# ####sample user code####
# #yhat.login("greg", "fCVZiLJhS95cnxOrsp5e2VSkk0GfypZqeRCntTD1nHA")
# rsp <- yhat.deploy("gregsModel")
# yhat.show_models()
# 
# yhat.predict_raw("MySMSClassifier", 12, "sex boner this shit")
# yhat.predict("MySMSClassifier", 12, "sex boner this shit")
# yhat.predict("MySMSClassifier", 12, "hello")
# 
# x <- 1:100
# xsq <- x^2
# y <- sin(x) + runif(length(x), -.3, .3)
# data <- data.frame(x=x, xsq=xsq, y=y)
# (fit <- randomForest(y ~ x + xsq, data=data))
# (fit <- lm(y ~ x + xsq, data=data))
# 
# model.transform <- function(df) {
#   df$xsq <- df$x^2
#   df
# }
# 
# model.predict <- function(x) {
#   p <- predict(fit, newdata=x)
#   data.frame("prediction"=p)
# }
# 
# model.require <- function() {
#   library("randomForest")
# }
# 
# 
# model.predict(model.transform(data.frame(x=2)))
# 
# yhat.deploy("gregsModel")
# rsp <- yhat.predict_raw("gregsModel", 16, data.frame(x=c(-10, 5)))
# yhat.predict("gregsModel", 16, data.frame(x=c(2, 5)))
# 
# 
