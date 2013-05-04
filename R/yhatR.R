library(httr)
library(rjson)
library(plyr)


YHAT_URL <- "http://api.yhathq.com/"
YHAT_URL = "http://0.0.0.0:8000/"
# YHAT_URL = "http://127.0.0.1:5000/"
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

yhat.login <- function(username, apikey) {
  AUTH <<- c(
    username = username,
    apikey = apikey
  )
}

yhat.get <- function(endpoint, query=c()) {
  if (length(AUTH)==0) {
    stop("You must login. Execute yhat.login(username, apikey).")
  }
  query <- c(query, AUTH)
  query <- paste(names(query), query, collapse="&", sep="=")
  url <- paste(YHAT_URL, endpoint, "?", query, sep="")
  GET(url)
}

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

yhat.show_models <- function() {
  rsp <- yhat.get("showmodels")
  js <- content(rsp)
  ldply(js$models, data.frame) 
}

yhat.predict_raw <- function(model_name, version, data) {
  rsp <- yhat.post("predict", c(model = model_name, 
                                version = version),
                   data = data)
  content(rsp)
}

yhat.predict <- function(model_name, version, data) {
  raw_rsp <- yhat.predict_raw(model_name, version, data)
  data.frame(raw_rsp$prediction)
}

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

#yhat.login("greg", "fCVZiLJhS95cnxOrsp5e2VSkk0GfypZqeRCntTD1nHA")
rsp <- yhat.deploy("gregsModel")
yhat.show_models()

####sample user code####


yhat.predict_raw("MySMSClassifier", 12, "sex boner this shit")
yhat.predict("MySMSClassifier", 12, "sex boner this shit")
yhat.predict("MySMSClassifier", 12, "hello")

x <- 1:100
xsq <- x^2
y <- sin(x) + runif(length(x), -.3, .3)
data <- data.frame(x=x, xsq=xsq, y=y)
(fit <- randomForest(y ~ x + xsq, data=data))
(fit <- lm(y ~ x + xsq, data=data))

model.transform <- function(df) {
  df$xsq <- df$x^2
  df
}

model.predict <- function(x) {
  p <- predict(fit, newdata=x)
  data.frame("prediction"=p)
}

model.require <- function() {
  library("randomForest")
}


model.predict(model.transform(data.frame(x=2)))

yhat.deploy("gregsModel")
rsp <- yhat.predict_raw("gregsModel", 16, data.frame(x=c(-10, 5)))
yhat.predict("gregsModel", 16, data.frame(x=c(2, 5)))


