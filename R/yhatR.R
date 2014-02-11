#' Private function for performing a GET request
#' 
#' @param endpoint /path for REST request
#' @param query url parameters for request
yhat.get <- function(endpoint, query=c()) {
  AUTH <- get("yhat.config")
  if (length(AUTH)==0) {
    stop("Please specify your account credentials using yhat.config.")
  }

  if ("env" %in% names(AUTH)) {
    url <- AUTH[["env"]]
    AUTH <- AUTH[!names(AUTH)=="env"]
    query <- c(query, AUTH)
    query <- paste(names(query), query, collapse="&", sep="=")
    url <- paste(url, endpoint, "?", query, sep="")
    httr::GET(url, httr::authenticate(AUTH["username"], AUTH["apikey"], 'basic'))
  } else {
    print("Please specify 'env' parameter in yhat.config.")
  }
}

#' Private function for performing a POST request
#' 
#' @param endpoint /path for REST request
#' @param query url parameters for request
#' @param data payload to be converted to raw JSON
yhat.post <- function(endpoint, query=c(), data) {
  AUTH <- get("yhat.config")
  if (length(AUTH)==0) {
    stop("Please specify your account credentials using yhat.config.")
  }

  if ("env" %in% names(AUTH)) {
    url <- AUTH[["env"]]
    AUTH <- AUTH[!names(AUTH)=="env"]
    query <- c(query, AUTH)
    query <- paste(names(query), query, collapse="&", sep="=")
    url <- paste(url, endpoint, "?", query, sep="")
    httr::POST(url, httr::authenticate(AUTH["username"], AUTH["apikey"], 'basic'),
               body = data)
  } else {
    print("Please specify 'env' parameter in yhat.config.")
  }
}

#' Private function for checking the size of the user's image.
#' 
check.image.size <- function() {
  total.img.size <- 0
  for (obj in ls()) {
    obj.size <- object.size(get(obj))*9.53674e-7#convert to MB
    total.img.size <- total.img.size + obj.size
    if (obj.size > 20) {
      msg <- paste("Object: ", obj, " is pretty big. Are you sure you want to send it to Yhat?", sep="")
#       print(msg)
    }
  }
  total.img.mb <- total.img.size[1]
  if (total.img.mb > 50) {
    stop("Sorry, your model is too big to deploy via HTTP.
         Try removing some large objects from your workspace using the rm() command, or
         deploy the model using yhat.deploy.to.file.")
  }
}

#' Shows which models you have deployed on Yhat.
#' 
#' This function queries the Yhat API and finds the models that have been deployed
#' for your account.
#' 
#' @export
#' @examples
#' yhat.config <- c(
#'  username = "your username",
#'  apikey = "your apikey",
#'  env = "http://cloud.yhathq.com/"
#' )
#' \dontrun{
#' yhat.show_models()
#' }
#' # some output here
#' #    username className                  name version
#' # 1      greg                 MySMSClassifier       1
#' # 2      greg                 MySMSClassifier       2
#' # 3      greg                 MySMSClassifier       3
#' # 4      greg                 MySMSClassifier       4
yhat.show_models <- function() {
  rsp <- yhat.get("showmodels")
  js <- httr::content(rsp)
  js <- lapply(js$models, function(model) {
    if (is.null(model$className)) {
      model$className <- ""
    }
    model
  })
  plyr::ldply(js, data.frame)
}

#' Calls Yhat's REST API and returns a JSON document containing both the prediction
#' and associated metadata.
#' 
#' @param model_name the name of the model you want to call
#' @param data input data for the model
#' 
#' @export
#' @examples
#' yhat.config <- c(
#'  username = "your username",
#'  apikey = "your apikey"
#' )
#' \dontrun{
#' yhat.predict_raw("irisModel", iris) 
#' }
yhat.predict_raw <- function(model_name, data) {
  AUTH <- get("yhat.config")
  if ("env" %in% names(AUTH)) {
    endpoint <- paste(AUTH["username"], "models", model_name, "", sep="/")
  } else {
    stop("Please specify an env.")
  }
  rsp <- yhat.post(endpoint, c(model = model_name),
                   data = data)
  httr::content(rsp)
}
#' Make a prediction using Yhat.
#' 
#' This function calls Yhat's REST API and returns a response formatted as a
#' data frame.
#' 
#' @param model_name the name of the model you want to call
#' @param data input data for the model
#' 
#' @keywords predict
#' @export
#' @examples
#' yhat.config <- c(
#'  username = "your username",
#'  apikey = "your apikey",
#'  env = "http://cloud.yhathq.com/"
#' )
#' \dontrun{
#' yhat.predict("irisModel", iris) 
#' }
yhat.predict <- function(model_name, data) {
  raw_rsp <- yhat.predict_raw(model_name, data)
  if ("prediction" %in% raw_rsp) {
    data.frame(raw_rsp$prediction)
  } else {
    data.frame(raw_rsp)
  }
}

#' Deploy a model to Yhat's servers
#' 
#' This function takes model.transform and model.predict and creates
#' a model on Yhat's servers which can be called from any programming language
#' via Yhat's REST API (see \code{\link{yhat.predict}}).
#' 
#' @param model_name name of your model
#' @keywords deploy
#' @export
#' @examples
#' yhat.config <- c(
#'  username = "your username",
#'  apikey = "your apikey",
#'  env = "http://cloud.yhathq.com/"
#' )
#' iris$Sepal.Width_sq <- iris$Sepal.Width^2
#' fit <- glm(I(Species)=="virginica" ~ ., data=iris)
#' 
#' model.require <- function() {
#'  # require("randomForest")
#' }
#' 
#' model.transform <- function(df) {
#'  df$Sepal.Width_sq <- df$Sepal.Width^2
#'  df
#' }
#' model.predict <- function(df) {
#'  data.frame("prediction"=predict(fit, df, type="response"))
#' }
#' \dontrun{
#' yhat.deploy("irisModel")
#' }
yhat.deploy <- function(model_name) {
  check.image.size()
  AUTH <- get("yhat.config")
  if (length(AUTH)==0) {
    stop("Please specify your account credentials using yhat.config.")
  }
  if ("env" %in% names(AUTH)) {
    url <- AUTH[["env"]]
    AUTH <- AUTH[!names(AUTH)=="env"]
    query <- AUTH
    query <- paste(names(query), query, collapse="&", sep="=")
    url <- paste(url, "deployer/model", "?", query, sep="")
    image_file <- ".yhatdeployment.img"
    save.image(image_file)
    rsp <- httr::POST(url, httr::authenticate(AUTH["username"], AUTH["apikey"], 'basic'),
         body=list(
           "model_image" = httr::upload_file(image_file),
           "modelname" = model_name
           )
    )
    unlink(image_file)
    js <- httr::content(rsp)
    data.frame(js)
  } else {
    print("Please specify 'env' parameter in yhat.config.")
  }
}


#' Deploy a model to a file that you can then upload via the browser.
#' 
#' This function creates a .yhat file which can be deployed via the browser.
#' This is useful for larger models (>20 MB).
#' 
#' @param model_name name of your model
#' @keywords deploy
#' @export
#' @examples
#' yhat.config <- c(
#'  username = "your username",
#'  apikey = "your apikey",
#'  env = "http://cloud.yhathq.com/"
#' )
#' iris$Sepal.Width_sq <- iris$Sepal.Width^2
#' fit <- glm(I(Species)=="virginica" ~ ., data=iris)
#' 
#' model.require <- function() {
#'  # require("randomForest")
#' }
#' 
#' model.transform <- function(df) {
#'  df$Sepal.Width_sq <- df$Sepal.Width^2
#'  df
#' }
#' model.predict <- function(df) {
#'  data.frame("prediction"=predict(fit, df, type="response"))
#' }
#' yhat.deploy.to.file("irisModel")
yhat.deploy.to.file <- function(model_name) {
  AUTH <- get("yhat.config")
  username <- AUTH[["username"]]
  f <- ".yhatdeployment.img"
  save.image(f)
  img <- RCurl::base64Encode(readBin(f, "raw", file.info(f)[1,"size"]))
  data <- list(
    image=img[1],
    modelName=model_name,
    className=model_name,
    username=username,
    language="r"
  )
  base::write(rjson::toJSON(data), file=paste(model_name, ".yhat", sep=""))
  paste("file written to :", paste(model_name, ".yhat", sep=""))
}

#' Quick function for setting up a basic scaffolding of functions for deploying on Yhat.
#' 
#' @export
#' @examples
#' yhat.scaffolding()
yhat.scaffolding <- function() {
  txt <- c(
    "model_transform <- function(df) {
  df.transformed <- transform(df, x2=x^2)
  df.transformed
}
model_predict <- function(df) {
  pred <- predict(df)
  data.frame('myPrediction' = pred)
}
model_require <- function() {
  require('library1')
  require('library2')
}"
)
  con <- file("yhatExample.R", open="w")
  writeLines(txt, con)
  close(con)
}
