#' Private predicate function that checks if the protocol of a url
#' is https.
#'
#' @param x is a url string
is.https <- function(x) {
  m <- regexec("^https?://", x)
  matches <- regmatches(x, m)
  if (matches=="https://") {
    h <- TRUE
  } else {
    h <- FALSE
  }
  h
}

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
    usetls <- FALSE
    if (is.https(url)) {
        usetls <- TRUE
    }
    url <- stringr::str_replace_all(url, "^https?://", "")
    url <- stringr::str_replace_all(url, "/$", "")
    if (usetls) {
      url <- sprintf("https://%s/", url)
    } else {
      url <- sprintf("http://%s/", url)
    }
    AUTH <- AUTH[!names(AUTH)=="env"]
    query <- c(query, AUTH)
    query <- paste(names(query), query, collapse="&", sep="=")
    url <- paste(url, endpoint, "?", query, sep="")
    httr::GET(url, httr::authenticate(AUTH[["username"]], AUTH[["apikey"]], 'basic'))
  } else {
    message("Please specify 'env' parameter in yhat.config.")
  }
}


#' Private function for verifying username and apikey
yhat.verify <- function() {
  tryCatch({
    AUTH <- get("yhat.config")
  }, error = function(e) {
    stop("Please define a yhat.config object")
  })
  env <- AUTH[["env"]]
  usetls <- FALSE
  if (is.https(env)) {
    usetls <- TRUE
  }
  env <- stringr::str_replace_all(env, "^https?://", "")
  env <- stringr::str_replace_all(env, "/$", "")
  username <- AUTH[["username"]]
  apikey <- AUTH[["apikey"]]
  if (usetls) {
    url <- sprintf("https://%s/verify?username=%s&apikey=%s",
                   env, username, apikey)
  } else {
    url <- sprintf("http://%s/verify?username=%s&apikey=%s",
                   env, username, apikey)
  }
  rsp <- httr::POST(url)
  if (httr::http_status(rsp)$category != "success") {
    stop(sprintf("Bad response from http://%s/", env))
  }
  status <- httr::content(rsp)$success
  if (is.null(status)) {
    stop("Invalid apikey/username combination!")
  }
  if (status != "true") {
    stop("Invalid apikey/username combination!")
  }
}

#' Private function for performing a POST request
#'
#' @param endpoint /path for REST request
#' @param query url parameters for request
#' @param data payload to be converted to raw JSON
#' @param silent should output of url to console be silenced? 
#' Default is \code{FALSE}.
yhat.post <- function(endpoint, query=c(), data, silent = TRUE) {
  if(!is.logical(silent)) stop("Argument 'silent' must be logical!")
  AUTH <- get("yhat.config")
  if (length(AUTH)==0) {
    stop("Please specify your account credentials using yhat.config.")
  }

  if ("env" %in% names(AUTH)) {
    url <- AUTH[["env"]]
    usetls <- FALSE
    if (is.https(url)) {
      usetls <- TRUE
    }
    url <- stringr::str_replace_all(url, "^https?://", "")
    url <- stringr::str_replace_all(url, "/$", "")
    if (usetls) { 
      url <- sprintf("https://%s/", url)
    } else {
      url <- sprintf("http://%s/", url)
    }
    AUTH <- AUTH[!names(AUTH)=="env"]
    query <- c(query, AUTH)
    query <- paste(names(query), query, collapse="&", sep="=")
    url <- paste(url, endpoint, "?", query, sep="")
    if(silent==FALSE) {
      message(url)
    }
    httr::POST(url, body = rjson::toJSON(data),
                    config = c(
                      httr::authenticate(AUTH[["username"]], AUTH[["apikey"]], 'basic'),
                      httr::add_headers("Content-Type" = "application/json")
                      )
              )
  } else {
    message("Please specify 'env' parameter in yhat.config.")
  }
}

#' Private function for checking the size of the user's image.
#'
check.image.size <- function() {
  bytes.in.a.mb <- 2 ^ 20
  model.size <- list()
  for (obj in yhat.ls()) {
    model.size[[obj]] <- object.size(get(obj))
  }
  # lets get this into a data.frame
  df <- data.frame(unlist(model.size))
  model.size <- data.frame(obj=rownames(df),size.mb=df[[1]] / bytes.in.a.mb)
  model.size
}

#' Checks dependencies and makes sure all are installed.
#' 
check.dependencies <- function() {
  is.function(jsonlite::validate)
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
#'  env = "http://sandbox.yhathq.com/"
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
#' @param model_owner the owner of the model [optional]
#' @param raw_input when true, incoming data will NOT be coerced into data.frame
#' @param silent should output of url to console (via \code{yhat.post})
#' be silenced? Default is \code{FALSE}.
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
yhat.predict_raw <- function(model_name, data, model_owner, raw_input = FALSE, silent = TRUE) {
  usage <- "usage:  yhat.predict(<model_name>,<data>)"
  if(missing(model_name)){
    stop(paste("Please specify the model name you'd like to call",usage,sep="\n"))
  }
  if(missing(data)){
    stop(paste("You didn't pass any data to predict on!",usage,sep="\n"))
  }
  AUTH <- get("yhat.config")
  if ("env" %in% names(AUTH)) {
    user <- AUTH[["username"]]
    if(!missing(model_owner)){
      user <- model_owner
    }
    endpoint <- sprintf("%s/models/%s/", user, model_name)
  } else {
    stop("Please specify an env in yhat.config")
  }

  # build the model url for the error message
  url <- AUTH[["env"]]
  usetls <- FALSE
  if (is.https(url)) {
    usetls <- TRUE
  }
  url <- stringr::str_replace_all(url, "^https?://", "")
  url <- stringr::str_replace_all(url, "/$", "")
  if (usetls) {
    model_url <- sprintf("https://%s/model/%s/", url, model_name)
  } else {
    model_url <- sprintf("http://%s/model/%s/", url, model_name)
  }
  query <- list()
  if (raw_input==TRUE) {
    query[["raw_input"]] <- "true"
  }

  error_msg <- paste("Invalid response: are you sure your model is built?\nHead over to",
                     model_url,"to see you model's current status.")
  tryCatch(
    {
      rsp <- yhat.post(endpoint, query = query, data = data, silent = silent)
      httr::content(rsp)
    },
    error = function(e){
      stop(error_msg)
    },
    exception = function(e){
      stop(error_msg)
    }
  )
}
#' Make a prediction using Yhat.
#'
#' This function calls Yhat's REST API and returns a response formatted as a
#' data frame.
#'
#' @param model_name the name of the model you want to call
#' @param data input data for the model
#' @param model_owner the owner of the model [optional]
#' @param raw_input when true, incoming data will NOT be coerced into data.frame
#' @param silent should output of url to console (via \code{yhat.post})
#' be silenced? Default is \code{FALSE}.
#'
#' @keywords predict
#' @export
#' @examples
#' yhat.config <- c(
#'  username = "your username",
#'  apikey = "your apikey",
#'  env = "http://sandbox.yhathq.com/"
#' )
#' \dontrun{
#' yhat.predict("irisModel", iris)
#' }
yhat.predict <- function(model_name, data, model_owner, raw_input = FALSE, silent = TRUE) {
  raw_rsp <- yhat.predict_raw(model_name, data, model_owner, raw_input = raw_input, silent = silent)
  tryCatch({
    if (raw_input==TRUE) {
      raw_rsp
    } else if ("result" %in% names(raw_rsp)) {
      data.frame(lapply(raw_rsp$result, unlist))
    } else {
      data.frame(raw_rsp)
    }
  },
  error = function(e){
    stop("Invalid response: are you sure your model is built?")
  },
  exception = function(e){
    stop("Invalid response: are you sure your model is built?")
  })
}

#' Test a prediction through the JSONification process
#'
#' This function tests model.transform and model.predict on new data by sending
#' it through a JSONification process before the two stated functions. This
#' allows users to test their model locally in conditions that are similar to
#' those after a deployment.
#'
#' @param data Data to envoke the model with
#' @param verbose Whether or not to print intermediate results
#' @export
#' @examples
#'
#' model.transform <- function(df) {
#'  df$Sepal.Width_sq <- df$Sepal.Width^2
#'  df
#' }
#' model.predict <- function(df) {
#'  data.frame("prediction"=predict(fit, df, type="response"))
#' }
#' \dontrun{
#' model.test_predict(iris)
#' }
yhat.test_predict <- function(data, verbose=FALSE) {
  t <- "model.transform"
  model.transform <- mget(t, globalenv(), ifnotfound=list(NULL))[[t]]
  if (is.null(model.transform)) {
      model.transform <- function(x) { x }
  }
  model.predict <- get("model.predict", globalenv())
  jsonified_data <- rjson::toJSON(data)
  model_input_data <- jsonlite::fromJSON(jsonified_data)
  model_input_data <- data.frame(model_input_data, stringsAsFactors=FALSE)
  if (verbose) {
    print(lapply(model_input_data, class))
  }
  transformed_data <- model.transform(model_input_data)
  if (verbose) {
    print(lapply(transformed_data, class))
  }
  model.predict(transformed_data)
}

#' Deploy a model to Yhat's servers
#'
#' This function takes model.transform and model.predict and creates
#' a model on Yhat's servers which can be called from any programming language
#' via Yhat's REST API (see \code{\link{yhat.predict}}).
#'
#' @param model_name name of your model
#' @param packages list of packages to install using apt-get
#' @keywords deploy
#' @export
#' @examples
#' yhat.config <- c(
#'  username = "your username",
#'  apikey = "your apikey",
#'  env = "http://sandbox.yhathq.com/"
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
yhat.deploy <- function(model_name, packages=c()) {
  if(missing(model_name)){
    stop("Please specify 'model_name' argument")
  }
  if (length(grep("^[A-Za-z_0-9]+$", model_name))==0) {
    stop("Model name can only contain following characters: A-Za-z_0-9")
  }
  yhat.verify()
  img.size.mb <- check.image.size()
  AUTH <- get("yhat.config")
  if (length(AUTH)==0) {
    stop("Please specify your account credentials using yhat.config.")
  }
  if ("env" %in% names(AUTH)) {
    env <- AUTH[["env"]]
    usetls <- FALSE
    if (is.https(env)) {
      usetls <- TRUE
    }
    env <- stringr::str_replace_all(env, "^https?://", "")
    env <- stringr::str_replace_all(env, "/$", "")
    AUTH <- AUTH[!names(AUTH)=="env"]
    query <- AUTH
    query <- paste(names(query), query, collapse="&", sep="=")
    if (usetls) {
      url <- sprintf("https://%s/deployer/model?%s", env, query)
    } else {
      url <- sprintf("http://%s/deployer/model?%s", env, query)
    }
    image_file <- ".yhatdeployment.img"

    all_objects <- yhat.ls()
    # if model.transform is not provided give it a default value
    if (!("model.transform" %in% all_objects)) {
        model.transform <- function(data) { data }
        all_objects <- c(all_objects, "model.transform")
    }

    all_funcs <- all_objects[lapply(all_objects, function(name){
      class(globalenv()[[name]])
    }) == "function"]
    save(list=all_objects,file=image_file)
    cat("objects detected\n")

    sizes <- lapply(all_objects, function(name) {
      format( object.size(globalenv()[[name]]) , units="auto")
    })
    sizes <- unlist(sizes)
    print(data.frame(name=all_objects, size=sizes))
    cat("\n")

    err.msg <- paste("Could not connect to yhat enterprise. Please ensure that your",
                     "specified server is online. Contact info [at] yhathq [dot] com",
                     "for further support.",
                     "-----------------------",
                     "Specified endpoint:",
                     env,
                     sep="\n")
    tryCatch({
        rsp <- httr::POST(url, httr::authenticate(AUTH[["username"]], AUTH[["apikey"]], 'basic'),
                        body=list(
                           "model_image" = httr::upload_file(image_file),
                           "modelname" = model_name,
                           "packages" = capture.packages(),
                           "apt_packages" = packages,
			   "code" = capture.src(all_funcs)
                                 )
                         )
      
        js <- httr::content(rsp)
        rsp.df <- data.frame(js)
      },
      error=function(e){ unlink(image_file); stop(err.msg) },
      exception=function(e){ unlink(image_file); stop(err.msg) }
    )
    unlink(image_file)
    cat("deployment successful\n")
    rsp.df
  } else {
    message("Please specify 'env' parameter in yhat.config.")
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
#'  env = "http://sandbox.yhathq.com/"
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
#'  yhat.deploy.to.file("irisModel")
#'  }
yhat.deploy.to.file <- function(model_name) {
  if(missing(model_name)){
    stop("Please specify 'model_name' argument")
  }  
  if (length(grep("^[A-Za-z_0-9]+$", model_name))==0) {
    stop("Model name can only contain following characters: A-Za-z_0-9")
  }
  AUTH <- get("yhat.config")
  username <- AUTH[["username"]]
  apikey <- AUTH[["apikey"]]
  f <- ".yhatdeployment.img"
  save(list=yhat.ls(),file=f)
  img <- RCurl::base64Encode(readBin(f, "raw", file.info(f)[1,"size"]))
  data <- list(
    image=img[1],
    modelName=model_name,
    className=model_name,
    username=username,
    apikey=apikey,
    language="r"
  )
  base::write(rjson::toJSON(data), file=paste(model_name, ".yhat", sep=""))
  paste("file written to :", paste(model_name, ".yhat", sep=""))
}
#' Deploy a model via SCP. For when you want to automate large model uploads.
#'
#' For when you have a really big model file and you don't want to mess with
#' uploading it via the admin console.
#' This is useful for larger models (>20 MB).
#'
#' @param model_name name of your model
#' @param pem_path path to your pemfile (for AWS)
#' @keywords deploy
#' @export
#' @examples
#' yhat.config <- c(
#'  username = "your username",
#'  apikey = "your apikey",
#'  env = "http://google.yhathq.com/"
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
#'  yhat.deploy.with.scp("irisModel", "~/path/to/pemfile.pem")
#' }
yhat.deploy.with.scp <- function(model_name, pem_path) {
  yhat.deploy.to.file(model_name)
  filename <- paste(model_name, ".yhat", sep="")
  AUTH <- get("yhat.config")
  servername <- AUTH[["env"]]

  system(paste("scp -i ", pem_path, " ubuntu@", servername, ":~/"))
  system(paste0("ssh -i ", pem_path, " ubuntu@", servername, " 'sudo mv ~/", filename, " /var/yhat/headquarters/uploads/'"))
  NULL
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

#' Private function for catpuring the source code of model
#'
#' @param funcs functions to caputre, defaults to required yhat model functions
capture.src <- function(funcs){
    if(missing(funcs)){
        funcs <- c("model.require","model.transform","model.predict")
    }
    global.vars <- ls(.GlobalEnv)
    src <- "library(yhatr)"
    for(func in funcs){
        if(func %in% global.vars){
	    func.src <- paste(capture.output(.GlobalEnv[[func]]),collapse="\n")
            func.src <- paste(func,"<-",func.src)
            src <- paste(src,func.src,sep="\n\n")
        }
    }
    src
}

capture.packages <- function(){
  si <- sessionInfo()
  pkgs <- names(si$otherPkgs)
  pkgs <- pkgs[pkgs != "yhatr"]
  pkgdata <- lapply(pkgs, function(x) list(name=x, version=packageDescription(x)$Version))
  rjson::toJSON(pkgdata)
}

#' Generates a model.transform function from an example input data.frame.
#' Handles columns which need to be type casted further after the initial JSON
#' to Robject such as factors and ints.
#'
#' @param df A data.frame object which mirrors the kind of input to the model. 
#'
#' @export
#' @examples
#' \dontrun{
#' model.transform <- yhat.transform_from_example(iris)
#' }
yhat.transform_from_example <- function(df) {
    if(!is.data.frame(df)) {
        stop("Input must be of class 'data.frame'")
    }
    # capture class types of each column
    classes <- lapply(df, class)
    factor_classes <- classes[classes == "factor"]
    factor_levels <- lapply(df[names(factor_classes)], levels)
    non_factor_classes <- classes[classes != "factor"]
  
    # The thing we're returning is a function
    function(new_df) {
        col_names <- names(new_df)
        # factors require the levels to be set to the correct values
        for(col_name in names(factor_levels)) {
            if (col_name %in% col_names) {
                new_df[[col_name]] <- factor(new_df[[col_name]], levels=factor_levels[[col_name]])
            }    
        }
        # for the non factor columns, simply type cast
        for(col_name in names(non_factor_classes)) {
            if (col_name %in% col_names) {
                as_type <- tryCatch({
                    get(paste("as", non_factor_classes[[col_name]], sep="."))
                }, error=function(cond) {
                    # if we can't find the as.[type] function, just leave it alone
                    function(col) { col }
                })
                new_df[[col_name]] <- tryCatch({
                    as_type(new_df[[col_name]])
                }, error=function(cond) {
                    new_df[[col_name]]
                })
            }
        }
        new_df
    }
}

#' Private function for recursively looking for variables
#'
#' @param block code block to spider
#' @param defined.vars variables which have already been defined within the
#'          scope of the block. e.g. function argument
yhat.spider.block <- function(block,defined.vars=c()){
    # if block is a symbol, just return that symbol
    if(typeof(block) == "symbol") {
        return(c(block))
    }
    symbols <- c()
    n <- length(block)
    if(n == 0) {
        return(symbols)
    }
    for(i in 1:n){
        node <- block[[i]]
        # Really weird bug that comes from assigning the "empty" symbol to a
        # variable. No obvious way to test for this case other than a try/catch
        is.valid.symbol <- tryCatch({
            node
            TRUE
        }, error = function(e) {
            FALSE
        })
        if(!is.valid.symbol){ next }
        node.type <- typeof(node)
        # if node type is "symbol" then it might be a variable 
        if(node.type == "symbol"){
            # if symbol not already defined then it might be a dependency
            if(!any(node == defined.vars)){
                symbols <- c(symbols,node)
            }
        # if node type is "language" then it is another block we'll want to spider
        } else if (node.type == "language"){
            # is the block an assignment statement? if so we'll want to add the
            # assignment result to the list of defined variables
            if ((node[[1]] == as.symbol("<-")) || (node[[1]] == as.symbol("="))){
                # Code will look like this:
                #     `assign.to` <- `assign.from`
                assign.from <- node[[3]]
                assign.from.type <- typeof(assign.from)
                if (assign.from.type == "symbol"){
                    # if symbol not already defined then it might be a dependency
                    if (!any(assign.from == defined.vars)){
                        symbols <- c(symbols,assign.from)
                    }
                } else if (assign.from.type == "language"){
                    symbols <- c(symbols,yhat.spider.block(assign.from,defined.vars))
                }

                assign.to <- node[[2]]
                assign.to.type <- typeof(assign.to)
                if (assign.to.type == "symbol"){
                    # yay! the user has defined a variable
                    defined.vars <- c(assign.to,defined.vars)
                } else if (assign.to.type == "language"){
                    # Wait, what?!?! are you assigning to a block of code?
                    symbols <- c(symbols,yhat.spider.block(assign.to, defined.vars))
                }
            } else {
                # if the block isn't an assignment, recursively crawl
                symbols <- c(symbols,yhat.spider.block(node,defined.vars))
            }
        }
    }
    # return a list of symbols which are candidates for global dependency
    symbols
}

#' Private function for spidering function source code
#'
#' @param func.name name of function you want to spider
yhat.spider.func <- function(func.name){
    # parse function to pull out main block and argument names
    func <- parse(text=getAnywhere(func.name))[[2]][[2]]
    # we will be comparing symbols not strings
    args <- lapply(names(func[[2]]),as.symbol)
    block <- func[[3]]
    # get all symbols used during function which are dependencies
    func.vars <- unique(yhat.spider.block(block,defined.vars=args))
    # return dependency candidates which are defined in the global scope
    # (these are all variables we'll want to capture)
    intersect(func.vars,names(as.list(.GlobalEnv)))
}

#' Private function for determining model dependencies
#'
#' List all object names which are dependencies of `model.transform`
#' and `model.predict`
yhat.ls <- function(){
    funcs <- c("model.predict") # function queue to spider
    global.vars <- ls(.GlobalEnv,all.names=T)
    if("model.transform" %in% global.vars){
        funcs <- c(funcs, "model.transform")
    }
    if (!("model.predict" %in% global.vars)){
        err.msg <- "ERROR: You must define \"model.predict\" before deploying a model"
        stop(err.msg)
    }
    dependencies <- funcs
    while(length(funcs) > 0){
        # pop first function from queue
        func.name <- funcs[[1]]
        n.funcs <- length(funcs)
        if(n.funcs > 1){
            funcs <- funcs[2:length(funcs)]
        } else {
            funcs <- c()
        }
        # spider a function and get all variable dependencies
        func.vars <- yhat.spider.func(func.name)
        n.vars <- length(func.vars)
        if(n.vars > 0){
            for(i in 1:n.vars){
                var <- func.vars[[i]]
                # is variable already a dependency?
                if(!(var %in% dependencies)){
                    dependencies <- c(var,dependencies)
                    # if this variable is a function we're going to
                    # want to spider it as well
                    if(typeof(.GlobalEnv[[var]]) == "closure"){
                        # add function to function queue
                        funcs <- c(var,funcs)
                    }
                }
            }
        }
    }
    if("model.require" %in% global.vars){
        dependencies <- c("model.require",dependencies)
    }
    dependencies
}
