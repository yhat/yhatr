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
  if (rsp$status_code != 200) {
    cat(httr::content(rsp, "text"), "\n")
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
#' @param bulk is this a bulk style request? Default is \code{FALSE}.
yhat.post <- function(endpoint, query=c(), data, silent = TRUE, bulk = FALSE) {
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

    # bullk sends back line delimited JSON
    if (bulk==TRUE) {
      out <- textConnection("data.json", "w")
      jsonlite::stream_out(data, con = out)
      close(out)
    } else {
      data.json <- jsonlite::toJSON(data, dataframe = "columns")
    }
    httr::POST(url, body = data.json,
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
#' @importFrom utils object.size
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

#' Calls Yhat's REST API and returns a JSON document containing both the prediction
#' and associated metadata.
#'
#' @param model_name the name of the model you want to call
#' @param data input data for the model
#' @param model_owner the owner of the model [optional]
#' @param raw_input when true, incoming data will NOT be coerced into data.frame
#' @param silent should output of url to console (via \code{yhat.post})
#' be silenced? Default is \code{FALSE}.
#' @param bulk should the bulk api be used Default is \code{FALSE}.
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
yhat.predict_raw <- function(model_name, data, model_owner, raw_input = FALSE, silent = TRUE, bulk = FALSE) {
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
  if (bulk==TRUE) {
    query[["bulk"]] <- "true"
  }

  error_msg <- paste("Invalid response: are you sure your model is built?\nHead over to",
                     model_url,"to see you model's current status.")
  tryCatch(
    {
      rsp <- yhat.post(endpoint, query = query, data = data, silent = silent, bulk = bulk)
      httr::content(rsp)
    },
    error = function(e){
      print(e)
      stop(error_msg)
    },
    exception = function(e){
      print(e)
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

#' Make bulk predictions using Yhat.
#'
#' This function calls Yhat's bulk API and returns a response formatted as a
#' data frame.
#'
#' @param model_name the name of the model you want to call
#' @param data input rows of data to be scored
#' @param model_owner the owner of the model [optional]
#' @param raw_input when true, incoming data will NOT be coerced into data.frame
#' @param silent should output of url to console (via \code{yhat.post})
#' be silenced? Default is \code{FALSE}.
#'
#' @keywords bulk
#' @export
#' @examples
#' yhat.config <- c(
#'  username = "your username",
#'  apikey = "your apikey",
#'  env = "http://sandbox.yhathq.com/"
#' )
#' \dontrun{
#' yhat.predict_bulk("irisModel", iris)
#' }
yhat.predict_bulk <- function(model_name, data, model_owner, raw_input = FALSE, silent = TRUE) {
  raw_rsp <- yhat.predict_raw(model_name, data, model_owner, raw_input = raw_input, silent = silent, bulk = TRUE)
  tryCatch({
    # this is weird but it's the only way I could figure out how to turn raw_rsp
    # into a connection() type (which is basically a stream). we create a tempfile,
    # write raw_rsp to that file, then read it back in using jsonlite
    f <- file(tmp <- tempfile())
    write(raw_rsp, f)
    close(f)
    output <- jsonlite::stream_in(file(tmp))
    unlink(tmp)
    output
  },
  error = function(e){
    stop(paste("Invalid response: are you sure your model is built?", e))
  },
  exception = function(e){
    stop("Invalid response: are you sure your model is built?")
  })
}

# Create a new environment in order to namespace variables that hold the package state
yhat <- new.env(parent = emptyenv())

# Packages that need to be installed for the model to run - this will almost always
# include all the packages listed in imports
yhat$dependencies <- data.frame()

# Private function for storing requirements that will be imported on
# the ScienceOps server
yhat$model.require <- function() {
}

#' Import one or more libraries and add them to the Yhat model's
#' dependency list
#'
#' @param name name of the package to be added
#' @param src source from which the package will be installed on ScienceOps (github or CRAN)
#' @param version version of the package to be added
#' @param user Github username associated with the package
#' @param install Whether the package should also be installed into the model on the
#' ScienceOps server; this is typically set to False when the package has already been
#' added to the ScienceOps base image.
#' @keywords import
#' @export
#' @examples
#' \dontrun{
#' yhat.library("MASS")
#' yhat.library(c("wesanderson", "stringr"))
#' yhat.library("cats", src="github", user="hilaryparker")
#' yhat.library("hilaryparker/cats")
#' yhat.library("my_proprietary_package", install=FALSE)
#' }
#' @importFrom utils packageDescription
yhat.library <- function(name, src="CRAN", version=NULL, user=NULL, install=TRUE) {
  # If a vector of CRAN packages is passed, add each of them
  if (length(name) > 1) {
    for (n in name) {
      yhat.library(n, install=install)
    }
    return()
  }

  if (!src %in% c("CRAN", "github")) {
    stop(cat(src, "is not a valid package type"))
  }

  if (src == "github") {
    if (is.null(user)) {
      stop(cat("no github username specified"))
    }
    installName <- paste(user, "/", name, sep="")
  } else {
    installName <- name
  }

  if (grepl("/", name)) {
    src <- "github"
    nameAndUser <- unlist(strsplit(name, "/"))
    user <- nameAndUser[[1]]
    name <- nameAndUser[[2]]
  }

  library(name, character.only = TRUE)

  # If a version wasn't manually specified, get this info from the session
  if (is.null(version)) {
    version <- packageDescription(name)$Version
  }

  add.dependency(installName, name, src, version, install)

  set.model.require()
}

#' Removes a library from the Yhat model's dependency list
#'
#' @param name of the package to be removed
#'
#' @export
#' @examples
#' \dontrun{
#' yhat.unload("wesanderson")
#' }
yhat.unload <- function(name) {
  deps <- yhat$dependencies
  yhat$dependencies <- deps[deps$importName != name,]
  set.model.require()
}

#' Private function that adds a package to the list of dependencies
#' that will be installed on the ScienceOps server
#' @param name name of the package to be installed
#' @param importName name under which the package is imported (for a github package,
#' this may be different from the name used to install it)
#' @param src source that the package is installed from (CRAN or github)
#' @param version version of the package
#' @param install whether or not the package should be installed in the model image
add.dependency <- function(name, importName, src, version, install) {
  # Don't add the dependency if it's already there
  dependencies <- yhat$dependencies
  if (!any(dependencies$name == name)) {
    newRow <- data.frame(name=name, importName=importName, src=src, version=version, install=install)
    dependencies <- rbind(dependencies, newRow)
    yhat$dependencies <- dependencies
  }
}

#' Private function that generates a model.require function based on
#' the libraries that have been imported in this session.
set.model.require <- function() {
  imports <- yhat$dependencies$importName
  yhat$model.require <- function() {
    for (pkg in imports) {
      library(pkg, character.only = TRUE)
    }
  }
}

confirm.deployment <- function() {
  deps <- yhat$dependencies
  deps$importName <- NULL
  cat("Model will be deployed with the following dependencies:\n")
  print(deps)
  needsConfirm <- TRUE
  while (needsConfirm) {
      sure <- readline("Are you sure you want to deploy? y/n ")
      if (sure == "n" || sure == "N") {
        needsConfirm <- FALSE
        stop("Deployment cancelled")
      } else if (sure == "y" || sure == "Y") {
        needsConfirm <- FALSE
      }
    }
}

#' Deploy a model to Yhat's servers
#'
#' This function takes model.transform and model.predict and creates
#' a model on Yhat's servers which can be called from any programming language
#' via Yhat's REST API (see \code{\link{yhat.predict}}).
#'
#' @param model_name name of your model
#' @param packages list of packages to install using apt-get
#' @param confirm boolean indicating whether to prompt before deploying
#' @param custom_image name of the image you'd like your model to use
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
#' yhat.deploy("irisModelCustomImage", custom_image="myImage:latest")
#' }
yhat.deploy <- function(model_name, packages=c(), confirm=TRUE, custom_image=NULL) {
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
    image_file <- tempfile(pattern="scienceops_deployment")

    all_objects <- yhat.ls()
    # Consolidate local environment with global one
    deployEnv <- new.env(parent = emptyenv())
    deployEnv$model.require <- yhat$model.require
    for (obj in all_objects) {
      deployEnv[[obj]] <- globalenv()[[obj]]
    }
    # if model.transform is not provided give it a default value
    if (!("model.transform" %in% all_objects)) {
        model.transform <- function(data) { data }
        deployEnv$model.transform <- function(data) { data }
        all_objects <- c(all_objects, "model.transform")
    }

    all_funcs <- all_objects[lapply(all_objects, function(name){
      class(globalenv()[[name]])
    }) == "function"]
    all_objects <- c("model.require", all_objects)

    save(list=all_objects, envir=deployEnv, file=image_file)
    cat("objects detected\n")

    sizes <- lapply(all_objects, function(name) {
      format( object.size(globalenv()[[name]]) , units="auto")
    })
    sizes <- unlist(sizes)
    print(data.frame(name=all_objects, size=sizes))
    cat("\n")

    if (confirm && interactive()) {
      confirm.deployment()
    }

    dependencies <- yhat$dependencies[yhat$dependencies$install,]

    err.msg <- paste("Could not connect to ScienceOps. Please ensure that your",
                     "specified server is online. Contact info [at] yhathq [dot] com",
                     "for further support.",
                     "-----------------------",
                     "Specified endpoint:",
                     env,
                     sep="\n")
    rsp <- httr::POST(url, httr::authenticate(AUTH[["username"]], AUTH[["apikey"]], 'basic'),
      body=list(
      "model_image" = httr::upload_file(image_file),
      "modelname" = model_name,
      "packages" = jsonlite::toJSON(dependencies),
      "apt_packages" = packages,
		  "code" = capture.src(all_funcs),
      "custom_image" = custom_image
      )
    )
    body <- httr::content(rsp)
    if (rsp$status_code != 200) {
      unlink(image_file)
      stop("deployment error: ", body)
    }
    rsp.df <- data.frame(body)
    unlink(image_file)
    cat("deployment successful\n")
    rsp.df
  } else {
    message("Please specify 'env' parameter in yhat.config.")
  }
}

#' Deploy a batch model to Yhat servers
#'
#' This function will deploy your batch model to the yhat servers
#'
#' @param job_name name of batch job
#' @param confirm boolean indicating whether to prompt before deploying
#' @keywords deploy
#' @export
#' @examples
#' yhat.config <- c(
#'  username = "your username",
#'  apikey = "your apikey",
#'  env = "http://sandbox.yhathq.com/"
#' )
#' yhat.batch <- function() {
#'   name <- "ross"
#'   greeting <- paste("Hello", name)
#'   print(greeting)
#' }
#' \dontrun{
#' yhat.batchDeploy("helloworld")
#' }
#' @importFrom utils tar
yhat.batchDeploy <- function(job_name, confirm=TRUE) {
  if(missing(job_name)) {
    stop("Please specify 'job_name' argument")
  }
  if (length(grep("^[a-z_0-9]+$", job_name))==0) {
    stop("Model name can only contain following characters: a-z_0-9")
  }
  yhat.verify()
  AUTH <- get("yhat.config")
  if (length(AUTH)==0) {
    stop("Please specify your account credentials using yhat.config.")
  }
  # Check if we have a yhat.yaml file, if we don't then ask the user to try again
  if (!file.exists('yhat.yaml')) {
    stop("Please provide a yhat.yaml file in the working directory to specify the job config.")
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
      url <- sprintf("https://%s/batch/deploy?%s", env, query)
    } else {
      url <- sprintf("http://%s/batch/deploy?%s", env, query)
    }

    all_objects <- yhat.ls(batchMode=TRUE)

    all_funcs <- all_objects[lapply(all_objects, function(name){
      class(globalenv()[[name]])
    }) == "function"]

    all_objects <- c("model.require", all_objects)

    cat("objects detected\n")

    sizes <- lapply(all_objects, function(name) {
      format( object.size(globalenv()[[name]]) , units="auto")
    })
    sizes <- unlist(sizes)
    print(data.frame(name=all_objects, size=sizes))
    cat("\n")

    if (confirm && interactive()) {
      confirm.deployment()
    }

    dependencies <- yhat$dependencies[yhat$dependencies$install,]

    err.msg <- paste("Could not connect to ScienceOps. Please ensure that your",
                     "specified server is online. Contact info [at] yhathq [dot] com",
                     "for further support.",
                     "-----------------------",
                     "Specified endpoint:",
                     env,
                     sep="\n")

    # Create the bundle.json and requirements.txt files
    bundleFrame <- list(
      code = jsonlite::unbox(capture.src(all_funcs, capture.model.require=FALSE)),
      language = jsonlite::unbox("R")
    )
    bundleJson <- jsonlite::toJSON(bundleFrame)
    f = file("bundle.json", open="wb")
    write(bundleJson, f)
    close(f)

    depList = list(dependencies=dependencies)
    depJson <- jsonlite::toJSON(depList, dataframe=c("rows"))
    f = file("requirements.txt", open="wb")
    write(depJson, f)
    close(f)

    # Create the bundle
    sysName <- Sys.info()["sysname"]
    zip <- ""
    if (sysName == "Darwin" || sysName == "Linux") {
      # OSX workaround...
      bundle_name <- "yhat_job.tar.gz"
      filenames <- c('bundle.json', 'yhat.yaml', 'requirements.txt')
      filenames.fmt <- paste(filenames, collapse=" ")
      cmd <- sprintf("/usr/bin/tar -czvf %s %s", bundle_name, filenames.fmt)
      system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    } else if (sysName == "Windows") {
      bundle_name <- "yhat_job.zip"
      zip(bundle_name, c("bundle.json", 'yhat.yaml', 'requirements.txt'))
      zip <- "true"
    } else {
      bundle_name <- "yhat_job.tar.gz"
      tar(bundle_name, c("bundle.json", 'yhat.yaml', 'requirements.txt'), compression = 'gzip', tar='tar')
    }

    rsp <- httr::POST(url,
      httr::authenticate(AUTH[["username"]], AUTH[["apikey"]], 'basic'),
      body=list(
        "job" = httr::upload_file(bundle_name),
        "job_name" = job_name,
        "zip" = zip
      )
    )
    body <- httr::content(rsp)
    if (rsp$status_code != 200) {
      unlink(bundle_name)
      stop("deployment error: ", body)
    }
    rsp.df <- data.frame(body)
    # After the upload, clean up
    unlink(bundle_name)
    unlink("bundle.json")
    unlink("requirements.txt")
    cat("deployment successful\n")
    rsp.df
  } else {
    message("Please specify 'env' parameter in yhat.config.")
  }
}



#' Private function for catpuring the source code of model
#'
#' @param funcs functions to capture, defaults to required yhat model functions
#' @param capture.model.require flag to capture the model.require function
#' @importFrom utils capture.output
capture.src <- function(funcs, capture.model.require=TRUE){
    yhat$model.require()
    if(missing(funcs)){
        funcs <- c("model.transform","model.predict")
    }
    global.vars <- ls(.GlobalEnv)
    src <- ""
    if (capture.model.require==TRUE) {
      src <- paste(capture.output(yhat$model.require),collapse="\n")
    }

    for(func in funcs){
        if(func %in% global.vars){
            func.src <- paste(capture.output(.GlobalEnv[[func]]), collapse="\n")
            func.src <- paste(func,"<-", func.src)
            src <- paste(src, func.src,sep="\n\n")
        }
    }
    src
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
                        symbols <- c(symbols, assign.from)
                    }
                } else if (assign.from.type == "language") {
                    symbols <- c(symbols, yhat.spider.block(assign.from, defined.vars))
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
#' @importFrom utils getAnywhere
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
#' and `model.predict` or `yhat.batch` if this is a batch mode deploy
#'
#' @param batchMode boolean to capture yhat.batch code for a batch job
yhat.ls <- function(batchMode=FALSE){
    funcs <- c("model.predict") # function queue to spider
    global.vars <- ls(.GlobalEnv,all.names=T)
    if ("model.transform" %in% global.vars) {
        funcs <- c(funcs, "model.transform")
    }
    if (batchMode) {
      funcs <- c("yhat.batch")
      if (!("yhat.batch" %in% global.vars)){
        err.msg <- "ERROR: You must define \"yhat.batch\" before deploying a batch job"
        stop(err.msg)
      }
    } else {
      if (!("model.predict" %in% global.vars)){
        err.msg <- "ERROR: You must define \"model.predict\" before deploying a model"
        stop(err.msg)
      }
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
        stop("Warning: model.require is deprecated as of yhatr 0.13.9 - please use yhat.library to specify model dependencies")
    }
    dependencies
}
