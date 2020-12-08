#' Start R Shiny app
#' @description start R Shiny apps included in this package.
#' @param app_name app or script name
#' @param n app number
#' @param pkg  package name
#' @param pt Port number
#' @param lb define the browser- shiny.launch.browser
#' @param ht define the host or ip address
#' @param dm display modes are auto, normal or showcase
#' @param msg_lvl message level
#' @param loc location of the scirpt: local|github; default to 'local'
#' @export
#' @examples
#' # this function start an interactive page so we could not add an runnable
#' # example
#'  NULL;
#' @author Hanming Tu
#' @name start_app
# ---------------------------------------------------------------------------
# HISTORY   MM/DD/YYYY (developer) - explanation
#  06/10/2019 (htu) - initial creation based on start_phuse
#  06/11/2019 (htu) - added loc parameter so that you can use local repository
#  12/02/2020 (htu) - added logic to define packages and dirs
#
# start_app <- function (n = 2)
#{
#  app_name <- paste0(sprintf("%02d", n), "_display")
#  start_app(app_name)
# }
#
# rm(list=ls())
# Sys.setenv("g_lvl"=3)

start_app <- function (app_name = "showenv", n = 1, pkg = "comFuncs"
                        , pt = NULL
                        , lb = getOption("shiny.launch.browser",interactive())
                        , ht = getOption("shiny.host", "127.0.0.1")
                        #, dm = c("auto", "normal", "Normal")
                        , dm =  "normal"
                        , msg_lvl = NULL
                       , loc = 'local'
) {
  prg <- "start_app"; echo_msg(prg,0.0,'Started', 1)

  if(!isNamespaceLoaded(pkg)){
    if (!requireNamespace(pkg)) {
      stop(paste("ERR: Could not load package", pkg))
    }
    }
  if (is_empty(msg_lvl)) {
    Sys.setenv("g_lvl"=0, "d_lvl"=0)
  } else {
    Sys.setenv("g_lvl"=msg_lvl, "d_lvl"=msg_lvl)
  }
  echo_msg(prg,0.1, paste("app_name =", app_name, "n =", n, "pkg =", pkg), 1)

  pks <- data.frame(genTS=c("07_genTS"),
        phuse=c("01_html","02_display","03_showenv","04_merge","05_d3","06_dashboardjs","07_genTS"),
        podr=c("01_podr"),
        orabkup=c("orabkup")
  )
  # get directory for the app
  dirs <- c(genTS="apps", phuse="examples",pdor="apps",orabkup="apps",comFuncs="apps")
  adr  <- ifelse((pkg %in% names(dirs)),"apps", dirs[pkg])
  str(dirs)
  echo_msg(prg,0.2, paste( "adr =", adr), 1)

  # get app name
  appDir <- system.file(adr, package = pkg )
  apps   <- list.files(appDir);
  app    <- ifelse(is_empty(app_name), apps[n],app_name)

  echo_msg(prg,0.3, paste("appDir =", appDir, " app =", app), 1)

  dir <- get_abs_path(appDir, app)
  echo_msg(prg,0.4, paste("Resolved dir =", dir), 1)

  if (is_empty(dir)) {
    echo_msg(prg,1.1,paste('could not find a dir for ',app), 1)
    if (is.na(app)) {
      errFun <- message
      errMsg <- ""
    } else {
      errFun <- stop
      errMsg <- paste("App", app, "does not exist. ")
    }
    errFun(errMsg, "Valid apps are \""
           , paste(list.files(appDir), collapse = "\", \""), "\"")
  } else {
    echo_msg(prg,1.2,paste('Start app from ',dir), 1)
    shiny::runApp(dir
                  , port = pt, host = ht, launch.browser = lb, display.mode = dm)
  }
}

