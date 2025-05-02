#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  ## start-2嵌入代码开始，作用：HTTP请求函数
  url_execute <- function(curl_type, cur_url, cur_data, cur_header){
    response <- NULL
    if(curl_type==1){
      response <- try(GET(cur_url, cur_header), silent = TRUE)
    }else{
      response <- try(POST(cur_url, cur_header, body = cur_data, encode = "json"), silent = TRUE)
    }
    
    if (inherits(response, "try-error")) {
      response <- list(code=-1)
    } else if (status_code(response) == 200) {
      response <- fromJSON(content(response, "text", encoding = "UTF-8"))
    } else {
      response <- list(code=-1)
    }
  }
  ## end-2嵌入代码开始，作用：HTTP请求函数
  # init
  observe({
    ## start-3嵌入代码开始，作用：进入/退出HTTP请求记录
    query <- parseQueryString(session$clientData$url_search)
    session$userData$id <- query$id
    session$userData$appName <- query$appName
    session$userData$token <- query$token
    headers <- httr::add_headers(`Token`=session$userData$token, `Content-Type`="application/json")
    connect_req = list(`appName`=session$userData$appName, `action`="connect", `id`=session$userData$id)
    connect_data <- try(url_execute(2, 'http://10.2.26.152/sqx_fast/app/workstation/shiny-connect-action', toJSON(connect_req, pretty = FALSE,auto_unbox = TRUE), headers), silent = TRUE)
    if (connect_data$code!=0) {
      session$close()
    }
    ## end-3嵌入代码开始，作用：进入/退出HTTP请求记录
  })
  # file size is 5MB by default. This changes it to 30MB
  # options(shiny.maxRequestSize = 30*1024^2)
  options(warn = -1) # turn off warning
  pdf(file = NULL)

  # define where database is located
  db_ver <<- "data107"
  db_url <<- "http://bioinformatics.sdstate.edu/data/"

  # if environmental variable is not set, use relative path
  DATAPATH <<- Sys.getenv("IDEP_DATABASE")[1]
  # if not defined in the environment, use too levels above
  if (nchar(DATAPATH) == 0) {
    DATAPATH <<- paste0("../../data/")
  }
  #Add version
  DATAPATH <<- paste0(DATAPATH, "/", db_ver, "/")
  org_info_file <<- paste0(DATAPATH, "demo/orgInfo.db")
  if(!file.exists(org_info_file)) {
    DATAPATH <<- paste0("./", db_ver, "/")
    org_info_file <<- paste0(DATAPATH, "demo/orgInfo.db")
  }

  # load static data files such as list of species, gmt files, etc
  # This could be moved to run_app as global variable, as in global.R
  # see https://github.com/ThinkR-open/golem/issues/6
  idep_data <- get_idep_data()

  # Tab Variable to control reactivity
  tab <- reactive(input$navbar)

  load_data <- mod_01_load_data_server(
    id = "load_data",
    idep_data = idep_data,
    tab = tab
  )
  pre_process <- mod_02_pre_process_server(
    id = "pre_process",
    load_data = load_data,
    tab = tab
  )
  mod_03_clustering_server(
    id = "clustering",
    pre_process = pre_process,
    idep_data = idep_data,
    tab = tab
  )
  mod_04_pca_server(
    id = "pca",
    load_data = load_data,
    pre_process = pre_process,
    idep_data = idep_data
  )
  deg <- mod_05_deg_server(
    id = "deg",
    pre_process = pre_process,
    idep_data = idep_data,
    load_data = load_data,
    tab = tab
  )
  mod_06_pathway_server(
    id = "pathway",
    pre_process = pre_process,
    deg = deg,
    idep_data = idep_data,
    tab = tab
  )
  mod_07_genome_server(
    id = "genome",
    pre_process = pre_process,
    deg = deg,
    idep_data = idep_data
  )
  mod_08_bicluster_server(
    id = "bicluster",
    pre_process = pre_process,
    idep_data = idep_data,
    tab = tab
  )
  mod_09_network_server(
    id = "network",
    pre_process = pre_process,
    idep_data = idep_data,
    tab = tab
  )
  mod_10_doc_server(
    id = "doc",
    pre_process = pre_process,
    idep_data = idep_data,
    tab = tab
  )
  session$onSessionEnded(function() {
    headers <- add_headers(`Token`=session$userData$token, `Content-Type`="application/json")
    connect_req_end = list(`appName`=session$userData$appName, `action`="disconnect", `id`=session$userData$id)
    connect_end_data <- try(url_execute(2, 'http://10.2.26.152/sqx_fast/app/workstation/shiny-connect-action', toJSON(connect_req_end, pretty = FALSE, auto_unbox = TRUE), headers), silent = TRUE)
    stopApp()
  })
}
