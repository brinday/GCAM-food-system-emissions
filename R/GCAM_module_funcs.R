
SavePorjectData <- function(.DFNAME, .DIR_MODULE = "FigData"){

  if(is.character(.DFNAME)){
    .DF <- get(.DFNAME)
    assertthat::assert_that(is.data.frame(.DF))
  } else
    if(is.data.frame(.DFNAME)){
      .DF <- .DFNAME
      .DFNAME <- deparse(substitute(.DFNAME))
    } else{stop("Check input data")}


  # create a fig data output folder
  dir.create(file.path(DIR_OUTPUT, Project, .DIR_MODULE), showWarnings = F)
  .DF %>% saveRDS(file.path(DIR_OUTPUT, Project, .DIR_MODULE, paste0(.DFNAME, ".RDS")))
  #.DF %>% readr::write_csv(file = file.path("output/FigDataCSV/", paste0(.DFNAME, ".csv")))

}

LoadProjectData <- function(.DFNAME, .DIR_MODULE = "FigData"){

  if(is.character(.DFNAME)){
    .DFNAME <- .DFNAME
  } else {
    .DFNAME <- deparse(substitute(.DFNAME))
  }

  FilePath <- file.path(DIR_OUTPUT, Project, .DIR_MODULE, paste0(.DFNAME, ".RDS"))
  assertthat::assert_that(file.exists(FilePath), msg = "RDS data does not exist in Cache")
  readRDS(FilePath)
}



# read_csv_bind <- function(.multiCSVpath){
#
#   library(doParallel)
#   myCluster <-
#     makeCluster(4, # number of cores to use
#                 type = "PSOCK") # type of cluster
#   #detectCores()
#   registerDoParallel(myCluster)
#
#   foreach(csvDir = .multiCSVpath,
#           .combine=rbind,
#           .packages = "dplyr" ,.errorhandling = "remove"
#   ) %dopar% {
#     readr::read_csv(csvDir, skip = 1)%>%
#       select(-matches("^X|\\...")) %>%
#       na.omit() %>%
#       filter(scenario != "scenario") %>%
#       mutate(scenario = gsub(",date.*$", "", scenario)) %>%
#       gcamdata::gather_years()
#   } -> df
#
#   stopCluster(myCluster)
#   return(df)
# }

# Modify/customize read csv function ----
read_csv_bind <- function(.multiCSVpath){

  library(doParallel)
  myCluster <-
    makeCluster(4, # number of cores to use
                type = "PSOCK") # type of cluster
  #detectCores()
  registerDoParallel(myCluster)

  foreach(csvDir = .multiCSVpath,
          .combine=rbind,
          .packages = "dplyr" ,.errorhandling = "remove"
  ) %dopar% {
    readr::read_csv(csvDir, skip = 1)%>%
      select(-matches("^X|\\...")) %>%
      na.omit() %>%
      filter(scenario != "scenario") %>%
      mutate(scenario = gsub(",date.*$", "", scenario)) %>%
      gcamdata::gather_years() %>%
      mutate(ss = sub(".*/([^/]+)/.*", "\\1", csvDir))
  } -> df

  stopCluster(myCluster)
  return(df)
}

Load_GCAM <- function(
    rootdatapath = "data",
    projnm = Project,
    pathpattern = ".csv$",
    return_availversion = F,
    return_availscen = F,
    return_availquery = F,
    versionnm = NULL,            # if NULL load all
    scennm = NULL,        # if NULL load all
    querynm = NULL,   # if NULL load all
    outputlistnm = NULL
){

  data.frame(path = list.files(path = file.path(rootdatapath, projnm),
                               recursive = T, full.names = T, pattern = pathpattern))  %>%
    mutate(path1= gsub( paste(paste0(file.path(rootdatapath, projnm), "/"), pathpattern, sep = "|") , "", path)) %>%
    separate(path1, into = c("version", "scen", "query"), sep = "/") ->
    AllQueriedCSVPath

  AllQueriedCSVPath %>% distinct(version) %>% pull -> availversion
  if(return_availversion == T){return(availversion)}
  AllQueriedCSVPath %>%
    filter(if(!is.null(versionnm)) version %in% versionnm else version %in% availversion) %>%
    distinct(scen) %>% pull -> availscen
  if(return_availscen == T){return(availscen)}
  AllQueriedCSVPath %>%
    filter(if(!is.null(versionnm)) version %in% versionnm else version %in% availversion) %>%
    filter(if(!is.null(scennm)) scen %in% scennm else scen %in% availscen) %>%
    distinct(query) %>% pull -> availquery
  if(return_availquery == T){return(availquery)}

  ## Conditional filter ----
  ## Asserting exist
  if(!is.null(versionnm)) assertthat::assert_that(all(versionnm %in% availversion))
  if(!is.null(scennm)) assertthat::assert_that(all(scennm %in% availscen))
  if(!is.null(querynm)) assertthat::assert_that(all(querynm %in% availquery))

  AllQueriedCSVPath %>%
    filter(if(!is.null(versionnm)) version %in% versionnm else version %in% availversion) %>%
    filter(if(!is.null(scennm)) scen %in% scennm else scen %in% availscen) %>%
    filter(if(!is.null(querynm)) query %in% querynm else query %in% availquery) ->
    FilteredQueriedCSVPath


  if (nrow(FilteredQueriedCSVPath) == 0) {
    stop("No input data/query available.")
  }

  ## check query after filtering ----

  FilteredQueriedCSVPath %>% distinct(query) %>% pull -> queries

  rlang::inform(paste0("Start loading data for ", length(queries), " queries : ",
                       paste(queries, collapse = " ")))


  list() -> outputlisttemp

  for (.query in queries) {

    rlang::inform(paste0("Loading ", .query))

    FilteredQueriedCSVPath %>% filter(query == .query) %>% pull(path) -> PathPerQuery

    #assign(.query, read_csv_bind(PathPerQuery), envir = get(outputlistnm, envir = .GlobalEnv))

    outputlisttemp[[.query]] <- read_csv_bind(PathPerQuery)

  }

  if (!exists(outputlistnm, envir = .GlobalEnv)) {
    # Create the list
    assign(outputlistnm, outputlisttemp, envir = .GlobalEnv)
  }else{

    assign(outputlistnm, append(get(outputlistnm, envir = .GlobalEnv), outputlisttemp), envir = .GlobalEnv)
  }

  rlang::inform("Done loading data")

}
