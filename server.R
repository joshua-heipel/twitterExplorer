# options
verbose = FALSE
cols    = c("text", "hashtags", "lang", "label", "image")
lang    = NULL

# load packages
library(shiny)
library(shinyFiles)
library(DT)
library(rtweet)

# main
searchTwitter <- function(query, n) {
  if (!is.null(query) && query != "") {
    tweets <- search_tweets(query, n = n, include_rts = FALSE, type="recent", 
                            retryonratelimit = FALSE, verbose = FALSE,
                            lang = lang)
    return(tweets)
  }
}

flatten_tweets <- function(tweets) {
  col_ids <- which(sapply(tweets, mode) == "list" & names(tweets) != "hashtags")
  tweets[col_ids] <- lapply(tweets[col_ids], function(x) 
    sapply(x, function(y) if (!is.na(y[1])) paste(y, collapse = " ") else NA))
  tweets$hashtags <- sapply(tweets$hashtags, function(x) if (!is.na(x[1])) 
    paste("#", x, sep="", collapse=" ") else NA)
  Encoding(tweets$text) <- "UTF-8"
  tweets$status_id <- paste("x", tweets$status_id, sep="")
  fotoIds <- which(tweets$media_type == "photo")
  tweets["label"] <- NA
  tweets["predicted"] <- NA
  tweets["image"] <- NA
  tweets$image[fotoIds] <- paste('<img src="', tweets$media_url[fotoIds], '">', sep="")
  return(tweets)
}

server <- shinyServer(function(input, output) {

  vals <- reactiveValues()
  vals$dirname <- "data"

  render <- function() {
    # try(rsoworder <- input$data_state$order)
    # try(print(input$data_state$order[[1]]))
    # print(roworder)
    # print(isolate(input$toList()))
    output$data <- DT::renderDataTable(vals$data[,cols],
                                       server = TRUE, 
                                       escape = FALSE,
                                       filter = "none",
                                       options = list(searching=FALSE, caseInsensitive=TRUE, smart=TRUE, 
                                                      columnDefs = list(list(targets = c(5), searchable=FALSE)),
                                                      stateSave = FALSE)
    # order = list(list(5, "desc")), 
    )
  }
    
  observeEvent(
    input$searchButton,
    {
      vals$data <- isolate(searchTwitter(input$inputQuery, as.numeric(input$number)))
      if (!is.null(vals$data) && nrow(vals$data) > 0) {
        vals$data <- flatten_tweets(vals$data)
        if (verbose) {
          View(vals$data)
        }
        render()
        # vals$proxy <- dataTableProxy("data")
      }
    },
    ignoreInit = TRUE)

  observeEvent(
    input$chooseDirectoryButton,
    {
      vals$dirname <- choose.dir(".", caption="Select folder")
      if (is.na(vals$dirname)) {
        vals$dirname <- "data"
      }
    },
    ignoreInit = TRUE)
  
  observeEvent(
    input$downloadTweetsButton,
    {
      if (!is.null(vals$data) && nrow(vals$data) > 0) {
        try(dir.create(vals$dirname, showWarnings = FALSE))
        if (file.exists(paste(vals$dirname, "tweets.csv", sep="/"))) {
          colnames = FALSE
          append = TRUE
        } else {
          colnames = TRUE
          append = FALSE
        }
        write.table(vals$data[input$data_rows_selected,-c(ncol(vals$data))], 
                    paste(vals$dirname, "tweets.csv", sep="/"), sep = ",", row.names = FALSE, 
                    fileEncoding = "UTF-8", append = append, col.names = colnames)  
      }
    },
    ignoreInit = TRUE)
  
  observeEvent(
    input$downloadImagesButton,
    {
      if (!is.null(vals$data) && nrow(vals$data) > 0) {
        try(dir.create(vals$dirname, showWarnings = FALSE))
        image_ids <- intersect(which(vals$data$media_type == "photo"),
                               input$data_rows_selected)
        if (length(image_ids)) {
          image_tweets <- vals$data[image_ids, c("status_id", "media_url")]
          apply(image_tweets, 1, function(x) try(download.file(x[2],
                                                               paste(vals$dirname, "/", x[1],".jpg",sep=""), "auto", quiet = TRUE, mode = "wb")))
        }
      }
    },
    ignoreInit = TRUE)
  
  observeEvent(input$labelButton,
  {
    if (length(input$data_rows_selected) && !is.null(vals$data) && nrow(vals$data > 0)) {
      vals$data$label[input$data_rows_selected] <- paste(unlist(input$selectedLabel), collapse = " ")
      render()
      # replaceData(vals$proxy, vals$data$label[input$data_rows_selected], resetPaging = FALSE)
      # dataTableAjax(session, vals$data[,cols], rownames = FALSE, outputId = "data")
      # reloadData(vals$proxy, resetPaging = FALSE)
      # try(print(input$data_state$order[[1]]))
    }
  },
  ignoreInit = TRUE)
})

# output$image = renderUI({
#   src = twitterData()$media_url[1]
#   tags$img(src=src, width="100px", height="auto")
# })
# 

# output$downloadData <- downloadHandler(
#   filename = function() {
#     paste(input$inputQuery, "csv", sep=".")
#   },
#   content = function(file) {
#     data <- vals$data[input$data_rows_selected,]
#     write.csv(
#       data, file, row.names = FALSE,
#       append = FALSE, fileEncoding = "UTF-8"
#     )
#   }
# )

# shinyFileChoose(input, "fileButton", roots=c(wd='.'), filetypes=c('', 'txt'))
