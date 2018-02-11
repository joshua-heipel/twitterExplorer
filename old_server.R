# options
verbose   = FALSE
cols      = c("text", "hashtags", "lang", "label", "image")
language  = NULL

# load packages
library(shiny)
library(shinyFiles)
library(DT)
library(rtweet)

searchTwitter <- function(query, n) {

  if (!is.null(query) && query != "") {

    # search for tweets for the given query, number n, and language
    tweets <- search_tweets(query, n = n, include_rts = FALSE, type="recent",
                            retryonratelimit = FALSE, verbose = FALSE,
                            lang = language)

    return(flattenTweets(tweets))
  }
}

flattenTweets <- function(tweets) {

  if (is.null(tweets)) return()

  # search columns that contain lists instead of single values
  col_ids <- which(sapply(tweets, mode) == "list" & names(tweets) != "hashtags")

  # paste values to a string
  tweets[col_ids] <- lapply(tweets[col_ids], function(x)
    sapply(x, function(y) ifelse(is.na(y[1]), NA , paste(y, collapse = " "))))

  # paste column 'hashtags' to single string with leading #s
  tweets$hashtags <- sapply(tweets$hashtags, function(x) ifelse(is.na(x[1]),
    NA, paste("#", x, sep="", collapse=" ")))

  # prepend ids (see ?write_as_csv)
  tweets$status_id <- paste("x", tweets$status_id, sep="")

  # add columns for label and prediction
  tweets["label"] <- NA
  tweets["predicted"] <- NA

  # construct image urls
  tweets["image"] <- NA
  fotoIds <- which(tweets$media_type == "photo")
  tweets$image[fotoIds] <- paste('<a href="', tweets$media_url[fotoIds], '"><img src="', tweets$media_url[fotoIds], '"></a>', sep="")

  # remove eof
  tweets$text <- gsub("\n", " ", tweets$text)
  
  # inspect data in R if verbose
  if (verbose) {
    View(tweets)
  }

  return(tweets)
}

writeTweets <- function(dir, data) {
  
  # handle user interruption
  if(is.na(dir)) return()

  # create directory
  if(!dir.exists(dir)) dir.create(dir, showWarnings = FALSE)

  # append tweets to an existing file
  if (file.exists(paste(dir, "tweets.csv", sep="/"))) {
    colnames = FALSE
    append = TRUE
  } else {
    colnames = TRUE
    append = FALSE
  }
  
  # write file
  write.table(data, paste(dir, "tweets.csv", sep="/"), sep = ",", row.names = FALSE,
              fileEncoding = "UTF-8", append = append, col.names = colnames)
}

writeImages <- function(dir, image_tweets) {

  # handle user interruption
  if(is.na(dir)) return()
  
  # create directory
  if(!dir.exists(dir)) dir.create(dir, showWarnings = FALSE)
  
  # download images
  apply(image_tweets, 1, function(x) try(download.file(x[2],
    paste(dir, "/", x[1],".jpg",sep=""), "auto", quiet = TRUE, mode = "wb")))
}

server <- shinyServer(function(input, output) {

  vals <- reactiveValues()
  folder <- NA

  # observer to render the data returned by twitter API call
  observe({

    # only render if data available
    if (!is.null(vals$tweets) && nrow(vals$tweets)) {

      # side effect instead of return value
      output$data <- DT::renderDataTable(
        vals$tweets[,cols], server = TRUE, escape = FALSE, filter = "none", #extensions = c("Scroller", "Responsive"),
        options = list(searching=FALSE, caseInsensitive=TRUE, smart=TRUE,
                       columnDefs = list(list(targets = c(5), searchable=FALSE)),
                       stateSave = FALSE) 
                       # deferRender = TRUE, scrollY = 500,scroller = TRUE)
      )
    }
  })

  
  # render <- function() {
  #   # try(rsoworder <- input$data_state$order)
  #   # try(print(input$data_state$order[[1]]))
  #   # print(roworder)
  #   if (is.null(isolate(input$data_rows_all))) {
  #     roworder <- 1:nrow(vals$data)
  #   } else {
  #     roworder <- input$data_rows_all
  #   }
  #   print(isolate(input$data_rows_all))
  # 
  #   # order = list(list(5, "desc")), 
  # 
  # }

  # conductor to call twitter search function
  observeEvent(
    input$searchButton,
    {
      # only react to search button
      isolate({
        vals$tweets <- searchTwitter(input$inputQuery, as.numeric(input$number))  
      })
    },
    ignoreInit = TRUE
  )

  observeEvent(input$labelButton,
    {
      if (length(input$data_rows_selected) && !is.null(vals$tweets) && nrow(vals$tweets)) {
        
        # only react to label button
        isolate({
          vals$tweets$label[input$data_rows_selected] <- paste(unlist(input$selectedLabel), collapse = " ")
        })
        # replaceData(vals$proxy, vals$data$label[input$data_rows_selected], resetPaging = FALSE)
        # dataTableAjax(session, vals$data[,cols], rownames = FALSE, outputId = "data")
        # reloadData(vals$proxy, resetPaging = FALSE)
        # try(print(input$data_state$order[[1]]))
      }
    }
  )
  
  observeEvent(
    input$chooseDirectoryButton,
    {
      # manually select a folder and create it
      folder <<- choose.dir("", caption="Select folder")
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$downloadTweetsButton,
    {
      
      # check if a directory has already been chosen
      if(!is.character(folder)) {
        folder <<- choose.dir("", caption="Select folder")
      }
      
      isolate({
        if (!is.null(vals$tweets) && nrow(vals$tweets)) {

          # write tweets
          writeTweets(folder, vals$tweets[input$data_rows_selected,-c(ncol(vals$tweets))])
        }
      })

    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$downloadImagesButton,
    {
      
      # check if a directory has already been chosen
      if(is.na(folder)) {
        folder <<- choose.dir("", caption="Select folder")
      }
      
      isolate({
        if (!is.null(vals$tweets) && nrow(vals$tweets)) {
          
          # get row_ids of tweets that contain a photo
          image_ids <- intersect(which(vals$tweets$media_type == "photo"),
                                 input$data_rows_selected)

          if (length(image_ids)) {
            writeImages(folder, vals$tweets[image_ids, c("status_id", "media_url")])
          }
        }
      })
    },
    ignoreInit = TRUE
  )

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
