# options
verbose   = FALSE
cols      = c('status_id', 'created_at', 'user_id', 'screen_name', 'text', 'source', 'reply_to_status_id', 'reply_to_user_id', 'reply_to_screen_name', 'is_quote', 'is_retweet', 'favorite_count', 'retweet_count', 'hashtags', 'symbols', 'urls_url', 'urls_t.co', 'urls_expanded_url', 'media_url', 'media_t.co', 'media_expanded_url', 'media_type', 'ext_media_url', 'ext_media_t.co', 'ext_media_expanded_url', 'ext_media_type', 'mentions_user_id', 'mentions_screen_name', 'lang', 'quoted_status_id', 'quoted_text', 'retweet_status_id', 'retweet_text', 'place_url', 'place_name', 'place_full_name', 'place_type', 'country', 'country_code', 'geo_coords', 'coords_coords', 'bbox_coords', 'label', 'predicted', 'image')
select = c('created_at', 'text', 'hashtags', 'lang', 'label', 'image')
language  = NULL

library(shiny)
library(DT)
library(rtweet)
library(readr)

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


shinyApp(
  ui = fluidPage(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
    ),
    
    titlePanel(
      h1("Shiny Twitter Explorer 0.15"),
      windowTitle = "Twitter Explorer 0.15"
    ),
    
    sidebarLayout(
      position = "left",
      
      sidebarPanel(
        textInput(inputId = "inputQuery", label = NULL, placeholder = "<Search query>", width="100%"),
        selectizeInput(inputId = "selectedLabel", label = NULL,
                       choices = list(biology = c("animals", "plants")),
                       multiple = FALSE, size = 50,
                       options = list(
                         placeholder = "<Select label>",
                         create = TRUE,
                         onInitialize = I('function() { this.setValue(""); }')
                       ), width="60%"),
        actionButton("search", "Search Tweets"),
        actionButton("update", "Label"),
        actionButton("clear", "Clear Selection"),
        p(sliderInput(inputId = "number", label = "Number of Tweets", min = 1, max = 1000, value = 100))
        , width=8
        ),
      
      mainPanel(
        DT::dataTableOutput(outputId = "DTtable", width="100%", height="auto"),
        p(
        actionButton(inputId = "chooseDirectoryButton", label = "Choose directory"),
        actionButton(inputId = "downloadTweetsButton", label = "Download Tweets"),
        actionButton(inputId = "downloadImagesButton", label = "Download Images"),
        actionButton(inputId = "importButton", label = "Import Tweets")),
        width = 12
    ))
  ), server = function(input, output, session) {
    
    data <- reactiveValues(table = data.frame(t(matrix("", length(cols), 1, dimnames=list(cols)))), folder = NA)
    
    # Reactive expression to compose a data frame containing all of
    # the values
    search <- reactive({
      input$search
      if (input$search) {
        # isolate(data$table <- data.frame(t(matrix(1:500, length(cols), input$number, dimnames=list(cols)))))
        isolate(data$table <- searchTwitter(input$inputQuery, as.numeric(input$number))) 
      }
    })
    
    label <- reactive({
      input$update
      isolate(if (input$update && !is.null(input$selectedLabel)){
        data$table$label[input$DTtable_rows_selected] <- paste(input$selectedLabel, collapse=" ")
        data$table
      } else {
        data$table
      })
    })

    clear <- reactive({
      input$clear
      if (input$clear) selectRows(proxy, NULL)
    })
        
    # Show the values using an HTML table
    output$DTtable = DT::renderDataTable(
      escape = FALSE,
      options = list(searching=FALSE, caseInsensitive=TRUE),
      rownames = FALSE, isolate({
      label()[select]
    }))
    
    proxy = dataTableProxy('DTtable')
    observe({
      replaceData(proxy, label()[select], rownames = FALSE, resetPaging = FALSE, clearSelection="none")
    })
    observe({
      replaceData(proxy, search()[select], rownames = FALSE, resetPaging = FALSE, clearSelection="none")
    })
    observe({
      clear()
    })
    observe({
      replaceData(proxy, import()[select], rownames = FALSE)
    })
    
    observeEvent(
      input$chooseDirectoryButton,
      {
        # manually select a folder
        data$folder <- choose.dir("", caption="Select folder")
      },
      ignoreInit = TRUE
    )
    
    observeEvent(
      input$downloadTweetsButton,
      {
        
        # check if a directory has already been chosen
        if(is.na(data$folder)) {
          data$folder <- choose.dir("", caption="Select folder")
        }

        isolate({
          if (!is.null(data$table) && nrow(data$table)) {
            
            # write tweets
            dat <- data$table[input$DTtable_rows_selected,-c(ncol(data$table))]
            writeTweets(data$folder, dat)
          }
        })
        
      },
      ignoreInit = TRUE
    )
    
    observeEvent(
      input$downloadImagesButton,
      {
        
        # check if a directory has already been chosen
        if(is.na(data$folder)) {
          data$folder <- choose.dir("", caption="Select folder")
        }
        
        isolate({
          if (!is.null(data$table) && nrow(data$table)) {
            
            # get row_ids of tweets that contain a photo
            image_ids <- intersect(which(data$table$media_type == "photo"),
                                   input$DTtable_rows_selected)
            
            if (length(image_ids)) {
              writeImages(data$folder, data$table[image_ids, c("status_id", "media_url")])
            }
          }
        })
      },
      ignoreInit = TRUE
    )
    
    import <- reactive({
      input$importButton
      if (input$importButton) {
        filename <- choose.files("", caption="Select file", multi = FALSE)
        if(!is.na(filename)) {
          dat <- read_csv(filename)
          data$table <- flattenTweets(dat)[cols]
        }
      }
    })
  })