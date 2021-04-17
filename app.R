#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# devtools::install_github("qinwf/cidian") #用来从github上安装R包的函数，“qinwf”应该是cidian的作者

library(shiny)
library(shinyjs)
# library(cidian)
library(wordcloud2)
library(xml2)
library(httr)
library(rvest)
library(stringr)
library(wordcloud)
if(!require(nytimes)) devtools::install_github("mkearney/nytimes")
if(!require(jsonlite)) install.packages("jsonlite")
#load library
if (!require("tm")) install.packages("tm")
library(tm)
library(jsonlite)
library(nytimes)


# API authorization
NYTIMES_KEY <- "BhVS4rx58dN1iJ83sP67O2tOrf7nfPNR"
Sys.setenv(NYTIMES_KEY="BhVS4rx58dN1iJ83sP67O2tOrf7nfPNR")


options(shiny.usecairo = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Word Cloud for NewYork Times"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("keyword_text","Key Word:", value = "China"),
            textInput("keyword_year","Year:", value = "2021"),
            actionButton("sure", "Generate Word Cloud"),
            
        ),
        
        

        # Show a plot of the generated word cloud
        mainPanel(
            htmlOutput("caption", container = div),
            wordcloud2Output('wordcloud'),
            h5("Author: Jihong Zhang", align = "right"),
            h5("HACKUIOWA 2021", align = "right")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

      observeEvent(input$sure, {
            
        ## get data for search API
        getNYT <- function(term, year) {
          begindate = paste0(year, "0101")
          enddate = paste0(year, "1231")
          baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                            "&begin_date=",begindate,"&end_date=",enddate,
                            "&facet_filter=true&api-key=", NYTIMES_KEY, sep="")
          
          initialQuery <- fromJSON(baseurl, flatten = T)
          maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 
          
          #try with the max page limit at 10
          maxPages = ifelse(maxPages >= 20, 10, maxPages)
          
          pages <- list()
          for(i in 0:maxPages){
            nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE)
            message("Retrieving page ", i)
            
            temp = data.frame(id=1:nrow(nytSearch$response$docs),
                              created_time = nytSearch$response$docs$pub_date,
                              snippet = nytSearch$response$docs$snippet,
                              headline = nytSearch$response$docs$headline.main)
            
            pages[[i+1]] <- temp 
            Sys.sleep(5) 
          }
          
          df = rbind_pages(pages)
          return(df)
        }
        
        #preprocessing function for text
        preprocessing = function (doc){
          doc = gsub("[^[:alnum:]]"," ",doc)
          #create corpus
          corpus = Corpus(VectorSource(doc))
          #Removal of punctuation
          corpus = tm_map(corpus, removePunctuation)
          #Removal of numbers
          corpus = tm_map(corpus, removeNumbers)
          #Conversion to lower case
          corpus = tm_map(corpus, content_transformer(tolower)) 
          #customize my stopwords
          mystopword = "Here’s what you need to know to start your day"
          #Removal of stopwords
          corpus = tm_map(corpus, removeWords, c(stopwords("english"),mystopword))
          #retun result
          return(corpus)
        }
        
        term.dt <- getNYT(input$keyword_text, input$keyword_year)
        term.clean <- preprocessing(term.dt$snippet)
        
        term.clean.dt <- data.frame(text = sapply(term.clean, as.character), stringsAsFactors = FALSE)
        
        mixseg <- worker()
        wordsC = segment(term.clean.dt$text, mixseg)
        filter = stopwords("SMART")
        
        wordsC <- filter_segment(wordsC, filter)
        
        wordsC <- as.data.frame(table(wordsC)[order(-as.numeric(table(wordsC)))])
        # output$caption <- renderUI({ 
        #   title_name <- lapply(html.content, function(x) {
        #     as.character(xml_attr(xml_find_all(x, xpath = "//h1"), "title"))}) 
        #   # return(paste0(unlist(title_name), collapse = "\t" ))
        #   HTML(paste0(unlist(title_name), sep = '<br/>'))
        # })
        
        
        
        output$wordcloud <- renderWordcloud2({
            # wordcloud(words = word, freq = Freq,
            #           col = brewer.pal(8, "Set1"), random.color = T, 
            #           random.order = T,  scale = c(3, 1), family="STKaiti")                  
            wordcloud2(wordsC, 
                       color = "random-dark", 
                       backgroundColor = "white",
                       fontFamily = "Roboto Condensed",
                       fontWeight = "bold",
                       size = 2,
                       # shape = c("circle", "cardioid", "diamond", "triangle-forward",
                       #"triangle", "pentagon", "star")[sample(x = 1:5, size = 1)]
                       shape = "pentagon"
                       ) # 先排序再做词云
        })
      })

}

# Run the application 
shinyApp(ui = ui, server = server)
