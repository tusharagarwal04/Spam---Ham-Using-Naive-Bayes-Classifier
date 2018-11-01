#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
require(quanteda)
require(RColorBrewer)
require(ggplot2)
require(shinyjs)
# Define UI for application that draws a histogram

ui <- dashboardPage(
  
  dashboardHeader(title="Spam and Ham"),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Dashboard",tabName="dashboard",icon=icon("dashboard")),
      menuItem("Details",tabName="details",icon=icon("list"))
    )
    
    
  ),
  dashboardBody(
    
    
    useShinyjs(),
    tags$head(tags$link(rel="stylesheet",type="text/css",href="style.css"),
              tags$script("
                          Shiny.addCustomMessageHandler('ham', function(color) {
                          document.getElementById('label').style.color = color;
                          document.getElementById('myBox').parentElement.setAttribute('class','box box-success');
                          });
                          
                          Shiny.addCustomMessageHandler('spam', function(color) {
                          document.getElementById('label').style.color = color;
                          document.getElementById('myBox').parentElement.setAttribute('class','box box-danger');
                          });
                          
                          Shiny.addCustomMessageHandler('normal', function(color) {
                          
                          document.getElementById('myBox').parentElement.setAttribute('class','box box-primary');
                          });
                          ")
              
              
              ),
    tabItems(
      tabItem(tabName="dashboard",
              
              fluidRow(
                column(
                  width=12,offset=3,
                  tags$br(),tags$br(),
                  box(
                    title="Enter a message",status="primary",solidHeader = TRUE,
                    textAreaInput(inputId = "message","",placeholder = "Enter text with more than 2 words"),
                    textOutput("Enter a message having more than 2 word"),
                    p(em("Eg-Hey,there how are you,let's meet up!?")),
                    
                    HTML('<div class="text-center">'),
                    actionButton(inputId = "btn",label="Submit",class="btn btn-primary",style="color:white"),
                    HTML('</div>')
                  )#box1
                )#column
                
              ),#fluidRow1 
              
              fluidRow(
                
                column(
                  width=12,offset=3,
                  tags$br(),
                  
                  box(
                    id="myBox",hide=TRUE,
                    status = "primary",
                    h2(class="text-center","This message is a ",strong(id="tc",textOutput(outputId = "label")))
                    
                    
                  )#box2
                  
                )#column
                
              )#fluidRow2     
              
      ),#dashboard
      
      tabItem(tabName="details",
              
              
              
              fluidRow(
                column(
                  width=12,
                  
                  #                  box(
                  
                  
                  HTML('<div class="text-justify">'),
                  h1("Naive Bayes",class="nb"),
                  h4("The model behind uses a Naive bayes classifier which is a simple conditional probabilistic model based on
                     ",strong("Bayes Theorem"),"and which uses the ",strong("feature independence assumption."),"The model was trained on a dataset of 4458 training examples
                     which consist of labels-(spam,ham) and the message itself."),
                  h4("The model achieved the accuracy of ",strong('98%')," based on which we developed this web app which can classify random unseen user based
                     text messages as either spam or ham with around 98% accuracy."),
                  
                  
                  #                 )#box2
                  HTML('</div>')
                  )#column
                
                
                ),#fluidRow1     
              fluidRow(
                column(
                  width=12,offset=3,
                  tags$br(),
                  box(
                    solidHeader = TRUE,
                    title="Data Distribution",status = "primary",
                    plotOutput(outputId="hist")
                    
                  )#box2
                )#column
                
              ),#fluidRow2   
              fluidRow(
                
                br(),br(),
                box(
                  solidHeader = TRUE,collapsible = TRUE,
                  title="HAM Wordcloud",status = "primary",
                  plotOutput(outputId="hamw")
                  
                ),#box2
                
                
                
                box(
                  solidHeader = TRUE,collapsible = TRUE,
                  title="SPAM Wordcloud",status = "primary",
                  plotOutput(outputId="spamw")
                  
                )#box2
                
              )#fluidRow3
              
              
              
      )#Details
      
      
      
      
    ),#tabItems
    br(),br(),
    HTML('<div class="text-center ft">'),
    "Made with",
    HTML('<span class="ht">'),
    icon("heart"),
    HTML('</span>'),
    " by Group 24 in India.",
    HTML('</div>')
    
    
    
    )#Body
  
  )#Page

library("quanteda")
spam<-read.csv("spam.csv",header=TRUE, sep=",", quote='\"\"', stringsAsFactors=FALSE)
set.seed(2012)
spam<-spam[sample(nrow(spam)),]
names(spam)<-c("type","message")
#to search more on this method
msg.corpus<-corpus(spam$message)
docvars(msg.corpus)<-spam$type   #ataching the label to the corpus message text
spam.train<-spam[1:4458,]
msg.dfm <- dfm(msg.corpus, tolower = TRUE)  #generating document freq matrix
msg.dfm <- dfm_trim(msg.dfm, min_count = 5, min_docfreq = 3)  
msg.dfm <- dfm_weight(msg.dfm)
#trining and testing data of dfm 
msg.dfm.train<-msg.dfm[1:4458,]
#training the naive-bayes classifier
nb.classifier<-textmodel_nb(msg.dfm.train,spam.train[,1])



spam<-read.csv("spam.csv",header=TRUE, sep=",", quote='\"\"', stringsAsFactors=FALSE)

table(spam$v1)


#checking the distribution of type of messages
theme_set(theme_bw())
#theme_set(theme_bw())

server <- function(input, output,session) {
  
  
  
  #print the entered message to console each time the submit button is pressed to check
  observeEvent(input$btn, {
    
    cat("\nEntered Text:", input$message,"\n")
  })
  observeEvent(is.null(input$message),{
    session$sendCustomMessage("normal",'primary')
    hideElement(id='myBox')
    
  })
  
  #take an action whenever a button is pressed
  class<-eventReactive(input$btn,{
    
    
    #extarct corpus from the user entered text message
    msg=corpus(input$message)
    
    ms.dfm<-dfm(msg,tolower=TRUE,stem=TRUE)
    ms.dfm=dfm_select(ms.dfm,msg.dfm.train)
    #predicting on the entered text by user
    class=predict(nb.classifier,newdata=ms.dfm)
    #feed processed user entered text to predict the class
    #printing the class label
    
    toupper(class[1])
    #print("Hiii")
    
    
  })    
  
  output$label <-renderText({
    if(is.null(input$message)) return()
    if(class()=="HAM")
    {
      tags$style(
        "#tc{color:green;}"
      )
      
    }#if
    else{
      
      tags$style(
        "#tc{color:red;}"
      )
      
    }
    
    if(class()=="HAM")
    {
      
      print("INTIT")
      print(class())
      # Listen for background-color messages
      #tags$script("document.body.style.backgroundColor = green;")
      session$sendCustomMessage("ham",'green')
      #"document.getElementById('label').setAttribute('class','ham')"
    }
    #calling the event function class() from above to print spam or ham after hitting submit button
    else
    {
      session$sendCustomMessage("spam",'red')
      
    }
    class()
  })
  output$hist<-renderPlot({
    ggplot(aes(x=v1),data=spam) +
      geom_bar(fill ="#2356a8",width=0.5)
    
  }
  
  )#reb=nderPlot
  output$spamw<-renderPlot({
    names(spam)<-c("type","message")
    head(spam)
    
    
    set.seed(2012)
    spam<-spam[sample(nrow(spam)),]
    
    ?corpus #to search more on this method
    msg.corpus<-corpus(spam$message)
    docvars(msg.corpus)<-spam$type   #ataching the label to the corpus message text
    
    
    #subsetting only the spam messages
    spam.plot<-corpus_subset(msg.corpus,docvar1=="spam")
    
    #now creating a document-feature matrix using dfm()
    spam.plot<-dfm(spam.plot, tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE, remove=stopwords("SMART"))
    
    spam.col <- brewer.pal(10, "BrBG")  
    
    textplot_wordcloud(spam.plot, min.freq = 16, color = spam.col,scale=c(7,0.7))  
    #wordcloud_rep<-repeatable(wordcloud)
    
    
    
  })
  output$hamw<-renderPlot({
    names(spam)<-c("type","message")
    head(spam)
    
    
    set.seed(2012)
    spam<-spam[sample(nrow(spam)),]
    
    ?corpus #to search more on this method
    msg.corpus<-corpus(spam$message)
    docvars(msg.corpus)<-spam$type   #ataching the label to the corpus message text
    ham.plot<-corpus_subset(msg.corpus,docvar1=="ham")
    ham.plot<-dfm(ham.plot,tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE,remove=c("gt", "lt", stopwords("smart")))
    ham.col=brewer.pal(10, "BrBG")  
    textplot_wordcloud(ham.plot,min.freq=16,colors=ham.col,scale=c(5,0.5))
    
  })
  observeEvent(input$btn,{
    toggle(id="myBox")
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

