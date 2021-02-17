## Example shiny app with bucket list

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

# First specify the packages of interest
packages = c("tidyverse", "shiny",
             "tableHTML", "shinyWidgets", "tidytext",
             "tokenizers", "shinythemes", "shinydashboard", "DT")

## Now load or install&load all
package.check <- lapply(
    packages,
    FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
        }
    }
)

library(shiny)
library(tidyverse)
library(tableHTML)
library(shinyWidgets)
library(tidytext)
library(tokenizers)
library(shinythemes)
library(shinydashboard)
library(DT)

ui <- fluidPage(
  useShinydashboard(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$title("CIU Analysis")
  ),
  navbarPage(theme = shinytheme("flatly"), 
             title = div(a(icon("github"),
                           href = "https://github.com/rbcavanaugh/clinical-discourse",
                           id = "img-id",
                           target = "_blank")),
             tabPanel("Scoring",
                      #fluidRow(
                      sidebarLayout(fluid = T,
                                    sidebarPanel(
                                      h4("Correct Information Unit Scoring"),
                                      p("Enter Transcript in the box below using established transcription rules for CIUs. Make sure to use periods and capitalize the first word of each sentence so that the transcript is split into sentences effectively."),
                                      textAreaInput("type",
                                                    label = "Transcript:",
                                                    value = "Young boy is practicing playing soccer. Kicking the ball up and keeping it in the air. He miskicks. It fall goes and breaks the window of his house. Of the living room actually. And bounces into the living room knocking a lamp over where his father is sitting. The father picks up the soccer ball. Looks out the window. And calls for the little boy to come and explain.",
                                                    height = '325px', width = "100%"
                                      ),
                                      numericInput("time", "Time in seconds", value= 120,
                                                   min = 1, max = 1200, step = 1)),
                                    mainPanel(
                                      fluidRow(
                                        column(
                                          width = 12
                                        ),
                                        box(width = NULL, style = "text-align:center",
                                            htmlOutput("txtOut", style = "font-size: 2rem;"), br(),
                                            uiOutput("choices"),
                                            uiOutput("coded_sentence1"),
                                            br(),
                                            actionButton("prev", "Previous Sentence", width = "150px"),
                                            actionButton("nxt", "Next Sentence", width = "150px")
                                        ),
                                        box(width = NULL, #height = "200px",
                                            column(width = 6,
                                                   h4("Final Results"),
                                                   DTOutput("final_table")
                                            ),
                                            column(width = 6,    
                                                   h4("Sentence Scores"),
                                                   DTOutput("results")
                                            )
                                            
                                        )
                                      ),
                                      fluidRow("Summary",
                                               textOutput("final"), br(),
                                               tags$a(id = "nb",
                                                      "More information about CIU scoring can be found here", href = "https://aphasia.talkbank.org/discourse/lit/Nicholas1993.pdf", target="_blank"),br(),
                                               h5("Notes: "),
                                               tags$ul(
                                                 tags$li("Currently, if there are duplicate words in a sentence, selecting one of the words will only count for one CIU. However, both words in the sentence above may be highlighted in red."),
                                                 tags$li("You should be able to change the transcript after you start scoring, provided you hit 'next' again after rescoring that sentence. However, these results could be off, so it may be best to copy your trasncript, refresh the page, and paste it in."),
                                                 tags$li("Data entered into this app is only stored temporarily as long as you are using the app and is deleted once you close the window. Furthermore, the app will time out after 5 minutes of no use, which will also clear any entered data. If you are running this app using runGithub(), the application is running on your local computer. The runGithub() function simply downloads the code from github.com and runs it automatically.")
                                               )
                                               
                                      )
                                    )
                      )
                      
             ),
             tabPanel("About",
                      fluidRow(
                        p("This web-app is free and open-source. You can view the underlying code using the github link at the top right of the page. I can't promise I didn't goof somewhere writing the calculations. Please feel free to contact me with any questions or feedback."),
                        p("- Rob"),
                        a(id = "contact", href = "rob.cavanaugh.com.", "You can find my information here.", target = "_blank"),
                        div(style = "text-align:center;",
                            actionButton("button", "Send Feedback")
                        )
                      )
             )
             
  )
)

server <- function(input,output) {
  
  values = reactiveValues(i=1,
                          scored = list())
  
  output$count <- renderText({
    paste0("i = ", values$i)
  })
  
  observeEvent(selected(),{
    # input$save
    #isolate({
    values$scored[[values$i]] = tibble(Sentences = round(values$i,0),
                                       CIUs = length(input$click_sentence),
                                       Words = nrow(sentences() %>% unnest_tokens(word, txt, to_lower = FALSE)))
    # })
  })
  
  
  observe({
    input$nxt
    isolate({
      values$i <- values$i + 1
    })
  })
  
  observe({
    input$prev
    isolate(values$i <- values$i - 1)
  })
  
  output$sen <- renderUI({
    paste0("Sentence", values$i, ": ", sentences()$txt[1])
  })
  
  sentences <- reactive({
    df = tibble(txt = unlist(tokenize_sentences(input$type))) %>%
      slice(values$i) 
  })
  
  output$choices <- renderUI({
    checkboxes = sentences() %>%
      unnest_tokens(word, txt, to_lower = FALSE)
    
    choices = (checkboxes$word)
    
    checkboxGroupButtons("click_sentence", "Click on a word to mark as a CIU; CIUs will turn red.", choices = choices)
  })
  
  selected = reactive({
    input$click_sentence
  })
  
  
  output$txtOut <- renderText({ 
    # Split into words
    df_words <- sentences() %>%
      unnest_tokens(word, txt, to_lower = FALSE)
    words = df_words$word
    
    outTxt <- ' '
    
    # Parse input based on logic
    if(values$i < 1) return("You've gone back too far! Hit next.")
    if(length(words) == 0) return('Nice job! All Done. ')
    
    
    # Loop trough words
    for (i in 1:length(words)){
      curr.word <- words[i]
      
      # Determine formating
      if (curr.word %in% selected() ){
        font <- 'red'
      } else {
        font <- 'black'
      }
      
      # Create html
      formatedFont <- sprintf('<font color="%s">%s</font>',font,curr.word) 
      
      # Append to text to show
      outTxt <- paste(outTxt, formatedFont,collapse=' ')
      
      
    }
    paste0("Sentence ", values$i, ": ", outTxt)
  })
  
  output$results = renderDT({
    if (length(values$scored) == 0) {return(tibble(
      Sentences = 0,
      CIUs = 0,
      Words = 0
    ))
    } else bind_rows(values$scored)
  }, rownames = FALSE, options = list(dom = 't',
                                      #ordering = FALSE,
                                      scrollY = "12vh",
                                      scroller = TRUE,
                                      fixedColumns = list(heightMatch = 'none'),
                                      scrollCollapse = TRUE,
                                      paging = FALSE,
                                      columnDefs = list(list(className = 'dt-center dt-bottom')))
  )
  
  output$final <- renderText({
    data = bind_rows(values$scored)
    ciu = sum(data$cius)
    word = sum(data$words)
    percent = round(ciu/word*100, 1)
    ciuminute = ciu/(input$time/60)
    paste("The scored transcript includes", ciu, "Correct Information Units out of ", word, "words. This results in", percent, "% CIUs and ",ciuminute, " words per minute.")
  })
  
  output$final_table <- renderDT({
    data = bind_rows(values$scored)
    ciu = sum(data$cius)
    word = sum(data$words)
    percent = if(word > 0) {round(ciu/word*100, 1)} else 0
    ciuminute = ciu/(input$time/60)
    tibble(
      CIUs = ciu,
      Words = word,
      `%CIUs` = percent,
      `CIU/min` = ciuminute
    )
    
  }, options = list(dom = '', ordering = FALSE), rownames= FALSE)
  
  observeEvent(input$button, {
    showModal(
      modalDialog(
        title = "Feedback",
        tags$iframe(src = 'https://docs.google.com/forms/d/e/1FAIpQLSfrae3ucppQC_Hy2udxj5_xZwRqbkHwTzUX6PQEnpUdahAb4g/viewform?usp=sf_link',
                    width = '100%',
                    height = 500,
                    frameborder = 0,
                    marginheight = 0),
        easyClose = TRUE,
        size = "l"
      ))
  })
}


shinyApp(ui, server)





