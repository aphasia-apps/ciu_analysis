## Example shiny app with bucket list

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
packages = c("tidyverse", "shiny",
             "tableHTML", "shinyWidgets", "tidytext",
             "tokenizers")

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

ui <- fluidPage(
    tags$head(
        tags$style(type="text/css", "#coded_sentence {word-break: break-all;} html {padding: 5%}")
    ),
    fluidRow(
        h2("Correct Information Unit Scoring"), br(),
        textAreaInput("type", "Enter Transcript here using established transcription rules for CIUs. Make sure to use periods and capitalize the first word of each sentence.", value = "Young boy is practicing playing soccer. Kicking the ball up and keeping it in the air. He miskicks. It fall goes and breaks the window of his house. Of the living room actually. And bounces into the living room knocking a lamp over where his father is sitting. The father picks up the soccer ball. Looks out the window. And calls for the little boy to come and explain.",
                      height = '150px', width = "100%"
        )),
    fluidRow(
        column(
            width = 12,
            br(),
            htmlOutput("txtOut"),br(),
            uiOutput("choices"),
            uiOutput("coded_sentence1"),
            actionButton("prev", "Previous Sentence"),
            actionButton("nxt", "Next Sentence"),
            br(),
            br(),
            h4("Results by Sentence"),
            tableOutput("results"),
            br(),
            br(),
            h4("Final Results"),
            textOutput("final"),
            tableOutput("final_table")
            
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
        values$scored[[values$i]] = tibble(sentence = values$i,
                                           cius = length(input$click_sentence),
                                           words = nrow(sentences() %>% unnest_tokens(word, txt, to_lower = FALSE)))
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
        
        choices = unique(checkboxes$word)
        
        checkboxGroupButtons("click_sentence", "Click on a box to mark as a CIU. CIUs will be red.", choices = choices)
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
    
    output$results = renderTable({
        if (length(values$scored) == 0) {return("Get started by scoring CIUs")
        } else bind_rows(values$scored)
    })
    
    output$final <- renderText({
        data = bind_rows(values$scored)
        ciu = sum(data$cius)
        word = sum(data$words)
        percent = round(ciu/word*100, 1)
        paste("This transcript included", ciu, "Correct Information Units out of ", word, "words. This results in", percent, "% CIUs.")
    })
    
    output$final_table <- renderTable({
        data = bind_rows(values$scored)
        ciu = sum(data$cius)
        word = sum(data$words)
        percent = round(ciu/word*100, 1)
        tibble(
            CIUs = ciu,
            Words = word,
            Percent.CIUs = percent
        )
        
    })
    
}


shinyApp(ui, server)





