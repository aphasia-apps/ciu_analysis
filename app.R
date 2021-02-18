##### website version ####

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
                             href = "https://github.com/rbcavanaugh/ciu_analysis",
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
                                                       min = 1, max = 1200, step = 1),
                                          div(style = "text-align:center;",
                                              actionButton("save", "Save scoring to download"),
                                              downloadButton("downloadData", "Download")
                                          )),
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
                                                  column(width = 6, align = "center",
                                                         
                                                         h4("Final Results", style = "text-align:center"),
                                                         tableOutput("final_table")
                                                         
                                                  ),
                                                  column(width = 6, align = "center",
                                                         h4("Current Sentence", style = "text-align:center"),
                                                         
                                                         tableOutput("results")
                                                         
                                                  )
                                                  
                                              )
                                          ),
                                          fluidRow("Notes:",
                                                   tags$ul(
                                                       tags$li("Currently, if there are duplicate words in a sentence, both words in the sentence above may be highlighted in red. However, only the word that is selected will count as a CIU. Fix TBD"),
                                                       tags$li("You should be able to change the transcript after you start scoring, provided you hit 'next' again after rescoring that sentence. However, these results could be off, so it may be best to copy your transcript, refresh the page, and paste it in."),
                                                       tags$li("Data entered into this app is only stored temporarily as long as you are using the app and is deleted once you close the window. Furthermore, the app will time out after 5 minutes of no use, which will also clear any entered data. Still, I do not recommend entering any clearly identifying PII. If you are concerned about this issue, you can read more about data storage in shiny apps here: https://docs.rstudio.com/shinyapps.io/Storage.html. You can also use the runGithub command (see the github link) to run the software locally, and on some computers this can be setup as a desktop shortcut.")
                                                   ),
                                                   tags$a(id = "nb",
                                                          "More information about CIU scoring can be found here", href = "https://aphasia.talkbank.org/discourse/lit/Nicholas1993.pdf", target="_blank"),
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
                            scored = list(),
                            out_cius = list(),
                            out_words = list(),
                            out_sentences = list(),
                            out_time = list(),
                            out_html = list()
    )
    
    output$count <- renderText({
        paste0("i = ", values$i)
    })
    
    observeEvent(selected(),{
        values$scored[[values$i]] = tibble(Sentence = round(values$i,0),
                                           CIUs = length(input$click_sentence),
                                           Words = nrow(sentences() %>% unnest_tokens(word, txt, to_lower = FALSE)))
        
        values$out_cius[values$i] = input$click_sentence
        values$out_words[values$i] = sentences() %>% unnest_tokens(word, txt, to_lower = FALSE)
        values$out_sentences[values$i] = sentences()
    })
    
    save_data <- eventReactive(input$save,{ 
        
        if (length(values$out_cius) ==0){return(tibble(a="You didn't enter any CIUs"))
        } else{
            
            cius = as_data_frame(t(map_dfr(values$out_cius, ~as_data_frame(t(.))))) %>%
                pivot_longer(cols = tidyselect::everything(), names_to = "sentence", values_to = "val") %>%
                mutate(sentence = str_remove(sentence, "V")) %>%
                drop_na() %>%
                arrange(sentence) %>%
                rename(item = val, sentence_num = sentence) %>%
                mutate(type = "ciu")
            
            words = as_data_frame(t(map_dfr(values$out_words, ~as_data_frame(t(.))))) %>%
                pivot_longer(cols = tidyselect::everything(), names_to = "sentence", values_to = "val") %>%
                mutate(sentence = str_remove(sentence, "V")) %>%
                drop_na() %>%
                arrange(sentence) %>%
                rename(item = val, sentence_num = sentence) %>%
                mutate(type = "word")
            
            sentences = as_data_frame(t(map_dfr(values$out_sentences, ~as_data_frame(t(.))))) %>%
                pivot_longer(cols = tidyselect::everything(), names_to = "sentence", values_to = "val") %>%
                mutate(sentence = str_remove(sentence, "V")) %>%
                rename(item = val, sentence_num = sentence) %>%
                mutate(type = "sentence")
            
            time = tibble(
                type = "time",
                item = as.character(input$time),
                sentence_num = 'seconds'
            )
            
            total_ciu = tibble(
                type = "total_ciu",
                item = as.character(nrow(cius)),
                sentence_num = NA
            )
            
            cium = tibble(
                type = "ciu_minute",
                item = as.character(nrow(cius)/input$time),
                sentence_num = NA
            )
            
            perciu = tibble(
                type = "ciu_percent",
                item = as.character(nrow(cius)/nrow(words)),
                sentence_num = NA
            )
            
            transcript = tibble(
                type = "transcript",
                item = as.character(input$type),
                sentence_num = NA
            )
            
            bind_rows(transcript, total_ciu, cium, perciu, time, cius, words, sentences)
        }
    })
    
    observeEvent(input$save,{
        if(length(values$out_cius > 0)) {
            Sys.sleep(1)
            showNotification("Ready to download", type = "message")
        } else {
            showNotification("You didn't score any CIUs!", type = "error")
        }
    })
    
    # Downloadable csv of selected dataset ----
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("downloadCIUs", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(save_data(), file, row.names = FALSE)
        }
    )
    
    
    
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
    
    output$results = renderTable({
        input$nxt
        if (length(values$scored) == 0) {return(tibble(
            Sentence = 0,
            CIUs = 0,
            Words = 0
        ))
        } else bind_rows(values$scored) %>% slice(values$i)
    }, rownames = F, striped = T, bordered = T, hover = F, align = 'c', digits = 2)
    
    output$final <- renderText({
        data = bind_rows(values$scored)
        ciu = sum(data$CIUs)
        word = sum(data$Words)
        percent = round(ciu/word*100, 2)
        ciuminute = round(ciu/(input$time/60),2)
        paste("The scored transcript includes", ciu, "Correct Information Units out of ", word, "words. This results in", percent, "% CIUs and ",ciuminute, " words per minute.")
    })
    
    output$final_table <- renderTable({
        data = bind_rows(values$scored)
        ciu = sum(data$CIUs)
        word = sum(data$Words)
        percent = if(word > 0) {round(ciu/word*100, 2)} else 0
        ciuminute = round(ciu/(input$time/60),2)
        tibble(
            CIUs = ciu,
            Words = word,
            `%CIUs` = percent,
            `CIU/min` = ciuminute
        )
        
        
    }, rownames = F, striped = T, bordered = T, hover = F, align = 'c', digits = 2)
    
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





