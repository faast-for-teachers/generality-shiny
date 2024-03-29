library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(shiny)
library(shinythemes)
library(boxr)
library(jose)

a <- read_csv("all-new-data.csv") %>%
    mutate(Why = text) %>% 
    mutate(Why = str_c(Why, " ", lesson, " ", gen_or_spec)) %>%
    mutate(row_num = row_number(),
           code = round(code),
           Why = ifelse(is.na(Why), "blank", Why))

aa <- corpus(a)
dfm_training <- dfm(aa, stem = TRUE)
m1 <- textmodel_svm(dfm_training, docvars(dfm_training, "code"))
c <- read_csv("coding-frame.csv")

box_auth_service(token_text = unlist(read_lines('boxr-auth/token.json')))

ui <- fluidPage(theme = shinytheme("united"),
                
                tags$style(HTML("thead:first-child > tr:first-child > th {
                border-top: 0;
                font-weight: normal;}
                                ")),
                titlePanel("Generality Embedded Assessment Classifier"),
                tabsetPanel(id = "maintabset",
                    tabPanel("Enter Username",
                             value = "username",
                             p(),
                             textInput("username",
                                       "Please enter your username (optional)"),
                             actionButton("next_screen", "Next tab"),
                    ),
                    tabPanel("Response Entry",
                             value = "entry",
                             p(),
                             radioButtons("checkbox",
                                          "Do you think your explanation should explain the specific phenomenon or the phenomenon in general?",
                                          choices = c("Specific", "General", "Neither/I'm unsure"),
                                          selected = ""),
                             textInput("text",
                                       "Why?"),
                             actionButton("button", "Run!")
                             #textOutput("input_confirmation")

                    ),
                    tabPanel("Classification and Feedback", 
                             value = "feedback",
                             p(),
                             textOutput("pred_code_is"),
                             p(),
                             textOutput("pred_class"),
                             br(),
                             textOutput("pred_prob_is"),
                             p(),
                             tableOutput("pred_table"),
                             p(),
                             hr(),
                             textInput("feedback", 
                                       "What do you think of the predicted code (or code probabilities)?"),
                             actionButton("feedback_button",
                                          "Enter feedback"),
                             p()
                             # textOutput("feedback_confirmation")
                    ),
                    tabPanel("More Info. and Contact",
                             value = "moreinfo",
                             h2("Thanks!"),
                             p(),
                             p("Here is additional information about this project and our contact information"),
                             tags$ul(
                                 tags$li(p("This classifier was trained on 1,021 embedded assessment responses from 6th and 7th grade students.")),
                                 tags$li(p("It was established to have satisfactory reliable for the six-code coding frame used (percentage agreement = .716; Cohen's Kappa = .621); see the results of the validation here: http://rpubs.com/jmichaelrosenberg/537982")),
                                 tags$li(p("More information can be found here: http://www.christinakrist.org/uploads/7/0/0/7/70078653/kristrosenbergicls2016revised.pdf")),
                                 tags$li(p("Source code (not including training data) is available here: https://gist.github.com/jrosen48/6b5051640975d53d2f5d3b88f8c6a3fe")),
                                 tags$li(p("Note that we log all content entered to this app (but no information who is entering the content or about you)."),
                                         tags$li(p("Please contact Christina Krist (ckrist@illinois.edu), Eric Kuo (ekuo@illinois.edu), and Joshua Rosenberg (jmrosenberg@utk.edu) with any questions about this!"))
                                 )
                             )
                             
                    )),
                p(),
                p("Please note that we save what you type into this app for research purposes. If you have any questions about this application, your response and how we are using the data generated from this app, or anything else at all, please check out the 'More Info. and Contact' tab")
                
)

server <- function(input, output, session) {
    
    output$coding_frame <- renderTable(c)
    
    observeEvent(input$next_screen, {
        updateTabsetPanel(session, 
                          "maintabset",
                          selected = "entry")
    })
    
    observeEvent(input$button, {
        updateTabsetPanel(session, 
                          "maintabset",
                          selected = "feedback")
    })
    
    observeEvent(input$feedback_button, {
        updateTabsetPanel(session, 
                          "maintabset",
                          selected = "moreinfo")
    })
    
    observeEvent(input$button, {
        
        validate(
            need(input$checkbox != "", "Did not select an option!")
        )
        
        checkbox <- input$checkbox
        text <- ifelse(input$text == "", "blank", input$text)
        
        checkbox <- ifelse(checkbox == "I'm unsure", "", checkbox)
        text <- str_c(text, " ", checkbox)
        aas <- corpus(text)
        dfm_test <- dfm(aas, stem = TRUE)
        
        dfmat_matched <- dfm_match(dfm_test, features = featnames(dfm_training))
        
        output$pred_class <- renderText({
            o <- predict(m1, newdata = dfmat_matched)
            o <- ifelse(o == 0, "A (Not Codeable)",
                        ifelse(o == 1, "B (Literal Task Goal)",
                               ifelse(o == 2, "C (Communication)",
                                      ifelse(o == 3, "D (Mechanism)",
                                             ifelse(o == 4, "E (Generality)",
                                                    ifelse(o == 5, "F (Generality & Mechanism)", NA))))))
            x <- c %>% 
                filter(Code == as.vector(o)) %>% 
                pull(Description)
            
            str_c(o, ": ", x)
            
        })
        
        output$pred_table <- renderTable( {
            o <- predict(m1, newdata = dfmat_matched, type = "probability")
            o <- as.data.frame(o)
            names(o) <- c("A (Not Codeable)", "B (Literal Task Goal)", "C (Communication)", "D (Mechanism)", "E (Generality)", "F (Generality & Mechanism)")
            o %>% 
                gather(Code, Probability) %>% 
                arrange(desc(Probability)) %>% 
                left_join(c, by = "Code") %>% 
                select(Code, Description, Probability)
        }, hover = TRUE, bordered = FALSE, striped = FALSE)
        
        output$pred_code_is <- renderText("Predicted code is:")
        
        output$pred_prob_is <- renderText("Predicted code probabilities are:")
        
    })
    
    out <- reactiveValues(data = tibble(username = "", text = "", feedback = "", time = "", session = ""))
    vals <- reactiveValues(counter = 0, counter_feedback = 0)
    vals1 <- reactiveValues(text = "")
    vals2 <- reactiveValues(counter_feedback = 0)
    vals3 <- reactiveValues(text = "")
    
    observeEvent(input$button | input$feedback_button, {
        
        if (input$button) {
            out$data$username[vals$counter] <- input$username
            out$data$text[vals$counter] <- input$text 
            out$data$time[vals$counter] <- as.character(sys.time())
            out$data$session[vals$counter] <- session$token
            
            if (vals$counter <= 0) {
                text <- "Thank you!"
            } else {
                text <- "Thank you, again!" 
            }
            
            output$input_confirmation <- renderText(text)
        }
        
        if (input$feedback_button) {
            out$data$username[vals$counter] <- input$username
            out$data$feedback[vals$counter] <- input$feedback
            out$data$time[vals$counter] <- as.character(sys.time())
            out$data$session[vals$counter] <- session$token
            
            if (vals2$counter_feedback <= 0) {
                text <- "Thank you for your feedback!"
                
            } else {
                text <- "Thank you for your feedback, again!" 
            }
            
            output$feedback_confirmation <- renderText(text)
        }
        
        vals$counter <- vals$counter + 1
        vals$counter <- vals$counter_feedback + 1
        
        # f <- str_c("logs/", "today", "-", round(runif(1) * 10000, 0), ".csv")
        
        if (!file.exists("tmp-file.csv")) {
            write_csv(out$data, "tmp-file.csv")
        }
        
        if (file.exists("tmp-file.csv")) {
            file <- read_csv("tmp-file.csv")
            bind_rows(out$data, file) %>% 
                write_csv("tmp-file.csv")
        }
        
    })
    
    onSessionEnded(function() {
        f <- str_c(Sys.time(), "-", round(runif(1) * 1000, 0), ".csv")
        data_to_write <- read_csv("tmp-file.csv")
        data_to_write <- filter(data_to_write, !is.na(session))
        boxr::box_write(data_to_write, dir_id = "119380520464", file_name = f)
        print("file written to FAAST server (on Box)")
        file.remove("tmp-file.csv")
    })
    
}

# Run the application 

shinyApp(ui = ui, server = server)
