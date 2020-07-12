library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(shiny)
library(shinythemes)
library(shinylogs)
library(aws.s3)

source("token.R")

# clear out bucket

# list.files("logs", full.names = T) %>%
#     map(aws.s3::delete_object, bucket = s3BucketName,
#         key=AWS_ACCESS_KEY_ID,
#         secret=AWS_SECRET_ACCESS_KEY,
#         region=AWS_DEFAULT_REGION)

a <- read_csv("all-new-data.csv") %>%
    mutate(Why = text) %>% 
    mutate(Why = str_c(Why, " ", lesson, " ", gen_or_spec)) %>%
    mutate(row_num = row_number(),
           code = round(code),
           Why = ifelse(is.na(Why), "blank", Why))

aa <- corpus(a)
dfm_training <- dfm(aa, stem = TRUE)
m1 <- textmodel_svm(dfm_training, docvars(dfm_training, "code"))
counter <- 0
c <- read_csv("coding-frame.csv")

ui <- fluidPage(theme = shinytheme("united"),
                
                use_tracking(),
                
                tags$style(HTML("thead:first-child > tr:first-child > th {
                border-top: 0;
                font-weight: normal;}
                                ")),
                titlePanel("Generality Embedded Assessment Classifier"),
                tabsetPanel(
                    tabPanel("Response Entry",
                             p(),
                             radioButtons("checkbox",
                                          "Do you think your explanation should explain the specific phenomenon or the phenomenon in general?",
                                          choices = c("Specific", "General", "Neither/I'm unsure"),
                                          selected = ""),
                             textInput("text",
                                       "Why?"),
                             actionButton("button", "Run!"),
                             textOutput("need_to_select"),
                             
                             p(),
                             hr(),
                             textOutput("pred_code_is"),
                             p(),
                             textOutput("pred_class"),
                             br(),
                             textOutput("pred_prob_is"),
                             p(),
                             tableOutput("pred_table")
                    ),
                    tabPanel("Feedback", 
                             p(),
                             textInput("feedback", 
                                       "What do you think of the predicted code (or code probabilities)?"),
                             actionButton("feedback_button",
                                          "Enter feedback"),
                             p(),
                             textOutput("feedback_confirmation")
                    ),
                    tabPanel("More Info. and Contact",
                             p(),
                             p("Here is additional information and our e-mail addresses:"),
                             tags$ul(
                                 tags$li(p("This classifier was trained on 1,021 embedded assessment responses from 6th and 7th grade students.")),
                                 tags$li(p("It was established to have satisfactory reliable for the six-code coding frame used (percentage agreement = .716; Cohen's Kappa = .621); see the results of the validation here: http://rpubs.com/jmichaelrosenberg/537982")),
                                 tags$li(p("More information can be found here: http://www.christinakrist.org/uploads/7/0/0/7/70078653/kristrosenbergicls2016revised.pdf")),
                                 tags$li(p("Source code (not including training data) is available here: https://gist.github.com/jrosen48/6b5051640975d53d2f5d3b88f8c6a3fe")),
                                 tags$li(p("Note that we log all content entered to this app (but no information who is entering the content or about you)."),
                                         tags$li(p("Please contact Joshua Rosenberg (jmrosenberg@utk.edu) and Christina Krist (ckrist@illinois.edu) with any questions about this!"))
                                 )
                             )
                             
                    ))
                
)

server <- function(input, output) {
    
    track_usage(storage_mode = store_json(path = "logs/"))
    
    output$coding_frame <- renderTable(c)
    
    observeEvent(input$button, {
        
        validate(
            need(input$checkbox != "", "Did not select an option!")
        )
        
        print(str_c("***** user entered the following: ", input$text, " ", input$checkbox))
        
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
                left_join(c) %>% 
                select(Code, Description, Probability)
        }, hover = TRUE, bordered = FALSE, striped = FALSE)
        
        output$pred_code_is <- renderText("Predicted code is:")
        
        output$pred_prob_is <- renderText("Predicted code probabilities are:")
        
    })
    
    # observeEvent(input$button, {
    # 
    #     if(input$checkbox != "") {
    #         text <- "Did not select an option!"
    #     }
    # 
    #     output$need_to_select <- renderText(text)
    # })
    
    
    observeEvent(input$feedback_button, {
        
        if (counter <= 0) {
            text <- "Thank you for your feedback!"
        } else {
            text <- "Thank you for your feedback, again!" 
        }
        
        output$feedback_confirmation <- renderText(text)
        print(str_c("***** user entered the following input and feedback: ", input$text, " ", input$checkbox, " feedback: ", input$feedback))
    })
    
}

# Stop

onStop(function() {
    list.files("logs", full.names = TRUE) %>% 
        map(put_object, bucket = s3BucketName,
            key=AWS_ACCESS_KEY_ID,
            secret=AWS_SECRET_ACCESS_KEY,
            region=AWS_DEFAULT_REGION)
})

# Run the application 


shinyApp(ui = ui, server = server)
