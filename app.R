if("DT" %in% rownames(installed.packages()) == FALSE) {install.packages("DT")}
if("readxl" %in% rownames(installed.packages()) == FALSE) {install.packages("readxl")}
library(DT)
library(readxl)

attributes <- read.csv("~/Smash/data/attributes.csv", row.names = 1)
vs <- read.csv("~/Smash/data/vs.csv", row.names = 1)
path <- "~/Smash/data/frames.xlsx"
sheets <- excel_sheets(path)
sheet_list <- lapply(sheets, function(f){
  read_excel(path, sheet = f)
  })
frame_data <- setNames(sheet_list,sheets)
ground <- c("Jab 1","F-Tilt","U-Tilt","D-Tilt","Dash Attack")

ui <- fluidPage(
  navbarPage("Smash stats",
    tabPanel("ANOVA",
      sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "character",
                      label = "Choose a character",
                      choices = rownames(attributes)
          ),
          selectInput(inputId = "response", 
                      label = "Choose a month", 
                      choices = colnames(attributes)[1:5]),
          selectInput(inputId = "factor",
                    label = "Choose a factor to test",
                    choices = colnames(attributes)[6:27])
        ),
        mainPanel(
          plotOutput("plot"),
          verbatimTextOutput("analysis")
        )
      )
    ),
    tabPanel("Head-to-Head",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "c1",
                             label = "Choose your characters",
                             choices = rownames(vs)
                 ),
                 selectInput(inputId = "c2",
                             label = "VS",
                             choices = rownames(vs)
                 )
               ),
               mainPanel(
                 verbatimTextOutput("adv"),
                 verbatimTextOutput("fastest"),
                 DT::dataTableOutput("data1"),
                 DT::dataTableOutput("data2")
               )
             )
    )
    
  )
)
gen_df <- function(f){
  temp <- frame_data[[f]]
  temp
}
server <- function(input, output) {
  output$plot <- renderPlot(
    plot(get(input$response) ~ get(input$factor), 
         col = ifelse(rownames(attributes) == input$character, "red", "black"), 
         xlab = input$factor, ylab=input$response, 
         data = attributes)+
      abline(lm(get(input$response) ~ get(input$factor), data=attributes))
    )
  output$analysis <- renderPrint(
    summary(aov(get(input$response) ~ get(input$factor), data=attributes))
  )
  output$adv <- renderPrint(
    cat( "At the end of an average game",
         input$c1,
         "has",
      as.character(vs[input$c1,input$c2]),
        "lives, while",
      input$c2,
        "has 0."
      )
    )
  output$data1 <- DT::renderDataTable(
    gen_df(input$c1)
    )
  output$data2 <- DT::renderDataTable(
    gen_df(input$c2)
  )
  output$fastest <- renderPrint(
    cat(input$c1,
     "'s fastest move starts in",
    attributes[input$c1,"fastest.ground"],
    "frames. which is ",
    ifelse(attributes[input$c1,"fastest.ground"]<attributes[input$c2,"fastest.ground"], "faster", "not faster"),
    "than",
    input$c2,
    "'s fastest move, which comes out in",
    attributes[input$c2,"fastest.ground"],
    "frames."
    )
  )
}
shinyApp(ui = ui, server = server)