# Application to facilitate machine learning training with the caret package
# Avoid the step learning curve of all ML framework
# 2 xlsx files to upload, need default to iris in case no upload


library(shiny)
library(caret)
library(plyr)

caret.table <- cbind(
  llply(getModelInfo(),function(l){l$label}),
  llply(getModelInfo(),function(l){l$type})
)
Train.model.algo.choice <- names(caret.table[,1])
# names(Train.model.algo.choice) <- caret.table[,1]

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      h5("Upload here two excel files with the independent and dependent variables, the first row must contain ths column names. If no files are upload, default dataset will be used"),
      fileInput("Ind","Independent variable file",accept = "xlsx"),
      fileInput("Dep","Dependent variable file",accept = "xlsx"),
      selectizeInput("algo",'Choice of the algorythm',choices= Train.model.algo.choice,selected='rf'),
      radioButtons('Trainproblem','Type',choices=c('Classification','Regression'),selected='Classification'),
      numericInput("tuning.length","Tuning length",10),
      actionButton('Go','Train'),
      helpText(   a("Click Here to learn about the Validation techniques",target="_blank",
                    href="https://en.wikipedia.org/wiki/Cross-validation_%28statistics%29")
      ),
      selectizeInput('Train.control.method','Validation method for the tunning',
                     choices=c('boot', 'repeatedcv', 'LOOCV'),
                     selected='repeatedcv'),
      uiOutput('Train.metric'),
      numericInput('Train.tunning.CV','Either the number of folds or number of resampling iterations',5)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data",
                 tabsetPanel(
                   tabPanel("Independent variables",
                     tableOutput("Ind")
                   ),
                   tabPanel("Dependent variables",
                            tableOutput("Dep")
                   )
                 )
        ),
        tabPanel("Prediction table",
                 h4("This table is the cross validation table"),
                 tableOutput("pred.table")
                 ),
        tabPanel("Model print",
                 verbatimTextOutput("model.print")
        ),
        tabPanel("Training plot",
                 plotOutput("model.plot")
        ),
        tabPanel("Tuning grid",
                 tableOutput("grid")
                 ),
        tabPanel("Model list",
                 dataTableOutput("caret_table")
                 ),
        tabPanel("Model Info",
                 verbatimTextOutput("algo.info")
                 )
      )
      
    )
  )
))
