# Application to facilitate machine learning training with the caret package


library(shiny)
library(caret)
library(plyr)
library(magrittr)
library(readxl)
library(pls)

shinyServer(function(input, output) {

  output$caret_table <- renderDataTable({
    caret.table <- cbind(
      llply(getModelInfo(),function(l){l$label}),
      llply(getModelInfo(),function(l){l$type})
    )
    type <- rep(0,nrow(caret.table))
    for(i in seq(nrow(caret.table))){
      if(caret.table[,2][i] == "Classification"){type[i] <- "Classification"}
      if(caret.table[,2][i] == "Regression"){type[i] <- "Regression"}
      if(length(unlist(caret.table[,2][i])) == 2){type[i] <- "Both"}
    }
    data.frame(.=names(caret.table[,1]),name = unlist(caret.table[,1]),type)
  })
  
  output$algo.info <- renderPrint({
    getModelInfo()[input$algo]
  })
  
  Dep <- reactive({
    if(is.null(input$Dep)){
      if(input$Trainproblem == "Regression"){
        gasoline$octane
      }else{
        iris$Species
      }
      
    }else{
      read_excel(input$Dep$datapath)
    }
  })
  output$Dep <- renderTable({
    Dep() %>% as.data.frame
  })
  Ind <- reactive({
    if(is.null(input$Ind)){
      if(input$Trainproblem == "Regression"){
        gasoline$NIR
      }else{
        iris[,1:4]
      }
    }else{
      read_excel(input$Dep$datapath)
    }
  })
  output$Ind <- renderTable({
    Ind() %>% as.matrix
  })
  output$grid <- renderTable({
    grid <- getModelInfo(model = input$algo)[[input$algo]]$grid
    grid(Ind(),Dep(),len=input$tuning.length)
  })
  
  output$Train.metric <- renderUI({
    if(input$Trainproblem == 'Classification' & length(unique(Dep())) == 2){
      truc <- c('Accuracy','Kappa','Specificity','Sensitivity','Pos_Pred_Value','Neg_Pred_Value','Detection_Rate','Balanced_Accuracy')
    }
    if(input$Trainproblem == 'Classification'& length(unique(Dep())) > 2){
      truc <- c('Accuracy','Kappa','Mean_Sensitivity','Mean_Specificity','Mean_Pos_Pred_Value','Mean_Neg_Pred_Value','Mean_Detection_Rate','Mean_Balanced_Accuracy')
      names(truc) <- c('Accuracy','Kappa','Specificity','Sensitivity','Pos_Pred_Value','Neg_Pred_Value','Detection_Rate','Balanced_Accuracy')
    }
    if(input$Trainproblem == 'Regression'){
      truc <- c('RMSE','Rsquared')
    }
    selectizeInput('Train.metric','what summary metric will be used to select the optimal model',choices=truc)
  })
  
  model <- eventReactive(input$Go,{
    # validate(need(nrow(Ind) == nrow(as.data.frame(Dep()))),"The number of observation in the two files are differents")
    withProgress(message = "Work in Progress", value=0, {
      incProgress(0)
      data <- data.frame(Ind = Ind(), Dep = Dep())
      set.seed(1)
      if(input$Trainproblem == 'Classification'){
        control <- trainControl(method = input$Train.control.method,
                                number=input$Train.tunning.CV,
                                savePredictions = "final",
                                summaryFunction = multiClassSummary,
                                allowParallel=T,verboseIter=F,returnData=F)
      }
      if(input$Trainproblem == 'Regression'){
        control <- trainControl(method = input$Train.control.method,
                                number=input$Train.tunning.CV,
                                savePredictions = "final",
                                summaryFunction = defaultSummary,
                                allowParallel=T,verboseIter=F,returnData=F)
      }
      set.seed(1)
      model <- train(Dep ~. , data = data,
                     method=input$algo,
                     metric = input$Train.metric,
                     trControl = control,
                     tuneLength = input$tuning.length
      )
    })
    return(model)
  })
  
  output$model.print <- renderPrint({
    print(model())
  })
  
  output$model.plot <- renderPlot({
    plot(model())
  })
  
  output$pred.table <- renderTable({
    x <- model()$pred$obs
    y <- model()$pred$pred
    data.frame(obs=x,pred=y)
  })
})
