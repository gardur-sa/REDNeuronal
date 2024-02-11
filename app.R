library(shiny)
library(shinydashboard)
library(neuralnet)
library(DT)
library(shinyWidgets)

##Datos
Aplicacion_NN=tabla_de_series
#check for NAs
sum(is.na(Aplicacion_NN))
Aplicacion_NN[is.na(Aplicacion_NN)]<-0
#train neural network for predicting PEMEX

ui <- dashboardPage(
  dashboardHeader(title = "Neural network Demo with neuralnet package", titleWidth = 500),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu()),
  dashboardBody(
    fluidPage(
      box(title="Input parameters", width = 6, 
          p("The user can train a neural network for predicting NHL players salary with this Shiny app. 
           Right now the user can only add one hidden layer with arbitrary neurons."),
          column(width = 6,
                 numericInput("training_proportion", "Training data proportion", 0.7, min=0.01, max = 0.99, step = 0.01)
          ),
          column(width = 6,
                 numericInput("test_proportion", "Test data proportion", 0.3, min=0.01, max = 0.99, step = 0.01)
          ),
          column(width = 6,
                 numericInput("threshold", "A numeric value specifying the threshold for the partial derivatives of the error function as stopping criteria", 0.1, min=0.01, max = 0.99, step = 0.01)
          ),
          column(width = 6,
                 numericInput("hidden_one_layer", "The number of hidden neurons in one layer", 3, min=0, max = 7, step = 1)
          ),
          column(width = 6,
                 numericInput("repetition", "The number of repetitions for the neural network's training", 1, min=1, max = 10, step = 1)
          ),
          column(width = 6,
                 numericInput("stepmax", "Maximum steps for the training of the neural network", 100000, min=10000, max = 1000000, step = 1000)
          ),
          column(width = 6,
                 pickerInput("cov_variables",  "Choose covariates", choices=names(Aplicacion_NN)[!names(Aplicacion_NN) %in% "PEMEX"], selected=names(Aplicacion_NN)[!names(Aplicacion_NN) %in% "PEMEX"], multiple = TRUE,  
                             options = list(
                               `actions-box` = TRUE,
                               `deselect-all-text` = "None...",
                               `select-all-text` = "Choose all",
                               `none-selected-text` = "Choose variables"
                             )),
                 actionButton("nn_start", "Click to train neural network")
          )
          
      ),
      box(title="Demo data", width = 6,
          dataTableOutput("table")
      ),
      conditionalPanel(
        condition = "input.nn_start",
        
        box(title = "Neural network plot", width = 12,
            plotOutput("nn_plot")
        ),
        box(title = "Modell results" ,width = 5,
            dataTableOutput("nn_table")
        ),
        box(title = "Prediction", width = 5,
            dataTableOutput("orig_pred_table")
        ),
        box(title = "Correlation", width = 2,    
            dataTableOutput("cor_table")
        )#,
        # plotOutput("nn_plot_2")
      )
    ) # end of fluidPage
  ) # end of dashboardBody
) # end of dashboardPage

server <- function(input, output) { 
  
  #demo data
  output$table <- renderDataTable(datatable(Aplicacion_NN, options = list(
    pageLength = 5,
    scrollX = 200,
    deferRender = TRUE,
    scroller = TRUE)))
  #training nn
  observeEvent(input$nn_start, {    
    
    withProgress(message = 'Training neural network', detail = "Wait for it...", value = 0, {
      #normalization
      Aplicacion_NN=data.frame(scale(Aplicacion_NN))
      #training and test data
      order = sample(2, nrow(Aplicacion_NN), replace = TRUE, prob=c(input$training_proportion, input$test_proportion))
      # order = sample(2, nrow(df), replace = TRUE, prob=c(0.7, 0.3))
      train_df = Aplicacion_NN[order == 1,]
      test_df = Aplicacion_NN[order == 2,]
      
      # paste formula
      nn_formula = input$cov_variables
      nn_formula <- paste(nn_formula, collapse=' + ')
      nn_formula <- paste('PEMEX ~', nn_formula)
      nn_formula <- as.formula(nn_formula)
      
      threshold=input$threshold
      hidden=input$hidden_one_layer
      rep=input$repetition
      stepmax=input$stepmax
      # threshold=0.1
      # hidden=3
      # rep=1
      # stepmax=100000
      
      
      
      neural_network <- neuralnet(#PEMEX ~ G500 + TOTAL + REPSOL + CHEVRON + MOBIL + BP,
        nn_formula,
        data = train_df,
        hidden = hidden,
        threshold = threshold,
        rep = rep,
        stepmax = stepmax,
        lifesign = "full")
      
      Aplicacion_NN <- data.frame(neural_network[["result.matrix"]])
      colnames(Aplicacion_NN)[1]=c("Result matrix")
      
      model_results <- predict(neural_network, test_df[1:7])
      orig_pred=data.frame(cbind(test_df$PEMEX, model_results))
      colnames(orig_pred)=c("PEMEX", "PEMEX prediction")
      
      # predicted_strength <- model_results$net.result
      cor=data.frame(round(cor(model_results, test_df$PEMEX),2))
      colnames(cor)[1]=c("Correlation between Salary and Prediction")
      
      output$nn_table <- renderDataTable(Aplicacion_NN, options = list(searching = FALSE))
      
      output$orig_pred_table <- renderDataTable(orig_pred, options = list(searching = FALSE))
      
      output$cor_table <- renderDataTable(cor, options = list(searching = FALSE))
      
      output$nn_plot <- renderPlot(plot(neural_network, rep="best"))
      
      # output$nn_plot_2 <- renderPlot(plot(orig_pred))
      
    })
  })
  
  
}

shinyApp(ui, server)
