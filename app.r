################################################################################
# 
# Creator: Ali PiyarAli
# 
# Link to Published App: https://alipiyarali.shinyapps.io/LinearProgrammingFinal/
# 
################################################################################


#Linear Programming
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(htmlTable)
library(stringr)
library(lpSolve)
# library(lpSolveAPI)
library(shinyjs)
library(shinyMatrix)
library(matlib)
library(gMOIP)

source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
    
    h2{
      font-weight: bold;
    }
    
    h4{
      font-weight: bold;
    }
    
    li{
      font-style: italic;
    }
    
    table, th, td {
      border: 1px solid white;
      color: Black;
      padding: 8px;
    }
    
    .matrix-input-col-header{
      border: 3px solid black;
      text-align: center;
      width: 100px;
    }
    
    .matrix-input-col-header th{
      text-align: center;
      width: 100px;
    }
    
    .matrix-input-col-header-cell{
      border: 3px solid black;
      text-align: center;
      width: 100px;
    }
    
    .matrix-input-row-header-cell{
      border: 3px solid black;
      text-align: center;
    }
    
    .matrix-input-cell{
      text-align: center;
      border: 1px solid black; 
    }
    
    #lhs{
      height: 190px;
    }
    
    #rhs{
      height: 150px;
    }
    
    #rhs, #lhs, #dec{
      overflow: scroll;
      text-align: center;
    }
    
    #rhs_heading{
      text-align: center;
    }
    
    
    #problem{
      overflow: scroll;
      font-size: 16px;
    }
    
    #math_model{
      font-family: "MathJax_Main","Times New Roman";
      font-size: 18px;
    }
    
    .resultBox{
      overflow-x: scroll;
    }

  ')
))
#The user interface
header <- dashboardHeader(title = "Linear Programming",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    # Inputs
    column(width=4,
      box(
        width = NULL,
        height = NULL,
        
        # Problem Statement
        textAreaInput("problem","Problem Statement (write your own problem statement)", height = 150, 
                      value="SAMPLE PROBLEM STATEMENT: 
A small chemical production company produces two products. Three raw material are used in the production of the two products: Product A and Product B. 
To produce a ton of product A, 0.4 ton of  material 1 and 0.6 ton of material 3 is required. To produce a ton of product B, 0.5 ton of material 1, 0.2 ton of material 2 and 0.3 ton of material 3 is required. The production facility is constrained by a limited availibility of three material, for the current produciton period 20 tons of material 1, 5 tons of material 2 and 21 tons of material 3 is available.
The accounting department analyzed the costs and determined the price that will result in a profit for both products: $40 for product A and $30 for product B."),
        
        hr(),
        
        # Tutorial
        h5(strong("Linear Programming Introduciton / Tutorial:")),
        withTags(ul(
          li(a(href = "https://www.youtube.com/watch?v=Bzzqx1F23a8", "YouTube")),
          li(a(href = "https://ocw.mit.edu/courses/sloan-school-of-management/15-053-optimization-methods-in-management-science-spring-2013/tutorials/MIT15_053S13_tut01.pdf", "MIT Sloan School of Management")),
          li(a(href = "https://www.datacamp.com/community/tutorials/linear-programming-with-spreadsheets", "Data Camp"))
        )),
        hr(),
        
        # Instructions
        h5(strong("Instructions:")),
        h5("Add rows and columns depending on the variables required for your problem."),
        h5("Click on the table cells below in step 1 to 3 to change values."),
        h5("Click the Calculate button to see the Graphical Solution and the Result."),
        hr(),
        
        # Number of Rows and Columns Input
        withTags(table(
          tr(td("          "),
             td(numericInput("row_val","Rows", width = 100, value = 3)),
             td(numericInput("col_val","Columns",  width = 100, value = 2))
          ))),
        
        # Objective Max or Min Selection and Objective Funciton Input
        fluidRow(
          box(width=12,
              h4("Step 1: Set Objective"),
              awesomeRadio(inputId = "maxmin", label = "", choices = c("Max", "Min"), inline = TRUE),
              matrixInput(inputId = "dec", value = matrix(c(40,30), 1, 2, dimnames = list(NULL, c("A", "B"))), 
                          class = "numeric",  rows = list(names = FALSE, editableNames = FALSE),
                          cols = list(names = TRUE, editableNames = TRUE)
              ), 
          ),
          
          # LHS Input
          box(width=12,
              h4("Step 2: Left-hand side constraints"),
              br(),
              matrixInput(inputId = "lhs", value = matrix(c(0.4,0.5,
                                                            0, 0.2,
                                                            0.6, 0.3), 3, 2, byrow=TRUE, 
                                                          dimnames = list(c("Material 1", "Material 2", "Material 3"),
                                                                          c("A", "B"))), 
                        class = "numeric",  rows = list(names = TRUE, editableNames = TRUE),
                        cols = list(names = TRUE, editableNames = TRUE),
            ), 
          ),
          
          # RHS Input
          box(width=12,
              
              h4("Step 3: Right-hand constraints"),
              h5(strong("Enter -1 for <= and 1 for >= in first column")),
              br(),
              
              matrixInput(inputId = "rhs_heading", value = matrix("0", 0, 2, dimnames = list(NULL, c("1(>)  and  -1(<)", "RHS"))), 
                          # dimnames = list(NULL, c("< or >", "RHS"))
                          class = "character",  rows = list(names = FALSE, editableNames = FALSE),
                          cols = list(names = TRUE, editableNames = FALSE)
              ), 
              
              matrixInput(inputId = "rhs", value = matrix(c(-1,-1,-1,20,5,21), 3, 2), 
                        # dimnames = list(NULL, c("< or >", "RHS"))
                        class = "numeric",  rows = list(names = FALSE, editableNames = FALSE),
                        cols = list(names = FALSE, editableNames = FALSE)
                        ),   
          ),
        ),
      ),
    ),
    
    # Outputs: Mathematical Model & Graphical Solution
    column(width=4,
       # Mathematical Model
       box(
         width = NULL,
         height = NULL, 
         
         # Button finalizes matrix and displays updated Mathematical Model, Grpah and Solution
         actionBttn(inputId = "calculate",
                    label = "Calculate", style = "jelly", color = "success"),
         
         br(),
         
         h2 ("Mathematical Model"),
         
         h5("To display the mathematical model correctly, all the column and row header must be filled out in Step 1, 2 and 3."),
         
         br(),
         
         # Mathematical Model
         box(class="math_model_box",
             width = 12,
             height = NULL,
             useShinyjs(),
             p(uiOutput("math_model"))
         ),
        ),
        
        # Graphical Solution
        box(
          width = NULL,
          height = NULL,
          h2 ("Graphical Solution (Feasible Region)"),br(),
          plotOutput("plot") 
        )
    ),

    # Results
    column(
      width = 4,
      box(class="resultBox",
        width = NULL,
        height = NULL,
        h2 ("Result"),br(),
        h4(uiOutput("val")),br(),
        
        h4(uiOutput("solutionTTitle")),
        tableOutput("solutionT"),br(),
      
        h4(uiOutput("constraintTitle")),
        tableOutput("constraintT"),br(),
        
        h4(uiOutput("ocrTitle")),
        tableOutput("ocrT"),br(),
        
        h4(uiOutput("rhsTitle")),
        tableOutput("rhsT"),br(),
      )
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

#Functions that implement the mathematics
#This file must go into the same directory as app.R
#source(".R")
#Any images, audio, or stylesheet must go into a subfolder named www

#Additional functions are OK here, but no variables


server <- function(session, input, output) {
  #Variables that are shared among server functions (use <<-)
  
  # Default Row and Column Value
  row_val <- 3
  col_val <- 2
  
  # Converting -1 & 1 to <= and >= in HTML for mathematical model
  rhs_sign <- data.frame(
    signs <- c(-1, 1),
    hex_sign <- c("&le;", "&ge;"),
    stringsAsFactors = FALSE
  )
  
  # Converting -1 & 1 to <= and >= for lp solve library
  rhs_solve_sign <- data.frame(
    signs <- c(-1, 1),
    solve_signs <- c("<=", ">="),
    stringsAsFactors = FALSE
  )
  
  #Initialization
  
  #Functions that respond to events in the input
  
  # Update "Number of Rows" and rerun lhs and rhs matrix input
  observeEvent(input$row_val, {
    row_val <<- input$row_val
    lhs_m <<- matrix(0, row_val, col_val)
    rhs_m <<- matrix("0", row_val, 2)
    # dimnames=list(c(), c("< or >", "RHS"))
    updateMatrixInput(session, inputId = "lhs", value = lhs_m)
    updateMatrixInput(session, inputId = "rhs", value = rhs_m)
  })
  
  # Update "Number of Columns" and rerun lhs and dec matrix input
  observeEvent(input$col_val, {
    col_val <<- input$col_val
    lhs_m <<- matrix(0, row_val, col_val)
    dec_m <<- matrix(0, 1, col_val)
    updateMatrixInput(session, inputId = "lhs", value = lhs_m)
    updateMatrixInput(session, inputId = "dec", value = dec_m)
  })
  
  # Update lhs matrix input
  observeEvent(input$lhs, {
    updateMatrixInput(session, inputId = "lhs",
                      value = input$lhs)
  })
  
  # Update rhs matrix input
  observeEvent(input$rhs, {
    updateMatrixInput(session, inputId = "rhs",
                      value = input$rhs)
  })
  
  # Update dec matrix input
  observeEvent(input$dec, {
    updateMatrixInput(session, inputId = "dec",
                      value = input$dec)
  })
  
  #final matrix
  finalm <- function(){
    
    # Update lhs, rhs and dec matrix input
    lhs_m <<- input$lhs
    rhs_m <<- input$rhs
    dec_m <<- input$dec
    
    # MATHEMATICAL MODEL PRINT
    
    # Get the column and row name for mathematical model
    var_colname <- colnames(dec_m, do.NULL = FALSE, prefix = "col")
    var_rowname <- rownames(lhs_m, do.NULL = FALSE, prefix = "row")
    
    formulaC <- c("<b>",input$maxmin)
    
    for (j in 1:col_val){
      formulaC <- c(formulaC, " ", dec_m[1,j], var_colname[j])
      if (j < col_val){
        formulaC <- c(formulaC," + ")
      }
    }
    
    formulaC <- c(formulaC,"</b>", "<br><br>", "<em>Subject to (s.t.):</em>", "<br><br>")
    
    for (i in 1:row_val){
      formulaC <- c(formulaC, "<b>", var_rowname[i], ": ", "</b>")
      for (j in 1:col_val){
        formulaC <- c(formulaC,lhs_m[i,j],var_colname[j])
        
        if (j < col_val){
          formulaC <- c(formulaC," + ")
        }
      }
      formulaC <- c(formulaC,rhs_sign[which(rhs_sign$signs == rhs_m[i,1]),2],rhs_m[i,2], "<br><br>")
    }
    
    output$math_model <- renderUI(HTML(paste0(formulaC)))
    
    # END MATHEMATICAL MODEL PRINT
    
    # Call solve function to solve the LP
    solve()
    
    # Print objective function value
    output$val <- renderUI(paste("Objective Function Value = ",solve()$objval))
    
    # Variable solution matrix
    solutionM <- matrix(round(solve()$solution,2), nrow = col_val, ncol = 1, dimnames = list(dimnames(dec_m)[[2]], c("Value")))
    
    # print variable solution matrix
    output$solutionTTitle <- renderUI(paste("Solution"))
    
    output$solutionT <- renderUI(htmlTable(solutionM,
                                     align = "lc",
                                     css.cell="padding:6px;",
                                     align.header="lr",
                                     css.header="padding-left:6px"))
    # Create slack vector
    slack <- c()
    
    # Add solve() output slack value to slack vector
    for (i in 1:row_val){
      s <- 0
      for (j in 1:col_val){
        s <- s + cons[i,j]*t(solutionM[j])
      }
      s <- rhs[i] - s
      slack[i] <- round(s,2)
    } 
    
    # Create Slack and Dual Prices matrix
    slackDual <- matrix(nrow=row_val, ncol=2)
    
    # Add above slack vector and solve() output dual prices value to slackDual matrix
    for (i in 1:row_val){
      slackDual[i,1] <- slack[i]
      slackDual[i,2] <- round(solve()$duals[i],2)
    }
    
    # Create constraint matrix
    constraintM <- matrix(slackDual, nrow=row_val, ncol = 2, dimnames = list(dimnames(lhs_m)[[1]], c("Slack/Surplus", "Dual Prices")))
    
    # Print constraint matrix
    output$constraintTitle <- renderUI(paste("Constraints"))
    
    output$constraintT <- renderUI(htmlTable(constraintM,
                                             align = "cc",
                                             css.cell="padding:6px;",
                                             align.header="cc",
                                             css.header="padding-left:6px"))
    
    # Create Objective Coefficient Ranges (OCR) matrix
    ocrvalue <- matrix(nrow = col_val, ncol = 5)
    
    # Add sove() OCR values to OCR matrix
    for (i in 1:col_val){
      ocrvalue[i,1] <- round(solve()$sens.coef.from[i],2) #Lower Limit
      ocrvalue[i,2] <- round(coe[i],2) #Current Value
      ocrvalue[i,3] <- round(solve()$sens.coef.to[i],2) #Upper Limit
      ocrvalue[i,4] <- round(coe[i] - solve()$sens.coef.from[i],2) #Allowable Decrease
      ocrvalue[i,5] <- round(solve()$sens.coef.to[i] - coe[i],2) #Allowable Increase
    }
    
    # Create OCR print matrix
    ocrM <- matrix(ocrvalue, nrow = col_val, ncol = 5, dimnames = list(dimnames(dec_m)[[2]], 
                                                             c("Lower Limit", "Current Value",
                                                               "Upper Limit","Allowable Decrease",
                                                               "Allowable Increase")))
    
    # Display OCR print matrix
    output$ocrTitle <- renderUI(paste("Objective Coefficient Ranges"))
    
    output$ocrT <- renderUI(htmlTable(ocrM,
                                      align = "cc",
                                      css.cell="padding:6px;",
                                      align.header="cc",
                                      css.header="padding-left:6px"))
    
    # Create Right Hand Side (RHS) matrix
    rhsvalue <- matrix(nrow = row_val, ncol = 3)
    
    # Add solve() rhs value to RHS matrix
    for (i in 1:row_val){
      
      #RHS Lower Limit
      if(round(solve()$duals.from[i],2) < 0){
        rhsvalue[i,1] <- "No Lower Limit" 
      } else if (round(solve()$duals.from[i],2) > 100000){
        rhsvalue[i,1] <- "No Lower Limit"
      } else{
        rhsvalue[i,1] <- round(solve()$duals.from[i],2)  
      }
        
        
      rhsvalue[i,2] <- round(rhs[i],2) #RHS Current Limit
      
      # RHS Upper Limit
      if(round(solve()$duals.to[i],2) < 0){
        rhsvalue[i,3] <- "No Upper Limit" 
      } else if (round(solve()$duals.to[i],2) > 100000){
        rhsvalue[i,3] <- "No Upper Limit"
      } else{
        rhsvalue[i,3] <- round(solve()$duals.to[i],2)  
      }
    }
    
    # Print RHS Matrix
    rhsM <- matrix(rhsvalue, nrow = row_val, ncol = 3, dimnames = list(dimnames(lhs_m)[[1]], c("Lower Limit", 
                                                                                               "Current Value", 
                                                                                               "Upper Limit")))
    output$rhsTitle <- renderUI(paste("Right Hand Side Ranges"))
    
    output$rhsT <- renderUI(htmlTable(rhsM,
                                      align = "cc",
                                      css.cell="padding:6px;",
                                      align.header="cc",
                                      css.header="padding-left:6px"))
    
    # Max Graph
    maxGraph <- plotPolytope(
                  cons,
                  rhs,
                  coe,
                  type = rep("c", ncol(cons)),
                  crit = "max",
                  faces = rep("c", ncol(cons)),
                  plotFaces = TRUE,
                  plotFeasible = TRUE,
                  plotOptimum = TRUE,
                  labels = "coord",
                  
                ) + ggplot2::xlab("x") + ggplot2::ylab("y") + ggplot2::ggtitle("")
    
    # Min Graph
    minGraph <- plotPolytope(
                  cons,
                  rhs,
                  coe,
                  type = rep("c", ncol(cons)),
                  crit = "min",
                  faces = rep("c", ncol(cons)),
                  plotFaces = TRUE,
                  plotFeasible = TRUE,
                  plotOptimum = TRUE,
                  labels = "coord",
                  
                ) + ggplot2::xlab("x") + ggplot2::ylab("y") + ggplot2::ggtitle("")
    
    # Display Graph
    if (input$maxmin == "Max"){
      output$plot <- renderPlot(maxGraph) 
    } else {
      output$plot <- renderPlot(minGraph)
    }
  }
  
  # Calculate Button
  observeEvent(input$calculate, {
    
    # Call final matrix function but delay it by 60 milliseconds to ensure
    # "matrixInput" updates first. The event "input$matrix_input" will not occur
    # until the user clicks on something besides another matrix cell. If they
    # immediately click on a button like "Generate Custom Matrix," it will 
    # return the old version of their custom matrix if we don't integrate a delay.
    delay(60,finalm())
  })
  
  # LP solve funciton
  solve <- function(){
    
    # coefficients of the objective function
    coe <<- as.vector(dec_m)
    
    # Create constraint matrix
    # Do not consider the non-negative constraint; it is automatically assumed
    cons <<- lhs_m
    
    # Right hand side constraint
    rhs <<- as.vector(rhs_m[,2])
    
    # Signs or direction of constraint
    s <- c()
    for (i in 1:length(rhs)){
      s[i] <- rhs_solve_sign[which(rhs_solve_sign$signs == rhs_m[i,1]),2]
    }

    # Optimum Solution
    optimum <<- lp(direction = tolower(input$maxmin),
                   objective.in = coe,
                   const.mat = cons,
                   const.dir = s,
                   const.rhs = rhs,
                   compute.sens = TRUE)
    return(optimum)
  }
}

#Run the app
shinyApp(ui = ui, server = server)