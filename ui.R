install.packages("shiny")
library(shiny)
install.packages("shinythemes")
library(shinythemes) #getting the theme for the app

#assigning a theme, Project name, and other aesthetic details
shinyUI(fluidPage(theme = shinytheme("cerulean"),
      titlePanel(title = h1("Statistical analysis of Indian Stocks", align="center")),
      navbarPage("MENU",
      tabPanel("Histogram",
      
      #Making the sidebar layout to select stocks and set the Confidence Interval         
      sidebarLayout(
        sidebarPanel(
          column(12, align="Center", offset = 0,
          selectInput("select",label = "Select Stock"
          ,choices = list("Tata Consultancy Services" = 1,"State Bank of India" = 2,"Reliance" = 3,"Ranbaxy" = 4,"ONGC" = 5,"Maruti" = 6,"Infosys" = 7,"HDFC" = 8,"GAIL"= 9,"ITC" = 10),selected = 1))
          ,column(12, align="Center", offset = 0,
          sliderInput("CI","Select the confidence level for the stock (%)",min = 0,max = 100,value = 95,post = " %",step = 0.5)
          )
        ),
        
        #Histogram of Log Returns
        mainPanel
        (
          plotOutput("histogram"),
          h4(textOutput("CI_Mean_Stock"), style = "font-family: 'Times New Roman'"),
          h4(textOutput("CI_Var_Stock"), style = "font-family: 'Times New Roman'")
        )
    )
),
    

    


#QQ Plots
tabPanel("QQ Plots",
         sidebarLayout(
           sidebarPanel(
             column(12, align="Center", offset = 0,
                    selectInput("select_6",label = "Select Stock"
                                ,choices = list("Tata Consultancy Services" = 1,"State Bank of India" = 2,"Reliance" = 3,"Ranbaxy" = 4,"ONGC" = 5,"Maruti" = 6,"Infosys" = 7,"HDFC" = 8,"GAIL"= 9,"ITC" = 10),selected = 1))
                               ),
           #QQ Plot
           mainPanel
           (
             plotOutput("qq")
           )
         )
         
         
), 


#Density Plots
tabPanel("Density Plots",
         sidebarLayout(
           sidebarPanel(
             column(12, align="Center", offset = 0,
                    selectInput("select_7",label = "Select Stock"
                                ,choices = list("Tata Consultancy Services" = 1,"State Bank of India" = 2,"Reliance" = 3,"Ranbaxy" = 4,"ONGC" = 5,"Maruti" = 6,"Infosys" = 7,"HDFC" = 8,"GAIL"= 9,"ITC" = 10),selected = 1))
           ),
           #DP Plot
           mainPanel
           (
             plotOutput("dp")
           )
         )
), 

    
    # Correlation Matrix
    tabPanel("Correlation Matrix Plot",
                 mainPanel(
                   column(10,
                          plotOutput("correlation"))
                  )
            ),         

          tabPanel("Regression Plot for One Stock ",
           sidebarLayout(
            sidebarPanel
             (
             column(12, align="Center", offset = 0,
             selectInput("select_2",label ="Select Stock",choices = list("Tata Consultancy Services" = 1,"SBI" = 2,"Reliance" = 3,"Ranbaxy" = 4,"ONGC" = 5,"Maruti" = 6,"Infosys" = 7,"HDFC" = 8,"GAIL"= 9,"ITC" = 10),selected = 1))
             ),
            mainPanel(
             plotOutput("StockDailyReg"),
             h4(textOutput("StockDailyRegResults"), style = "font-family: 'Times New Roman'"),
             plotOutput("ResidualPlot_1")
          )
        )
    ),
          #Two Stock t Test
          tabPanel("Two Stock t-Test",
           sidebarLayout(
            sidebarPanel(
              column(12, align="Center", offset = 0,
              selectInput("select_3",label ="Select Stock 1",choices = list("Tata Consultancy Services" = 1,"SBI" = 2,"Reliance" = 3,"Ranbaxy" = 4,"ONGC" = 5,"Maruti" = 6,"Infosys" = 7,"HDFC" = 8,"GAIL"= 9,"ITC" = 10),selected = 1)),
              column(12, align="Center", offset = 0,
              selectInput("select_4",label ="Select Stock 2",choices = list("Tata Consultancy Services" = 1,"SBI" = 2,"Reliance" = 3,"Ranbaxy" = 4,"ONGC" = 5,"Maruti" = 6,"Infosys" = 7,"HDFC" = 8,"GAIL"= 9,"ITC" = 10),selected = 2))
            ),
            mainPanel(
             plotOutput("TwoStockRegression"),
             h4(textOutput("TwoStockResult"), style = "font-family: 'Times New Roman'"),
             plotOutput("ResidualPlot_2"),
             h4(textOutput("PairedTest"), style = "font-family: 'Times New Roman'")
            )
                     
           )
      ),
      #Market Index analysis
      tabPanel("Sectoral Indexes VS NIFTY ",
             sidebarLayout(
               sidebarPanel(
                 column(12, align="Center", offset = 0,
                        selectInput("select_5",label = "Select Sector",choices = list("IT" = 12,"ENERGY" = 13,"FINANCE" = 14,"FMCG" = 15,"BANK" = 16,"AUTO" = 17),selected = 1))
               ),
               mainPanel(
                 plotOutput("NiftyIndCorr"),
                 h4(textOutput("NiftyIndResult"), style = "font-family: 'Times New Roman'")
                 
               )
            )
      )
    
  )
))
