library(shiny)
library(shinythemes)
library(shinyWidgets)
library(markdown)
library(dygraphs)
library(DT)

if (interactive()) {
  navbarPage(
    "My Portfolio!",
    theme = shinytheme("slate"),
    
    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    
    #PORTFOLIO MANAGEMENT
    
    tabPanel(
      "Portfolio management",
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            inputId = "sliderCapInit",
            label = "Capitale Iniziale",
            min = 10000,
            max = 20000,
            value = 15000,
            step = 1000
          ),
          br(),
          hr(),
          
          selectInput(
            "selectAssetToSell",
            "Seleziona Asset da vendere:",
            c(
              "Berkshire" = "BRKB",
              "BlackRock" = "BLCK",
              "Apple" = "AAPL",
              "Microsoft" = "MSFT",
              "Moodys" = "MOOY",
              "Coca Cola" = "COCA"
            )
          ),
          
          sliderInput(
            inputId = "sliderFeeSell",
            label = "Fee",
            min = 0,
            max = 0.25,
            value = 0.1,
            step = 0.05
          ),
          
          actionButton(inputId = "btnSellAsset",
                       label = "Vendi")
        ),
        
        mainPanel(
          tableOutput("tablePortfolio"),
          hr(),
          textOutput("txtPortfolioValue_t_1"),
          br(),
          textOutput("txtPortfolioValue_t"),
          br(),
          htmlOutput("txtPortfolioProfit"),
          br(),
          htmlOutput("txtPortfolioReturn"),
          hr(),
          tableOutput("tableForecasting"),
          hr(),
          textOutput("txtPortfolioValuePred"),
          br(),
          textOutput("txtPortfolioValueReal"),
          br(),
          htmlOutput("txtPortfolioProfitPred"),
          br(),
          htmlOutput("txtPortfolioProfitReal"),
          br(),
          htmlOutput("txtPortfolioProfitDifference"),
          hr(),
          htmlOutput("txtAssetValueSell"),
          br(),
          htmlOutput("txtFeeSell"),
          br(),
          htmlOutput("txtProfitSell"),
          br()
        )
      )
    ),
    
    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    
    #DATA SUMMARY
    
    navbarMenu(
      "Data Summary",
      tabPanel("Price/Return",
               sidebarLayout(
                 sidebarPanel(
                   radioButtons(
                     "rbPriceReturnSummary",
                     label = h3("Plot"),
                     choices = list("Price" = 1, "Return" = 2),
                     selected = 1
                   )
                 ),
                 mainPanel(
                   dygraphOutput("plotDataSummary1"),
                   hr(),
                   dygraphOutput("plotDataSummary2"),
                   hr(),
                   dygraphOutput("plotDataSummary3"),
                   hr(),
                   dygraphOutput("plotDataSummary4"),
                   hr(),
                   dygraphOutput("plotDataSummary5"),
                   hr(),
                   dygraphOutput("plotDataSummary6"),
                   br()
                 )
               )),
      
      tabPanel(
        "CandleStick",
        
        mainPanel(
          plotOutput("plotCandlestick1"),
          hr(),
          plotOutput("plotCandlestick2"),
          hr(),
          plotOutput("plotCandlestick3"),
          hr(),
          plotOutput("plotCandlestick4"),
          hr(),
          plotOutput("plotCandlestick5"),
          hr(),
          plotOutput("plotCandlestick6"),
          br()
        )
      ),
      
      tabPanel(
        "Asset - Index Plot",
        
        mainPanel(
          dygraphOutput("plotReturnAssetIndex1"),
          hr(),
          dygraphOutput("plotReturnAssetIndex2"),
          hr(),
          dygraphOutput("plotReturnAssetIndex3"),
          hr(),
          dygraphOutput("plotReturnAssetIndex4"),
          hr(),
          dygraphOutput("plotReturnAssetIndex5"),
          hr(),
          dygraphOutput("plotReturnAssetIndex6"),
          br()
        )
      ),
      
      tabPanel(
        "Volumes",
        
        mainPanel(
          plotOutput("plotVolumes1"),
          hr(),
          plotOutput("plotVolumes2"),
          hr(),
          plotOutput("plotVolumes3"),
          hr(),
          plotOutput("plotVolumes4"),
          hr(),
          plotOutput("plotVolumes5"),
          hr(),
          plotOutput("plotVolumes6"),
          br()
        )
      ),
      
      tabPanel(
        "Asset - Asset Price",
        
        sidebarLayout(sidebarPanel(
          selectInput(
            "selectAssetPricePlot1",
            "Seleziona Asset :",
            c(
              "Berkshire" = "BRKB",
              "BlackRock" = "BLCK",
              "Apple" = "AAPL",
              "Microsoft" = "MSFT",
              "Moodys" = "MOOY",
              "Coca Cola" = "COCA"
            )
          ),
          selectInput(
            "selectAssetPricePlot2",
            "Seleziona Asset :",
            c(
              "Berkshire" = "BRKB",
              "BlackRock" = "BLCK",
              "Apple" = "AAPL",
              "Microsoft" = "MSFT",
              "Moodys" = "MOOY",
              "Coca Cola" = "COCA"
            )
          )
        ),
        mainPanel(dygraphOutput("plotAssetAssetPrice")))
      ),
      
      tabPanel(
        "Asset - Asset Return",
        
        sidebarLayout(sidebarPanel(
          selectInput(
            "selectAssetReturnPlot1",
            "Seleziona Asset :",
            c(
              "Berkshire" = "BRKB",
              "BlackRock" = "BLCK",
              "Apple" = "AAPL",
              "Microsoft" = "MSFT",
              "Moodys" = "MOOY",
              "Coca Cola" = "COCA"
            )
          ),
          selectInput(
            "selectAssetReturnPlot2",
            "Seleziona Asset :",
            c(
              "Berkshire" = "BRKB",
              "BlackRock" = "BLCK",
              "Apple" = "AAPL",
              "Microsoft" = "MSFT",
              "Moodys" = "MOOY",
              "Coca Cola" = "COCA"
            )
          )
        ),
        mainPanel(dygraphOutput("plotAssetAssetReturn")))
      )
    ),
  
    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    
    #DESCRIPTIVE ANALYTICS
    
    navbarMenu(
      "Descriptive analytics",
      tabPanel("Statistics",
               tableOutput("tableStatisticsMatrix"),
               hr(),
               textOutput("txtMaxMean"),
               br(),
               textOutput("txtMinMean"),
               hr(),
               textOutput("txtMaxStd"),
               br(),
               textOutput("txtMinStd")
          ),
      
      tabPanel("Correlation",
               plotOutput("plotCorrelationMatrix")),
      
      tabPanel("Covariance",
               tableOutput("tableCovarianceMatrix")),
      
      tabPanel(
        "Histogram Plot",
        plotOutput("plotHistogram1"),
        hr(),
        plotOutput("plotHistogram2"),
        hr(),
        plotOutput("plotHistogram3"),
        hr(),
        plotOutput("plotHistogram4"),
        hr(),
        plotOutput("plotHistogram5"),
        hr(),
        plotOutput("plotHistogram6"),
        br()
      ),
      
      tabPanel(
        "Box Plot",
        plotOutput("plotBox1"),
        hr(),
        plotOutput("plotBox2"),
        hr(),
        plotOutput("plotBox3"),
        hr(),
        plotOutput("plotBox4"),
        hr(),
        plotOutput("plotBox5"),
        hr(),
        plotOutput("plotBox6"),
        hr(),
        plotOutput("plotBoxAll"),
        br()
      ),
      
      tabPanel(
        "Quantile Plot",
        plotOutput("plotQuantile1"),
        hr(),
        plotOutput("plotQuantile2"),
        hr(),
        plotOutput("plotQuantile3"),
        hr(),
        plotOutput("plotQuantile4"),
        hr(),
        plotOutput("plotQuantile5"),
        hr(),
        plotOutput("plotQuantile6"),
        br()
      ),
      
      tabPanel(
        "QQ Plot",
        plotOutput("plotQQ1"),
        hr(),
        plotOutput("plotQQ2"),
        hr(),
        plotOutput("plotQQ3"),
        hr(),
        plotOutput("plotQQ4"),
        hr(),
        plotOutput("plotQQ5"),
        hr(),
        plotOutput("plotQQ6"),
        br()
      )
    ),
    
    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    
    #BETA COMPUTATION
    
    tabPanel(
      "Beta Computation",
      sidebarLayout(
        sidebarPanel(selectInput(
          "selectAssetBeta",
          "Seleziona Asset : ",
          c(
            "Berkshire" = "BRKB",
            "BlackRock" = "BLCK",
            "Apple" = "AAPL",
            "Microsoft" = "MSFT",
            "Moodys" = "MOOY",
            "Coca Cola" = "COCA"
          )
        )),
        
        mainPanel(
          textOutput("txtBeta"),
          hr(),
          dygraphOutput("plotPriceAssetIndex"),
          hr(),
          dygraphOutput("plotBeta"),
          br()
        )
      )
    ),
    
    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    #------------------------------------------------------------------------------------------------
    
    #PREDICTIVE ANALYTICS
    
    navbarMenu(
      "Predictive analytics",
      tabPanel("Price",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "selectAssetToPriceForecast",
                     "Seleziona Asset :",
                     c(
                       "Berkshire" = "BRKB",
                       "BlackRock" = "BLCK",
                       "Apple" = "AAPL",
                       "Microsoft" = "MSFT",
                       "Moodys" = "MOOY",
                       "Coca Cola" = "COCA"
                     )
                   ),
                   sliderInput(
                     inputId = "sliderPriceAR",
                     label = "AR",
                     min = 0,
                     max = 5,
                     value = 2,
                     step = 1
                   ),
                   sliderInput(
                     inputId = "sliderPriceMA",
                     label = "MA",
                     min = 0,
                     max = 5,
                     value = 4,
                     step = 1
                   ),
                   
                   actionButton(inputId = "btnUpdatePriceForecast",
                                label = "Modifica")
                 ),
                 
                 mainPanel(
                   plotOutput("plotPricePeriod"),
                   hr(),
                   plotOutput("plotPriceForecast"),
                   hr(),
                   textOutput("txtPriceForecastRSME"),
                   br()
                 )
               )),
      
      tabPanel("Return",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "selectAssetToReturnForecast",
                     "Seleziona Asset :",
                     c(
                       "Berkshire" = "BRKB",
                       "BlackRock" = "BLCK",
                       "Apple" = "AAPL",
                       "Microsoft" = "MSFT",
                       "Moodys" = "MOOY",
                       "Coca Cola" = "COCA"
                     )
                   ),
                   sliderInput(
                     inputId = "sliderReturnAR",
                     label = "AR",
                     min = 0,
                     max = 5,
                     value = 2,
                     step = 1
                   ),
                   sliderInput(
                     inputId = "sliderReturnMA",
                     label = "MA",
                     min = 0,
                     max = 5,
                     value = 4,
                     step = 1
                   ),
                   
                   actionButton(inputId = "btnUpdateReturnForecast",
                                label = "Modifica")
                 ),
                 
                 mainPanel(
                   plotOutput("plotReturnPeriod"),
                   hr(),
                   plotOutput("plotReturnForecast"),
                   hr(),
                   textOutput("txtReturnForecastRSME"),
                   br()
                 )
               ))
    )
  )
}