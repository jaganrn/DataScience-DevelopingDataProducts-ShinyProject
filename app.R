# Developing Data Products: Shiny Project
#
# This program demonstrates few capabilities of Shiny for building interactive application
#
# Written by Jagannatha Reddy
#
# Definitions of Diamond Cuts and Clarity sourced from the following locations:
#     Diamond Cut: http://www.lumeradiamonds.com/diamond-education/diamond-cut
#     Diamond Clarity: http://4cs.gia.edu/en-us/diamond-clarity/

#include required packages
library(shiny)
library(ggplot2)

#define constants
CutDefSampleSize <- 1500
CutDefLineSize   <- 1
ClarityDefSampleSize <- 5000
ClarityDefLineSize   <- 1

allDiamondCuts <- c("Ideal", "Premium", "Very Good", "Good", "Fair")
diamondCutLegendTable <- data.frame(
    Cut=allDiamondCuts,
    Represents=c("The highest grades of polish and symmetry allow it to reflect even more light than the standard ideal cut",
                 "Maximum fire and brilliance. Reflects nearly all of the light that enters the diamond, creating exceptional sparkle and life",
                 "Properly reflects most of the light that enters the diamond, producing superior fire and brilliance. Under normal lighting conditions, appears very similar to Excellent Cut, but for a lower price",
                 "Reflects a majority of the light that enters the diamond, for an above average appearance. An excellent value compared to higher cut grades",
                 "Allows much of the light entering the diamond to escape from the sides or bottom, reducing perceived fire and brilliance")
)

allDiamondClarities <- c("IF", "VVS1", "VVS2", "VS1", "VS2", "SI1", "SI2", "I1")
allDiamondClarityLevels <- c("IF", "VVS1, VVS2", "VS1, VS2", "SI1, SI2", "I1")
diamondClarityLegendTable <- data.frame(
    Clarity=allDiamondClarityLevels,
    Represents=c("Internally Flawless (IF) No inclusions visible under 10x magnification",             
                 "Inclusions so slight they are difficult for a skilled grader to see under 10x magnification",             
                 "Inclusions are observed with effort under 10x magnification, but can be characterized as minor",
                 "Inclusions are noticeable under 10x magnification",
                 "Inclusions are obvious under 10x magnification which may affect transparency and brilliance")
)

#UI
ui <- fluidPage(
        #define 3 tabs showcase the application
        navbarPage("Diamonds Analysis", tabPanel("By Cut", uiOutput('cutPage')),
                   tabPanel("By Clarity", uiOutput('clarityPage')),
                   tabPanel("Help", uiOutput('helpPage'))
        )
    )

#server function which takes input, output, & session parameters
server <- function(input, output, session) {

    #Tab to perform analysis on the Cut
    output$cutPage <- renderUI(
        sidebarLayout(
            sidebarPanel(
                #define various inputs
                checkboxGroupInput("cut", "Select Diamond Cut Types to Consider for Analysis",
                           allDiamondCuts, selected=allDiamondCuts),
                numericInput("cutSampleSize", "Enter Sample Size", CutDefSampleSize, 500, 1600, 100),
                numericInput("cutLineSize", "Enter Line Size", CutDefLineSize, 0.5, 2, 0.1),
                actionButton("resetCutInputs", "Reset Inputs")
            ),
            mainPanel(
                #show the contents on the main panel
                h3("Diamond Price by Carat Value for selected Cuts"),
                plotOutput("myDiamondCutPlot"),
                tableOutput("cutLegend")
            )
        )
    )

    output$myDiamondCutPlot = renderPlot({
      #validate to ensure user has selected at-least one Cuts for analysis
        validate(
            need(input$cut != "", "Please select one or more Cuts to perform analysis")
        )
      
        #obtain the data for selected Cuts   
        myData <- diamonds[diamonds$cut==input$cut,]
        #plot the graph only on sample data for performance reasons
        data <- myData[sample.int(nrow(myData), input$cutSampleSize, replace=TRUE),]
        g <- ggplot(data=data, aes(x=carat, y=price, color=clarity)) +
             geom_smooth(alpha=0.1, size=input$cutLineSize) +
             labs(x="Carat", y="Price (USD)")
        g
    })

    #reset the input selections on pression the Reset Inputs
    observe({
        input$resetCutInputs
        updateNumericInput(session, "cutSampleSize", value = CutDefSampleSize)
        updateNumericInput(session, "cutLineSize", value = CutDefLineSize)
        updateCheckboxInput(session, "cut", value = allDiamondCuts)
    })

    #define the legend table
    output$cutLegend <-  output$cutLegend2 <- renderTable(diamondCutLegendTable)
    
    #Tab to perform analysis on the Clarity
    output$clarityPage <- renderUI(
        sidebarLayout(
            sidebarPanel(
                #define various inputs
                checkboxGroupInput("clarity", "Select Diamond Clarities to Consider for Analysis",
                           allDiamondClarities, selected=allDiamondClarities),
                numericInput("claritySampleSize", "Enter Sample Size", ClarityDefSampleSize, 1000, 10000, 250),
                numericInput("clarityLineSize", "Enter Line Size", ClarityDefLineSize, 0.5, 2, 0.1),
                actionButton("resetClarityInputs", "Reset Inputs")
            ),
            mainPanel(
                #show the contents on the main panel
                h3("Diamond Price by Carat Value for selected Clarities"),
                plotOutput("myDiamondClarityPlot"),
                tableOutput("clarityLegend")
            )
        )
    )

    output$myDiamondClarityPlot = renderPlot({
      #validate to ensure user has selected at-least one Clarity for analysis
      validate(
        need(input$clarity != "", "Please select one or more Clarities to perform analysis")
      )
      
        #obtain the data for selected Clarities
        myData <- diamonds[diamonds$clarity==input$clarity,]
        #plot the graph only on sample data for performance reasons
        data <- myData[sample.int(nrow(myData), input$claritySampleSize,replace=TRUE),]
        g <- ggplot(data=data, aes(x=carat, y=price, color=cut)) +
             geom_smooth(alpha=0.1, size=input$clarityLineSize) +
             labs(x="Carat", y="Price (USD)")
        g
    })

    #reset the input selections on pression the Reset Inputs
    observe({
        input$resetClarityInputs
        updateNumericInput(session, "claritySampleSize", value = ClarityDefSampleSize)
        updateNumericInput(session, "clarityLineSize", value = ClarityDefLineSize)
        updateCheckboxInput(session, "clarity", value = allDiamondClarities)
    })

    #define the legend table
    output$clarityLegend <-  output$clarityLegend2 <- renderTable(diamondClarityLegendTable)
        
    #display the legends in the Help tab
    output$helpPage <- renderUI(
        mainPanel(
          h3("Cut Definitions"),
          tableOutput("cutLegend2"),
          h3("Clarity Definitions"),
          tableOutput("clarityLegend2")
        )
    )
}
    
shinyApp(ui=ui, server=server)
