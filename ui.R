#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text Predictor, Swiftkey / John Hopkins university project, June 2017"),
  h4(div("Enter a text and I will guess your next word", style="color: blue;")),
  h4(div("Samir Ghoudrani, 01/06/2017", style="color: blue;")),
    # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      ##img(src='1.png', align = "right",width=300), 
      sliderInput("nb_res",
                   "Number of prediction alternatives :",
                   min = 1,
                   max = 20,
                   value = 10),
       textInput('text', "Enter your text here : ","I usually like beer and wine, but for chrismas let's open a bottle of"),
       ##actionButton("guess", "Next word ?"),
      ##img(src='2.png', align = "right",width=300),
       tableOutput("results")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # Use imageOutput to place the image on the page
      img(src='fleche.png', align = "left",width=1000),
      img(src='approach.png', align = "left",width=1000)
      
    )
  )
)) 


## i love litterature, i have read a lot of


