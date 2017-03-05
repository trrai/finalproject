library(shiny)
source("data.R")

sepal.range <- range(iris_db$Sepal.Length)
petal.range <- range(iris_db$Petal.Length)
diff.range<-range(data$diff)

ui <- fluidPage(
  

  titlePanel("The Wage Gap"),
  

  sidebarLayout(
    
    sidebarPanel(
      sliderInput('diff.choice', label="Male to Female Wage Gap Range: ", min=diff.range[1], max=diff.range[2], value=diff.range),
      checkboxInput('smooth', label=strong("Show Trendline"), value=TRUE),
      
      selectInput('data', label="Organize By: ", choices = c("Group", "Individual")),
      
      conditionalPanel(
        condition = "input.data == 'Group'",
        checkboxGroupInput('occupation1', "Occupation", choices = grouped.data$Occupation, selected = NULL)
      ),
      
      conditionalPanel(
        condition = "input.data == 'Individual'",
        checkboxGroupInput('occupation2', "Occupation", choices = single.data$Occupation, selected = NULL)
      )
      
     
      
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 plotOutput('plot', brush = "plot_brush"),
                 plotOutput('plot2'),
                 plotOutput('plot3'),
                 plotOutput('plot4'),
                 plotOutput('plot5'),
                 plotOutput('plot6'),
                 p(textOutput('description', inline=TRUE)),
                 p(strong("Highlighted Points:"), "(Click and drag on chart to select points!)", tableOutput('selected'))
          
          
          ),
        tabPanel("Table", 
                 p("The table below is a collection of the data visualized in the plot. Although, in addition to seeing the 
                   sepal height and petal height, you're also able to see the sepal width and petal width! You can search for 
                   specific data entries using the search bar on the right, or organize how many results you would like to see
                   per page using the selector on the left."),
                 dataTableOutput('table')
                 )
      )
     
    )
  )
)

shinyUI(ui)