library(shiny)
source("data.R")


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
        checkboxGroupInput('occupation1', "Occupation", choices = grouped.data$Occupation, selected = c("MANAGEMENT", "BUSINESS"))
      ),
      
      conditionalPanel(
        condition = "input.data == 'Individual'",
        checkboxGroupInput('occupation2', "Occupation", choices = single.data$Occupation, selected = NULL)
      ),
      
      selectInput("time.wage", "Select Country for Time Wage", choices = data.wage.time$LOCATION %>% unique())
      
     
      
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 plotlyOutput('plot'),
                 br(),
                 p(textOutput('text1')),
                 br(),br(),
                 plotlyOutput('plot2'),
                 br(),
                 p(textOutput('text2')),
                 br(),br(),
                 plotlyOutput('plot3'),
                 br(),
                 p(textOutput('text3')),
                 br(),br(),
                 plotlyOutput('plot4'),
                 br(),
                 p(textOutput('text4')),
                 br(),br(),
                 plotlyOutput('plot5'),
                 br(),
                 p(textOutput('text5')),
                 br(),br(),
                 plotlyOutput('plot6'),
                 br(),
                 p(textOutput('description', inline=TRUE)),
                 br(),
                 p(strong("Highlighted Points:"), "(Click and drag on chart to select points!)", tableOutput('selected')),
                 br(),br(),
                 plotOutput('plot7', hover = "hover"),
                 p(textOutput('text')),
                 br(),
                 p(textOutput('text6'))
          
          
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
