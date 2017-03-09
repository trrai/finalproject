library(shiny)
source("data.R")
library(shinythemes)

diff.range<-range(data$diff)



ui <- fluidPage(theme = shinytheme("sandstone"),
                navbarPage("The Wage Gap."),

  sidebarLayout(
    
    sidebarPanel(
      sliderInput('diff.choice', label="Male to Female Wage Gap Range: ", min=diff.range[1], max=diff.range[2], value=diff.range),
      
      selectInput('data', label="Organize By: ", choices = c("Group", "Individual")),
      
      conditionalPanel(
        condition = "input.data == 'Group'",
        checkboxGroupInput('occupation1', "Occupation", choices = grouped.data$Occupation, selected = c("MANAGEMENT", "BUSINESS"))
      ),
      
      conditionalPanel(
        condition = "input.data == 'Individual'",
        checkboxGroupInput('occupation2', "Occupation", choices = single.data$Occupation, selected = NULL)
      ),

    
    mainPanel(
      img(src="gap.png"),
      p(textOutput('introductionText')),
      
      navbarPage("Visuals: ",
        
        tabPanel("Plot", 
                 p(textOutput('plotText1')),
                 plotlyOutput('plot'),
                 br(),
                 p(textOutput('plotText2')),
                 br(),br(),
                 plotlyOutput('plot2'),
                 br(),
                 p(textOutput('plotText3')),
                 br(),br(),
                 plotlyOutput('plot3'),
                 br(),
                 p(textOutput('plotText4')),
                 br(),br(),
                 plotlyOutput('plot4'),
                 br(),
                 p(textOutput('plotText5')),
                 br(),br(),
                 plotlyOutput('plot5'),
                 br(),
                 p(textOutput('plotText6')),
                 br(),br(),
                 plotlyOutput('plot6'),
                 br(),

                 p(textOutput('description', inline=TRUE)),

                 br(),
                 br(),br(),
                 p(textOutput('plotText7')),
                 plotOutput('plot7', hover = "hover"),
                 p(textOutput('text')),
                 br(),
                 p(textOutput('conclusionText1')),
                 p(textOutput('conclusionText2'))
          
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
