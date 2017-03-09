library(shiny)
library(shinythemes)
library(plotly)
source("data.R")

diff.range<-range(data$diff)



ui <- fluidPage(theme = shinytheme("sandstone"),
                navbarPage("The Wage Gap."),
                
                sidebarLayout(
                  
                  sidebarPanel(
                    sliderInput('diff.choice', label="Male to Female Wage Gap Range: ", min=diff.range[1], max=diff.range[2], value=diff.range),
                    
                    selectInput('data', label="Organize By: ", choices = c("Group", "Individual")),
                    
                    actionButton("reset", "Clear Choices"),
                    
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
                      img(src="gap.png"),
                      p(textOutput('introductionText')),
                      
                      navbarPage("Visuals: ",
                                 
                                 tabPanel("Plot", 
                                          p(textOutput('plotText1')),
                                          plotlyOutput('plot'),
                                          p(textOutput('plotText2')),
                                          plotlyOutput('plot2'),
                                          p(textOutput('plotText3')),
                                          plotlyOutput('plot3'),
                                          p(textOutput('plotText4')),
                                          plotlyOutput('plot4'),
                                          p(textOutput('plotText5')),
                                          plotlyOutput('plot5'),
                                          p(textOutput('plotText6')),
                                          plotlyOutput('plot6'),
                                          p(textOutput('description', inline=TRUE)),
                                          p(textOutput('plotText7')),
                                          plotOutput('plot7', hover = "hover"),
                                          p(textOutput('text')),
                                          p(textOutput('conclusionText1')),
                                          p(textOutput('conclusionText2'))
                                          
                                 ),
                                 
                                 tabPanel("U.S TABLE", 
                                          p(textOutput('tableText')),
                                          dataTableOutput('table')
                                 ),
                                 
                                 tabPanel("WAGE GAP / TIME TABLE", 
                                          p(textOutput('tableText2')),
                                          dataTableOutput('table2')
                                 )
                                
                      )
                      
                    )
                  )
                )


shinyUI(ui)
