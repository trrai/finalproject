library(shiny)
library(shinythemes)
library(plotly)
source("data.R")

diff.range<-range(data$diff)



ui <- fluidPage(theme = shinytheme("sandstone"), #Use a theme to add some flare to the UI
                navbarPage("Analyzing Equal Pay: A Report By Group AE5"), #Top bar for title/header
                
                sidebarLayout( 
                  
                  sidebarPanel( #Widgets panel
                    
                    #Range for which wag gaps the user wants to show
                    sliderInput('diff.choice', label="Male to Female Wage Gap Range: ", min=diff.range[1], max=diff.range[2], value=diff.range),
                    
                    #User's choice on which input options they want for occupation
                    selectInput('data', label="Organize By: ", choices = c("Group", "Individual")),
                    
                    #clear the choices 
                    actionButton("reset", "Clear Choices"),
                    
                    #Show the user options based on how they want to organize the occupations!
                    conditionalPanel( 
                      condition = "input.data == 'Group'", #if they want to select groups
                      checkboxGroupInput(inputId="occupation1", label =  "Occupation", choices = grouped.data$Occupation, selected = c("MANAGEMENT", "BUSINESS"))
                    ),
                    
                    conditionalPanel(
                      condition = "input.data == 'Individual'", #if they want to select individual
                      checkboxGroupInput(inputId='occupation2',  label = "Occupation", choices = single.data$Occupation, selected = NULL)
                    ),
                     
                    #selecting the country for the line graph
                    selectInput("time.wage", "Select Country for Time Wage", choices = data.wage.time$LOCATION %>% unique())
                  ),
                    
                    
                    mainPanel(
                      img(src="gap.png"), #Main image
                      p(textOutput('introductionText')), 
                      
                      navbarPage("Visuals: ", #Navigation between plots & tables
                                 
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
