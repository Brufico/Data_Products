#
# This is a test Shiny web application. It tries nested layouts
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

        # Application title
        titlePanel("Trying layout"),

        # begin a tabset panel (top)
        tabsetPanel(
                # first main tab
                tabPanel("Situation and Instructions",
                         #"contents is an  tablist panel inside the taPanel"
                         navlistPanel(
                                 tabPanel("Situation",
                                          p("Example borrowed from Lagarde, Jean, 'L'analyse des données'"),
                                          h2("Marketing Mix"),
                                          p("A company has just launched a promotional campaign.
                                            A first report lists, for each of the 8 sales sectors, the sales volume,
                                            the advertising budget and the number of visits to the distributors made by
                                            the sales representatives. The data is shown below (left tab 'data')"),
                                          p("In order to analyze the effectiveness of the campaign, the manager
                                            has tried to explain the sales volume using two regresion models:"),
                                          tags$ul(
                                                  tags$li("sales ~ advertising"),
                                                  tags$li("sales ~ visits")
                                          ),
                                          p("You can view the two corresponding scatterplots by clicking on the
                                            'simple regresions' tabs on the left).
                                            Is'nt there something very odd with one of these scatterplots / regression models ?"),
                                          p("A double regresion model has also been made. You can view and manipulate the
                                            corresponding 3D-scatterplot (and regression plane) by clicking the tab
                                            'multiple regression' (on top)",
                                             br(),
                                             "By changing the view angles, can you match and explain the odd
                                              simple regressoion result we saw before?" )
                                 ),
                                 tabPanel("Data",
                                          h3("The data")
                                          ),
                                 tabPanel("Simple regression: Advertising",
                                          h3("Simple regression: Sales ~ Advertising"),
                                          h4("Regression equation"),
                                          h4("Scatterplot")
                                          ),
                                 tabPanel("Simple regression: Visits",
                                          h3("Simple regression: Sales ~ Visits"),
                                          h4("Regression equation"),
                                          h4("Scatterplot")
                                 ),
                                 tabPanel("Scatterplot matrix",
                                          "contents instructions xx")
                         )

                ),
                # second main tab panel: the interactive part
                tabPanel("Multiple regression : Sales ~ Advertising + Visits",
                         sidebarLayout(
                                 sidebarPanel(
                                         "Sidebar here",
                                         sliderInput("bins",
                                                     "Number of bins:",
                                                     min = 1,
                                                     max = 50,
                                                     value = 30)
                                 ),
                                 mainPanel(
                                         plotOutput("distPlot")
                                 )
                         )
                )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        output$distPlot <- renderPlot({
                # generate bins based on input$bins from ui.R
                x    <- faithful[, 2]
                bins <- seq(min(x), max(x), length.out = input$bins + 1)

                # draw the histogram with the specified number of bins
                hist(x, breaks = bins, col = 'darkgray', border = 'white')
        })
}

# Run the application
shinyApp(ui = ui, server = server)

