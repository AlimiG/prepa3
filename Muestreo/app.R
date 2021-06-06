#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse, warn.conflicts = FALSE)
# Define UI for application that draws a histogram
ui <- navbarPage("Prepa 3",
   tabPanel("Importancia del muestreo",
            sidebarPanel(
                sliderInput("samples",
                            "Numero de muestras",
                            min = 10,
                            max = 10000,
                            value = 15)
            ),
            
            mainPanel(plotOutput("sampling_plot"))
            
            
            
            
            ),
    # Sidebar with a slider input for number of bins 
   tabPanel("Distribucion T", sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Grados de Libertad:",
                        min = 1,
                        max = 30,
                        value = 3),
            "Podemos ajustar los grados de libertad para comparar la distribucion t con la normal"
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("first_plot")
        )
    )
),

tabPanel("Chi-Squared",
         
         sidebarPanel(
             sliderInput("df",
                         "Grados de Libertad:",
                         min = 1,
                         max = 30,
                         value = 3),
             "Podemos ajustar los grados de libertad para comparar la distribucion chi-cuadrado con la normal"
         ),
         mainPanel(
             plotOutput("chisplot")
         )
         )





)

# function to do all the work
estImg <- list()
emphCol <- rgb(0,0,1)
emphColLight <- rgb(.5,.5,1)
emphGrey <- grey(.5)
colour <- TRUE
width <- 4.5
height <- 4.5

# plot
x <- 60:140
y <- dnorm(x,100,15)
plot(x,y,lwd=3,type="l",col=ifelse(colour,emphCol,"black"),
     xlab="IQ Score", ylab="Probability Density",frame.plot=FALSE
)


# function to do all the work
plotSamples <- function( n ) {
    
    IQ <- rnorm(n, 100, 15)
    hist( IQ, breaks=seq(10,180,5), border="white", 
          col=ifelse(colour,emphColLight,emphGrey),
          xlab="IQ Score", ylab="Frequency", xlim=c(60,140),
          main=""
    )
    #print( paste( "n=",n,"mean=",mean(IQ), "sd=",sd(IQ) ) )
}

x_axis = seq(-4,4,0.1)
normal_dist <- tibble(density = dnorm(x_axis),x_axis= x_axis)
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$first_plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        ggplot(normal_dist, aes(x = x_axis, y = density ))+
            geom_line()+ geom_line(data = tibble(density = dt(x_axis,input$n),x= x_axis), aes(x = x_axis, y = density), linetype = "dashed", color = "Red")+
            labs(title= "Comparacion entre la distribucion normal y la t", x= "", y = "Densidad")

        # draw the histogram with the specified number of bin
    })
    
    
    sampling <- eventReactive(input$go,{tibble(data =  rnorm(input$samples))})
    
    output$sampling_plot <- renderPlot({
        
        
      plotSamples(input$samples)
    })
    
    output$chisplot <- renderPlot({
        x = seq(0,40,.1)
        datos <- data.frame(x = x, y = dchisq(x,input$df))
        ggplot(datos)+geom_line(aes(x,y))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
