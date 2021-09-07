library(shiny)
library(DT)
library(tidyquant)
library(timetk)
library(tidyverse)
library(PerformanceAnalytics)
library(shinythemes)

ui <- fluidPage(theme=shinytheme("darkly"),
  titlePanel("UFFinance Asset Historical"),
  sidebarLayout(
    sidebarPanel(
      h3("UFFinance Quant"),
      h5('Criado por: Fernando Nogueira, Henrique Manoel e Pedro Amorim'),
      textInput("stock1",h3("Insira seu ativo aqui:"), "%5EBVSP"),
      img(src = "UFFinance.png", height = 225, width = 225)
    ),
    mainPanel(
      h1("Análise Histórica do ativo"),
      h4("O ativo escolhido foi:"),
      textOutput("stock1"),
      h2("Distribuição dos Retornos do Ativo"),
      plotOutput("returns",height="300px",width="100%"),
      h2("Volatilidade Histórica do Ativo"),
      plotOutput("volatilidade",height="300px",width="100%"),
      h2("Retornos Acumulados do Ativo"),
      plotOutput("cumulative",height="300px",width="100%"),
    )
  )
)




# Define server logic ----
server <- function(input, output) {
  output$stock1 <- renderText(input$stock1)
  output$returns <- renderPlot({
    stock = tq_get(input$stock1,
                   from = Sys.Date() %m-% months(60),
                   get = "stock.prices")
    daily_returns <- stock %>%
      tq_transmute(select = adjusted,           
                   mutate_fun = periodReturn,   
                   period = "daily") 
    
    ggplot() + geom_histogram(data = daily_returns, aes(x=daily.returns), fill="dodgerblue4") +
      labs(x="", y = "Contagem", title = "Histograma dos Retornos") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  output$cumulative <- renderPlot({
    stock = tq_get(input$stock1,
                   from = Sys.Date() %m-% months(60),
                   get = "stock.prices")
    daily_returns <- stock %>%
      tq_transmute(select = adjusted,           
                   mutate_fun = periodReturn,   
                   period = "daily") 
    cum_returns <- daily_returns %>%
      mutate(cr = cumprod(1 + daily.returns)) %>%      
      mutate(cumulative_returns = cr - 1)
    
      ggplot() +
        geom_line(data=cum_returns,aes(x = date, y = cumulative_returns), colour="dodgerblue4") +
        labs(x="Data",y="Retornos", title="Retornos Acumulados") +
        theme(plot.title = element_text(hjust = 0.5))
  })
  output$volatilidade <- renderPlot({
    stock = tq_get(input$stock1,
                   from = Sys.Date() %m-% months(60),
                   get = "stock.prices")
    
    daily_returns <- stock %>%
      tq_transmute(select = adjusted,           
                   mutate_fun = periodReturn,   
                   period = "daily") 
    
    cum_returns <- daily_returns %>%
      mutate(cr = cumprod(1 + daily.returns)) %>%      
      mutate(cumulative_returns = cr - 1)
    
    rolling_sd <- na.omit(rollapply(daily_returns$daily.returns, 60, 
                                    function(x) StdDev(x)))
    
    rolling_sd = as_tibble(rolling_sd)
    date_for_rolling = cum_returns[-c(1:59),]
    df_rolling = cbind(date_for_rolling$date,rolling_sd)
    colnames(df_rolling) = c("date", "value")
    
    ggplot(df_rolling, aes(x=date, y=value)) +
      geom_ribbon(aes(ymin=0, ymax=value),
                  fill = "navyblue", alpha=.4)+
      geom_line(color="dodgerblue4", lwd=.8) +
      labs(x="Data", y="Volatilidade", title="Volatilidade Histórica") +
      theme(plot.title = element_text(hjust = 0.5))
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)

