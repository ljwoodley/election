library(tidyverse)
library(shiny)
library(shinythemes)


df <- read.csv("df.csv")

# Define UI for application
ui <- navbarPage("Election Results", theme = shinytheme("flatly"),
                 
      tabPanel("County Comission District",
            
               plotOutput("ccdplot"),
              
              br(),
              br(),
              
           fluidRow(column(6,
               
               radioButtons("ccdInput","Election Type", choices = c("2014 Governor Election", "2015 Mayoral Election","2016 Presidental Election"),
                            selected = "2014 Governor Election")
      
            )
      )
),

      tabPanel("School Board District",
         
               plotOutput("sbdplot"),
         
              br(),
              br(),
       
         
         fluidRow(column(6,
                         
              radioButtons("sbdInput","Election Type", choices = c("2014 Governor Election", "2015 Mayoral Election","2016 Presidental Election"),
                                      selected = "2014 Governor Election")

                          )
                         )    
                  )
)

#--------------SERVEER-------------------------------#
server <- function(input, output) {

  #create dataset
  df_ccd <- df %>% select(election, party, ccd, candidate_issue, totalvotes) %>% 
    group_by(election,ccd, party, candidate_issue) %>% 
    summarise(total = sum(totalvotes)) %>% 
    group_by(election,ccd) %>% 
    mutate(percentage = (round(total/sum(total)*100,0)),
           position = rank(-percentage))
  
  
  #reorder
  df_ccd$ccd <- factor(df_ccd$ccd, levels = (1:14))
  
  #make dataset reactive
  ccd_df <- reactive({df_ccd %>% 
      filter(election == input$ccdInput)
  })
  
  #plot data
  output$ccdplot <- renderPlot({
    ggplot(ccd_df(), aes(x=ccd ,y=percentage, fill=reorder(party,-percentage), group=position))+
    geom_bar(stat="identity",position = "dodge")+
    scale_fill_manual("Party", values = c("REP"="red4","DEM"="blue4","OTH"="green4"))+
    theme_minimal(base_size = 20)+
    labs(x="County Comission District", y="Votes (%)")+
    geom_text(aes(label=(paste0(percentage,"%"))), position = position_dodge(width=0.9), vjust=-0.35, size=4)+
    scale_y_continuous(limits = c(0,100), breaks = c(20,40,60,80,100))

})

  #create dataset
  df_sbd <- df %>% select(election, party, sbd, candidate_issue, totalvotes) %>% 
    group_by(election,sbd, party, candidate_issue) %>% 
    summarise(total = sum(totalvotes)) %>% 
    group_by(election,sbd) %>% 
    mutate(percentage = (round(total/sum(total)*100,0)),
           position = rank(-percentage))
  
  
  #reorder
  df_sbd$sbd <- factor(df_sbd$sbd, levels = (1:7))
  
  #make dataset reactive
  sbd_df <- reactive({df_sbd %>% 
      filter(election == input$sbdInput)
  })
  
  output$sbdplot <- renderPlot({
    ggplot(sbd_df(), aes(x=sbd ,y=percentage, fill=reorder(party,-percentage), group=position))+
      geom_bar(stat="identity",position = "dodge")+
      scale_fill_manual("Party", values = c("REP"="red4","DEM"="blue4","OTH"="green4"))+
      theme_minimal(base_size = 20)+
      labs(x="School Board District", y="Votes (%)")+
      geom_text(aes(label=(paste0(percentage,"%"))), position = position_dodge(width=0.9), vjust=-0.35, size=4)+
      scale_y_continuous(limits = c(0,100), breaks = c(20,40,60,80,100))
    
  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)



