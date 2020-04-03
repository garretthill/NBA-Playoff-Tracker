#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinyWidgets)
library(ggplot2)
library(openxlsx)
library(tidyverse)
library(readxl)
library(dplyr)
library(zoo)
library(lubridate)
library(data.table)
library(highcharter)
library(chron)
library(openxlsx)
library(scales)
library(formattable)
library(rmarkdown)
library(kableExtra)
library(xts)
library(here)
library(reactable)
library(shiny)
library(datapasta)


rm(list=ls())


ui <- htmlTemplate("index.html",
                   
                   table1 = DT::dataTableOutput("table1"), 
                   playoffs33 = htmlOutput("playoffs123"),
                   away = uiOutput("my_button1"),
                   home = uiOutput("my_button2"),
                   east = uiOutput("my_button_east"),
                   west = uiOutput("my_button_west"),
                   table_playoff_east = reactableOutput("mytable_east"),
                   table_playoff_west = reactableOutput("mytable_west"),
                   final_results = htmlOutput("results123"),
                   away_team = textOutput("away_team"),
                   home_team = textOutput("home_team"),
                   away_icon33 = htmlOutput("away_icon"),
                   home_icon33 = htmlOutput("home_icon"),
                   away_color33 = htmlOutput("away_color"),
                   home_color33 = htmlOutput("home_color"),
                   away_pic33 = htmlOutput("away_pic"),
                   home_pic33 = htmlOutput("home_pic"),
                   away_name33 = htmlOutput("away_name"),
                   home_name33 = htmlOutput("home_name")
                   
                   
)



server = function(input,output,session) {
    
    dat<-readRDS(file = "all_games.rds")
    
    
    
    dat1<-dat %>% 
        filter(Away_Points <= 1) %>% 
        mutate(code1 = runif(1, 0, 1)) %>% 
        mutate(
            winner = case_when(
                Away_Points > Home_Points & code1 > Home_Points ~ Away,
                Home_Points > Away_Points & code1 > Away_Points ~ Home,
                Away_Points < Home_Points & code1 < Away_Points ~ Away,
                Home_Points < Away_Points & code1 < Home_Points ~ Home,
                TRUE                      ~  "other"
            )
        ) %>% 
        select(-c(code1))
    
    dat77<-as.data.frame(unique(dat$Away))
    
    names(dat77)<-'team'
    
    stop<-as.numeric(nrow(dat1))
    
    dat2<-dat %>% 
        filter(Away_Points >1)
    
    dat3<-rbind(dat2, dat1)
    
    output$my_button1 <- renderUI({actionButton("away", label = vals$Data$Away[abc()])})
    output$my_button2 <- renderUI({actionButton("home", label = vals$Data$Home[abc()])})
    
    output$my_button_east <- renderUI({actionButton("east", label = 'east')})
    output$my_button_west <- renderUI({actionButton("west", label = 'west')})
    
    output$away_pic <- renderUI({ 
      HTML(paste('<div class="game-prediction-column2">',
                 '<div class="game-prediction-column" style= "background-image:url(',
                 vals$Data$away_pic[abc()],
                 ') ; background-color: ',
                 vals$Data$away_color[abc()],';">',
                 '<div class="game-prediction-border-column" style= "background-color: ',vals$Data$away_color[abc()],';">',
                 '<div class="team-details">',
                 '<div class="team-icon"><img src="',
                 vals$Data$away_pic[abc()],
                 '" ></div>',
                 '<h1>',vals$Data$Away[abc()],'</h1>',
                 '<p>AWAY</p>
    </div>
    </div>'))
      
      
    })
    
    
    output$home_pic <- renderUI({ 
      HTML(paste('<div class="game-prediction-column2">',
                 '<div class="game-prediction-column" style= "background-image:url(',
                 vals$Data$home_pic[abc()],
                 ') ; background-color: ',
                 vals$Data$home_color[abc()],';">',
                 '<div class="game-prediction-border-column" style= "background-color: ',vals$Data$home_color[abc()],';">',
                 '<div class="team-details">',
                 '<div class="team-icon"><img src="',
                 vals$Data$home_pic[abc()],
                 '" ></div>',
                 '<h1>',vals$Data$Home[abc()],'</h1>',
                 '<p>HOME</p>
    </div>
    </div>'))
      
      
    })
    
    output$away_icon <- renderUI({ 
      HTML(paste('<div class="team-icon"><img src="',vals$Data$away_pic[abc()],'" ></div>'))
      
      
    })
      
      output$home_icon <- renderUI({ 
        HTML(paste( '<div class="team-icon"><img src="',vals$Data$home_pic[abc()],'" ></div>'))
             
             
      })
      
      
      
      output$away_pic3 <- renderUI({ 
        HTML(paste('<div class="game-prediction-column" style= "background-image:url(',vals$Data$away_pic[abc()],') ; background-color: ',
                   vals$Data$away_color[abc()],';">'))
        
        
      })
      
      output$home_pic3 <- renderUI({ 
        HTML(paste('<div class="game-prediction-column" style= "background-image:url(',vals$Data$home_pic[abc()],') ; background-color: ',
                   vals$Data$home_color[abc()],';">'))
        
        
      })
      
      output$away_name <- renderUI({ 
        HTML(paste('<h1>',vals$Data$Away[abc()],'</h1>'))
        
        
      })
      
      output$home_name <- renderUI({ 
        HTML(paste('<h1>',vals$Data$Home[abc()],'</h1>'))
        
        
      })
      
      output$away_color <- renderUI({ 
        HTML(paste('<div class="game-prediction-border-column" style= "background-color: ',vals$Data$away_color[abc()],';">'))
        
        
      })
      
      output$home_color <- renderUI({ 
        HTML(paste('<div class="game-prediction-border-column" style= "background-color: ',vals$Data$home_color[abc()],';">'))
        
        
        
        
      })
    
    output$away_team <- renderText({ 
      vals$Data$Away[abc()]
    })
    
    output$home_team <- renderText({ 
      vals$Data$Home[abc()]
    })
    
    vals=reactiveValues()
    vals$Data=data.table(dat3)
    
    vals2=reactiveValues()
    vals2$Data=data.table(dat3)
    
    output$selected_var <- renderText({ 
        input$Team
    })
    ###values <- reactiveValues()
    ###values$a<-1
    
    value <- reactiveVal(1) 
    
    counter <- reactiveVal(0) 
    
    newdata <- reactiveVal(100) 
    
    rank <- reactiveVal(100) 
    
    
    output$result1235 <- renderText({
      paste("You chose", value())
    })
    
    output$result123 <- renderText({
      paste("You chose", counter())
    })
    
    
    output$result1234 <- renderText({
      paste("You chose", newdata())
    })
    
    abc <- reactive({
        b<-which((dat3$Away == input$Team & dat3$Away_Points < 1) | (dat3$Home == input$Team & dat3$Away_Points < 1))
        b[value()]
    })
    
    newdata <- reactive({
      y<-which((dat3$Away == input$Team & dat3$Away_Points < 1) | (dat3$Home == input$Team & dat3$Away_Points < 1))
      length(y)
    })
    
    west_rank <- reactive({
      
      y<-which(dat11_west$Standings == input$Team )
      dat11_west$Standings[y]
    })
    
    east_rank <- reactive({
      
      y<-which(dat11_east$Standings == input$Team )
      dat11_east$Standings[y]
    })
    
    observeEvent(input$east, {
      
      
      
      removeUI(
        selector = 'div:has(> #front_page)'
      )
      
      
      
    }
)
    
    observeEvent(input$west, {
      
      
      
      removeUI(
        selector = 'div:has(> #front_page)'
      )
      
      
      
    }
    )
    
    observeEvent(input$eastDivisions, {
      
      
      
      removeUI(
        selector = 'div:has(> #divisions-page-east)'
      )
      
      
      
    }
    )
    
    observeEvent(input$westDivisions, {
      
      
      
      removeUI(
        selector = 'div:has(> #divisions-page-west)'
      )
      
      
      
    }
    )
    
    
    observeEvent(input$Team, {
        
        
        
        removeUI(
            selector = 'div:has(> #flexcontaineratlantic)'
        )
        
      
      removeUI(
        selector = 'div:has(> #flexcontainersoutheast)'
      )
      
      removeUI(
        selector = 'div:has(> #flexcontainercentral)'
      )
      
      removeUI(
        selector = 'div:has(> #flexcontainernorthwest)'
      )
      
      
      removeUI(
        selector = 'div:has(> #flexcontainerpacific)'
      )
      
      removeUI(
        selector = 'div:has(> #flexcontainersouthwest)'
      )
        #Load the mtcars table into a dataTable
        output$table1 = DT::renderDataTable({
            vals$Data %>% 
                filter(Away_Points <= 1) %>% 
                filter(Away == input$Team | Home == input$Team) %>%     
                slice(value())
            
            
            
            
            
        })
        
       
        
       
    })
    
    observeEvent(input$away, {
        vals$Data$winner[abc()] <- vals$Data$Away[abc()] 
        
        newValue <- value() + 1     # newValue <- rv$value - 1
        value(newValue)
        
        
        newcounter <- counter() + 1     # newValue <- rv$value - 1
        counter(newcounter)
        
    })   
    
    observeEvent(input$home, {
        vals$Data$winner[abc()] <- vals$Data$Home[abc()]
        
        newValue <- value() + 1     # newValue <- rv$value - 1
        value(newValue)  
        
        newcounter <- counter() + 1     # newValue <- rv$value - 1
        counter(newcounter)
        
    })  
    
   
    
    
    
    
    
    
    output$playoffs123 <- renderUI({
    if (counter() == newdata() & newdata() > 0){
      removeUI(
        selector = 'div:has(> #game-prediction)'
      )
     
      HTML(paste('<div class="game-prediction-border-column" style= "background-color: ',vals$Data$home_color[abc()],'; display:flex;">'))
      
      
      
      
    }
    
    
    else {
      
      HTML(paste('<div class="game-prediction-border-column" style= "background-color: ',vals$Data$home_color[abc()],'; display:none;">'))
     
      } 
    
      
    }
    )
    
    
    
    
    
    
    dat_final <- reactive({
        if (counter() == newdata() & newdata() > 0){
          
          
            
            dat1000 <- reactive({
                vals$Data
            })
            
            
            
            dat5 <- dat1000() %>% 
                group_by(winner) %>% 
                filter(winner != 'other') %>% 
                summarise(wins = n()) %>% 
                mutate(losses = 82-wins) %>% 
                mutate(pct = round(wins/82,3)) %>% 
                mutate(GB = max(wins) - wins) %>% 
                mutate(Home = wins/2) %>% 
                mutate(Away = wins/2) %>% 
                mutate(Div = wins/2) %>% 
                mutate(Con = wins/2) %>% 
                mutate(PPG = wins/2) %>% 
                mutate(OPG = wins/2) %>%
                mutate(Diff = wins/2) %>%
                mutate(Strk = wins/2) %>%
                mutate(L10 = wins/2) 
            
            conf<-read.csv('conf.csv', stringsAsFactors = F)
            
            dat6<-dat5 %>% 
                left_join(conf, by = c("winner" = "Name2"))
            
            divisions<-unique(conf$Divi)
            
            div_winner2<-NULL
            
            for (l in divisions) {
                
                
                
                se<-dat6 %>% 
                    filter(Divi == l)
                
                aaa<-as.numeric(max(se$wins))
                
                
                
                dat_tie_se<-se %>% 
                    filter(wins == aaa) 
                
                a<-as.numeric(unique(dat_tie_se$wins))
                
                
                
                total2<-NULL
                
                total3<-NULL
                
                
                
                if (nrow(dat_tie_se) > 1 ){
                    
                    
                    
                    
                    
                    total2<-NULL
                    
                    teams<-se %>% 
                        filter(wins == aaa)
                    
                    teams<-unique(teams$winner)
                    
                    h2h<-dat3 %>% 
                        filter(Away %in% teams & Home %in% teams)
                    
                    
                    
                    for (i in 1:length(teams)){
                        
                        
                        
                        h2h2<-h2h %>% 
                            filter(Away == teams[i]| Home == teams[i]) %>% 
                            mutate(h2h_wins = sum(winner == teams[i])/n())
                        
                        h2h_1 <- data.frame(Team=teams[i], 
                                            h2h_wins=h2h2$h2h_wins[i], 
                                            stringsAsFactors=FALSE)
                        
                        h2h<-dat3 %>% 
                            filter(Away %in% teams & Home %in% teams)
                        
                        h2h2<-h2h %>% 
                            filter(Away == teams[i]| Home == teams[i]) %>% 
                            mutate(h2h_wins = sum(winner == teams[i])/n())
                        
                        h2h_1 <- data.frame(Team=teams[i], 
                                            h2h_wins=h2h2$h2h_wins[i], 
                                            stringsAsFactors=FALSE)
                        
                        se_rec<-dat3 %>% 
                            filter(Away %in% se$winner & Home %in% se$winner )
                        
                        se_rec2<-se_rec %>% 
                            filter(Away == teams[i]| Home == teams[i]) %>% 
                            mutate(div_wins = sum(winner == teams[i])/n())
                        
                        div_2 <- data.frame(Team=teams[i], 
                                            div_wins=se_rec2$div_wins[i], 
                                            stringsAsFactors=FALSE)
                        
                        con1<-with(conf, Conf[Name2 == teams[i]])
                        
                        con2<-conf %>% 
                            filter(Conf == con1)
                        
                        
                        conf_wins<-dat3 %>% 
                            filter(Away %in% con2$Name2 & Home %in% con2$Name2) %>%  
                            filter(Away == teams[i]| Home == teams[i]) %>% 
                            mutate(conf_wins = sum(winner == teams[i])/n())
                        
                        con_2 <- data.frame(Team=teams[i], 
                                            conf_wins=conf_wins$conf_wins[i], 
                                            stringsAsFactors=FALSE)
                        
                        
                        top8<-dat6 %>% 
                            arrange(desc(wins)) %>% 
                            filter(Conf == con1) %>% 
                            mutate(need = wins[8]) %>% 
                            filter(wins >= need)
                        
                        
                        
                        
                        
                        top8_conf<-dat3 %>% 
                            filter(Away == teams[i]| Home == teams[i]) %>% 
                            filter(Away %in% top8$winner | Home %in% top8$winner) %>% 
                            mutate(conf_playoff_wins = sum(winner == teams[i])/n())
                        
                        
                        top8_conf_wins <- data.frame(Team=teams[i], 
                                                     top8_conf_wins=top8_conf$conf_playoff_wins[i], 
                                                     stringsAsFactors=FALSE)
                        
                        
                        
                        top8_nonconf<-dat6 %>% 
                            arrange(desc(wins)) %>% 
                            filter(Conf != con1) %>% 
                            mutate(need = wins[8]) %>% 
                            filter(wins >= need)
                        
                        
                        
                        
                        
                        top8_nonconf2<-dat3 %>% 
                            filter(Away == teams[i]| Home == teams[i]) %>% 
                            filter(Away %in% top8_nonconf$winner | Home %in% top8_nonconf$winner) %>% 
                            mutate(nonconf_playoff_wins = sum(winner == teams[i])/n())
                        
                        
                        top8_nonconf_wins <- data.frame(Team=teams[i], 
                                                        top8_nonconf_wins=top8_nonconf2$nonconf_playoff_wins[i], 
                                                        stringsAsFactors=FALSE)
                        
                        wins<-as.numeric(se$wins[se$winner==teams[i]])
                        
                        
                        total<-cbind(h2h_1[1],wins,h2h_1[2],div_2[2], con_2[2],top8_conf_wins[2],top8_nonconf_wins[2])
                        
                        
                        total2<-rbind(total2,total)
                        
                        total2<-total2[order(c(-total2$h2h_wins),(-total2$div_wins),(-total2$conf_wins),(-total2$top8_conf_wins),(-total2$top8_nonconf_wins)),]
                        
                        
                        
                    }
                    
                    total2<- rowid_to_column(total2, "tiebreaker")
                    
                    total3<-rbind(total3,total2)
                    
                    
                    
                    total3<-total3[order(c(-total3$wins), (total3$tiebreaker)),] 
                    
                    
                    div_winner<-total3$Team[1]
                    
                }
                
                
                
                else {
                    se<-se[order(c(-se$wins)),] 
                    
                    
                    
                    
                    div_winner<-se$winner[1]
                }
                
                div_winner2<-rbind(div_winner2,div_winner)
                
            }
            
            
            
            
            div_winner3<-as.data.frame(div_winner2[1:6])
            
            div_winner3$division_winner<-1
            
            names(div_winner3)<-c('winner', 'div_winner')
            
            dat7<-left_join(dat6,div_winner3)
            
            dat7[is.na(dat7)] <- 0
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            total<-NULL
            total2<-NULL
            total3<-NULL
            total4<-NULL
            
            conferences<-unique(conf$Conf)
            
            for (l in conferences) {
                
                
                conf_1<-dat7 %>% 
                    filter(Conf == l)
                
                dat_tie<-conf_1 %>% 
                    group_by(wins) %>% 
                    mutate(count= n()) %>% 
                    filter(count>1) %>% 
                    ungroup()
                
                a<-as.numeric(unique(dat_tie$wins))
                
                
                
                total2<-NULL
                
                total3<-NULL
                
                length(a)
                
                if (length(a) > 0 ){
                    
                    for (j in 1:length(a) ){
                        
                        
                        
                        
                        
                        total2<-NULL
                        
                        teams<-conf_1 %>% 
                            filter(wins == a[j])
                        
                        jjj<-as.numeric(nrow(teams))-1
                        
                        teams7<-unique(teams$winner)
                        
                        all_div<-with(conf, Divi[Name2 %in% teams])
                        
                        all_div2<-as.numeric(length(unique(all_div)))
                        
                        rid_of_team <- 'Jimmy'
                        
                        if (length(teams7) >2 ){
                            
                            
                            for (p in 1:jjj){  
                                
                                total2<-NULL
                                
                                
                                
                                
                                teams<-as.data.frame(teams) %>% 
                                    filter(winner != rid_of_team)
                                
                                teams2<-unique(teams$winner)
                                
                                
                                h2h<-dat3 %>% 
                                    filter(Away %in% teams & Home %in% teams)
                                
                                
                                
                                for (i in 1:length(teams2)){
                                    
                                    
                                    
                                    
                                    
                                    h2h2<-h2h %>% 
                                        filter(Away == teams2[i]| Home == teams2[i]) %>% 
                                        mutate(h2h_wins = sum(winner == teams2[i])/n())
                                    
                                    h2h_1 <- data.frame(Team=teams2[i], 
                                                        h2h_wins=h2h2$h2h_wins[i], 
                                                        stringsAsFactors=FALSE)
                                    
                                    h2h<-dat3 %>% 
                                        filter(Away %in% teams2 & Home %in% teams2)
                                    
                                    h2h2<-h2h %>% 
                                        filter(Away == teams2[i]| Home == teams2[i]) %>% 
                                        mutate(h2h_wins = sum(winner == teams2[i])/n())
                                    
                                    h2h_1 <- data.frame(Team=teams2[i], 
                                                        h2h_wins=h2h2$h2h_wins[i], 
                                                        stringsAsFactors=FALSE)
                                    
                                    
                                    divi2<-conf %>% 
                                        filter(Name2 == teams2[i]) %>% 
                                        select(Divi)
                                    
                                    divi3<-divi2[1,1]
                                    
                                    se<-dat6 %>% 
                                        filter(Divi == divi3)
                                    
                                    se_rec<-dat3 %>% 
                                        filter(Away %in% se$winner & Home %in% se$winner )
                                    
                                    se_rec2<-se_rec %>% 
                                        filter(Away == teams2[i]| Home == teams2[i]) %>% 
                                        mutate(div_wins = sum(winner == teams2[i])/n())
                                    
                                    if (as.numeric(all_div2) == 1){
                                        
                                        
                                        
                                        
                                        div_2 <- data.frame(Team=teams2[i], 
                                                            div_wins=se_rec2$div_wins[i], 
                                                            stringsAsFactors=FALSE)
                                        
                                    } else {div_2 <- data.frame(Team=teams2[i], 
                                                                div_wins=0, 
                                                                stringsAsFactors=FALSE) 
                                    }
                                    
                                    con1<-with(conf, Conf[Name2 == teams2[i]])
                                    
                                    con2<-conf %>% 
                                        filter(Conf == con1)
                                    
                                    
                                    conf_wins<-dat3 %>% 
                                        filter(Away %in% con2$Name2 & Home %in% con2$Name2) %>%  
                                        filter(Away == teams2[i]| Home == teams2[i]) %>% 
                                        mutate(conf_wins = sum(winner == teams2[i])/n())
                                    
                                    con_2 <- data.frame(Team=teams2[i], 
                                                        conf_wins=conf_wins$conf_wins[i], 
                                                        stringsAsFactors=FALSE)
                                    
                                    
                                    top8<-dat6 %>% 
                                        arrange(desc(wins)) %>% 
                                        filter(Conf == con1) %>% 
                                        mutate(need = wins[8]) %>% 
                                        filter(wins >= need)
                                    
                                    
                                    
                                    
                                    
                                    top8_conf<-dat3 %>% 
                                        filter(Away == teams2[i]| Home == teams2[i]) %>% 
                                        filter(Away %in% top8$winner | Home %in% top8$winner) %>% 
                                        mutate(conf_playoff_wins = sum(winner == teams2[i])/n())
                                    
                                    
                                    top8_conf_wins <- data.frame(Team=teams2[i], 
                                                                 top8_conf_wins=top8_conf$conf_playoff_wins[i], 
                                                                 stringsAsFactors=FALSE)
                                    
                                    
                                    
                                    top8_nonconf<-dat6 %>% 
                                        arrange(desc(wins)) %>% 
                                        filter(Conf != con1) %>% 
                                        mutate(need = wins[8]) %>% 
                                        filter(wins >= need)
                                    
                                    
                                    
                                    
                                    
                                    top8_nonconf2<-dat3 %>% 
                                        filter(Away == teams2[i]| Home == teams2[i]) %>% 
                                        filter(Away %in% top8_nonconf$winner | Home %in% top8_nonconf$winner) %>% 
                                        mutate(nonconf_playoff_wins = sum(winner == teams2[i])/n())
                                    
                                    
                                    top8_nonconf_wins <- data.frame(Team=teams2[i], 
                                                                    top8_nonconf_wins=top8_nonconf2$nonconf_playoff_wins[i], 
                                                                    stringsAsFactors=FALSE)
                                    
                                    wins<-as.numeric(se$wins[se$winner==teams2[i]])
                                    
                                    division_winner<-as.numeric(dat7$div_winner[dat7$winner==teams2[i]])
                                    
                                    
                                    total<-cbind(h2h_1[1],wins,division_winner,h2h_1[2],div_2[2], con_2[2],top8_conf_wins[2],top8_nonconf_wins[2])
                                    
                                    
                                    total2<-rbind(total2,total)
                                    
                                    total2<-total2[order(c(-total2$h2h_wins),(-total2$division_winner),(-total2$div_wins),(-total2$conf_wins),(-total2$top8_conf_wins),(-total2$top8_nonconf_wins)),]
                                    
                                    
                                    
                                }
                                
                                
                                
                                total2<- rowid_to_column(total2, "tiebreaker")
                                
                                total2$tiebreaker<-total2$tiebreaker -1 + p
                                
                                if (nrow(total2) > 2){
                                    
                                    total3<-rbind(total3,total2[1,])
                                    
                                    rid_of_team<-total2$Team[1]
                                    
                                } else {
                                    
                                    total3<-rbind(total3,total2)
                                }
                                
                            }
                            
                            
                            
                            
                            
                            
                        } else {
                            
                            total2<-NULL
                            
                            teams<-as.data.frame(teams) 
                            
                            teams2<-unique(teams$winner)
                            
                            
                            h2h<-dat3 %>% 
                                filter(Away %in% teams & Home %in% teams)
                            
                            
                            for (i in 1:length(teams2)){
                                
                                
                                
                                
                                
                                h2h2<-h2h %>% 
                                    filter(Away == teams2[i]| Home == teams2[i]) %>% 
                                    mutate(h2h_wins = sum(winner == teams2[i])/n())
                                
                                h2h_1 <- data.frame(Team=teams2[i], 
                                                    h2h_wins=h2h2$h2h_wins[i], 
                                                    stringsAsFactors=FALSE)
                                
                                h2h<-dat3 %>% 
                                    filter(Away %in% teams2 & Home %in% teams2)
                                
                                h2h2<-h2h %>% 
                                    filter(Away == teams2[i]| Home == teams2[i]) %>% 
                                    mutate(h2h_wins = sum(winner == teams2[i])/n())
                                
                                h2h_1 <- data.frame(Team=teams2[i], 
                                                    h2h_wins=h2h2$h2h_wins[i], 
                                                    stringsAsFactors=FALSE)
                                
                                
                                divi2<-conf %>% 
                                    filter(Name2 == teams2[i]) %>% 
                                    select(Divi)
                                
                                divi3<-divi2[1,1]
                                
                                se<-dat6 %>% 
                                    filter(Divi == divi3)
                                
                                se_rec<-dat3 %>% 
                                    filter(Away %in% se$winner & Home %in% se$winner )
                                
                                se_rec2<-se_rec %>% 
                                    filter(Away == teams2[i]| Home == teams2[i]) %>% 
                                    mutate(div_wins = sum(winner == teams2[i])/n())
                                
                                if (as.numeric(all_div2) == 1){
                                    
                                    
                                    
                                    
                                    div_2 <- data.frame(Team=teams2[i], 
                                                        div_wins=se_rec2$div_wins[i], 
                                                        stringsAsFactors=FALSE)
                                    
                                } else {div_2 <- data.frame(Team=teams2[i], 
                                                            div_wins=0, 
                                                            stringsAsFactors=FALSE) 
                                }
                                
                                con1<-with(conf, Conf[Name2 == teams2[i]])
                                
                                con2<-conf %>% 
                                    filter(Conf == con1)
                                
                                
                                conf_wins<-dat3 %>% 
                                    filter(Away %in% con2$Name2 & Home %in% con2$Name2) %>%  
                                    filter(Away == teams2[i]| Home == teams2[i]) %>% 
                                    mutate(conf_wins = sum(winner == teams2[i])/n())
                                
                                con_2 <- data.frame(Team=teams2[i], 
                                                    conf_wins=conf_wins$conf_wins[i], 
                                                    stringsAsFactors=FALSE)
                                
                                
                                top8<-dat6 %>% 
                                    arrange(desc(wins)) %>% 
                                    filter(Conf == con1) %>% 
                                    mutate(need = wins[8]) %>% 
                                    filter(wins >= need)
                                
                                
                                
                                
                                
                                top8_conf<-dat3 %>% 
                                    filter(Away == teams2[i]| Home == teams2[i]) %>% 
                                    filter(Away %in% top8$winner | Home %in% top8$winner) %>% 
                                    mutate(conf_playoff_wins = sum(winner == teams2[i])/n())
                                
                                
                                top8_conf_wins <- data.frame(Team=teams2[i], 
                                                             top8_conf_wins=top8_conf$conf_playoff_wins[i], 
                                                             stringsAsFactors=FALSE)
                                
                                
                                
                                top8_nonconf<-dat6 %>% 
                                    arrange(desc(wins)) %>% 
                                    filter(Conf != con1) %>% 
                                    mutate(need = wins[8]) %>% 
                                    filter(wins >= need)
                                
                                
                                
                                
                                
                                top8_nonconf2<-dat3 %>% 
                                    filter(Away == teams2[i]| Home == teams2[i]) %>% 
                                    filter(Away %in% top8_nonconf$winner | Home %in% top8_nonconf$winner) %>% 
                                    mutate(nonconf_playoff_wins = sum(winner == teams2[i])/n())
                                
                                
                                top8_nonconf_wins <- data.frame(Team=teams2[i], 
                                                                top8_nonconf_wins=top8_nonconf2$nonconf_playoff_wins[i], 
                                                                stringsAsFactors=FALSE)
                                
                                wins<-as.numeric(se$wins[se$winner==teams2[i]])
                                
                                division_winner<-as.numeric(dat7$div_winner[dat7$winner==teams2[i]])
                                
                                
                                total<-cbind(h2h_1[1],wins,division_winner,h2h_1[2],div_2[2], con_2[2],top8_conf_wins[2],top8_nonconf_wins[2])
                                
                                total2<-rbind(total2,total)
                                
                                total2<-total2[order(c(-total2$h2h_wins),(-total2$division_winner),(-total2$div_wins),(-total2$conf_wins),(-total2$top8_conf_wins),(-total2$top8_nonconf_wins)),]
                                
                                
                            }
                            
                            
                            
                            total2<- rowid_to_column(total2, "tiebreaker")
                            
                            total3<-rbind(total3,total2)
                            
                        }
                        
                    }
                    
                }
                
                total4<-rbind(total4,total3)
                
            }
            
            
            
            
            
            
            
            
            
            
            dat8<-left_join(dat7,total4[,1:2], by = c('winner'= 'Team'))
            
            dat8[is.na(dat8)] <- 0
            
            
            
            dat8_west<-dat8 %>% 
                filter(Conf == 'West')
            
            dat8_east<-dat8 %>% 
                filter(Conf == 'East')
            
            
            dat8_west2<-dat8_west[order(c(-dat8_west$wins),(dat8_west$tiebreaker)),]
            
            
            dat8_east2<-dat8_east[order(c(-dat8_east$wins),(dat8_east$tiebreaker)),]
            
            dat9_west<- rowid_to_column(dat8_west2, "Standings")
            
            dat10_west<-dat9_west %>% 
                mutate(
                    group = case_when(
                        Standings < 9 ~ 'A',
                        TRUE                      ~  "B"
                    )
                )
            
            dat11_west<-dat10_west %>% 
              select(c(Standings,winner,wins,losses,GB,group, Conf))
            
            dat9_east<- rowid_to_column(dat8_east2, "Standings")
            
            dat10_east<-dat9_east %>% 
              mutate(
                group = case_when(
                  Standings < 9 ~ 'A',
                  TRUE                      ~  "B"
                )
              )
            
            dat11_east<-dat10_east %>% 
              select(c(Standings,winner,wins,losses,GB,group, Conf))
            
            extra_info2<-read.csv('extra info 2.csv')
            
            dat22_west<-inner_join(dat11_west, extra_info2)
            
            dat22_west$Opponent2<-dat22_west$winner[dat22_west$Opponent]
            
            dat22_east<-inner_join(dat11_east, extra_info2)
            
            dat22_east$Opponent2<-dat22_east$winner[dat22_east$Opponent]
            
            dat12_a<-rbind(dat22_east,dat22_west)
            
            extra_info<-read.csv('extra info.csv')
            
            dat12_b<-inner_join(dat12_a, extra_info)
            
            extra_info3<-extra_info %>% 
              select(c(1,4))
            
            names(extra_info3)<-c('Opponent2','name2')
            
            dat12<-inner_join(dat12_b,extra_info3)
            
           
            
            
            dat13 <- dat12 %>%
              mutate(
                name3 = case_when(
                  Standings < 9  ~ as.character(name2),
                  TRUE                      ~  "None"
                )
              )
            
            dat13
            
        }
           
            })
              
    output$mytable_east = renderReactable({   
      
      if (counter() == newdata() & newdata() > 0){
      
    dat_playoff_table_east<-dat_final() %>% 
      filter(Conf == 'East' ) %>% 
      select(c(Standings,winner,wins,losses,group))
    
    
    format_pct <- function(value) {
      if (value == 'A') "\u2714"    # en dash  # checkmark
      else "  \U1F5D9 "
    }
    
    group_column <- function(class = NULL, ...) {
      colDef(cell = format_pct, minWidth = 80, align = "center", ...)
    }
      
    box_score_tbl1 <- reactable(dat_playoff_table_east,
                                pagination = FALSE,
                                defaultSortOrder = "asc",
                                defaultSorted = c("group", "Standings"),
                                defaultColDef = colDef(
                                  sortNALast = TRUE,
                                  minWidth = 45,
                                  class = JS("function(rowInfo, colInfo, state) {
        // Highlight sorted columns
        for (var i = 0; i < state.sorted.length; i++) {
          if (state.sorted[i].id === colInfo.id) {
            return 'sorted'
          }
        }
      }"),
                                  
                                  
                                  
                                  headerClass = "box-score-header",
                                ),
                                columns = list(
                                  Standings = colDef(name = "Rk", minWidth = 30, align = "center"),
                                  winner = colDef(name = "Team", minWidth = 200, align = "left"),
                                  wins = colDef(name = "W", minWidth = 50, align = "center"),
                                  losses = colDef(name = "L", minWidth = 50, align = "center"),
                                  
                                  
                                  
                                  group = group_column(name = "Playoffs", class = "group")
                                                
                                ),
                                rowClass = JS("
 function(rowInfo, state) {
      // Add horizontal separators between groups when sorting by group
      const firstSorted = state.sorted[0]
      if (firstSorted && firstSorted.id === 'group') {
        const nextRow = state.pageRows[rowInfo.viewIndex + 1]
        if (nextRow && rowInfo.row.group !== nextRow.group) {
          return 'group-last'
        }
      }
    }"),
                                showSortIcon = FALSE,
                                highlight = TRUE,
                                striped = TRUE,
                                class = "box-score-tbl")
    
    
    box_score_tbl1 
    
      }
    }
    )
    
    output$mytable_west = renderReactable({   
      
      if (counter() == newdata() & newdata() > 0){
      
      dat_playoff_table_west<-dat_final() %>% 
        filter(Conf == 'West' ) %>% 
        select(c(Standings,winner,wins,losses,group))
      
      format_pct <- function(value) {
        if (value == 'A') "\u2714"    # en dash  # checkmark
        else "  \U1F5D9 "
      }
      
      group_column <- function(class = NULL, ...) {
        colDef(cell = format_pct, minWidth = 80, align = "center", ...)
      }
      
      box_score_tbl1 <- reactable(dat_playoff_table_west,
                                  pagination = FALSE,
                                  defaultSortOrder = "asc",
                                  defaultSorted = c("group", "Standings"),
                                  defaultColDef = colDef(
                                    sortNALast = TRUE,
                                    minWidth = 45,
                                    class = JS("function(rowInfo, colInfo, state) {
        // Highlight sorted columns
        for (var i = 0; i < state.sorted.length; i++) {
          if (state.sorted[i].id === colInfo.id) {
            return 'sorted'
          }
        }
      }"),
                                    
                                    
                                    
                                    headerClass = "box-score-header",
                                  ),
                                  columns = list(
                                    Standings = colDef(name = "Rk", minWidth = 30, align = "center"),
                                    winner = colDef(name = "Team", minWidth = 200, align = "left"),
                                    wins = colDef(name = "W", minWidth = 50, align = "center"),
                                    losses = colDef(name = "L", minWidth = 50, align = "center"),
                                    
                                    
                                    
                                    group = group_column(name = "Playoffs", class = "group")
                                  ),
                                  rowClass = JS("
 function(rowInfo, state) {
      // Add horizontal separators between groups when sorting by group
      const firstSorted = state.sorted[0]
      if (firstSorted && firstSorted.id === 'group') {
        const nextRow = state.pageRows[rowInfo.viewIndex + 1]
        if (nextRow && rowInfo.row.group !== nextRow.group) {
          return 'group-last'
        }
      }
    }"),
                                  showSortIcon = FALSE,
                                  highlight = TRUE,
                                  striped = TRUE,
                                  class = "box-score-tbl")
      
      
      box_score_tbl1 
      
      }
    }
    )
          
   
    rank <- reactive({
      
      dat55 <- dat_final() %>% 
        filter(winner == input$Team) 
      
      dat55$Standings[[1]]
    })
    
    
    team_picture <- reactive({
      
      dat55 <- dat_final() %>% 
        filter(winner == input$Team) 
      
      dat55$home_pic[[1]]
    })
    
    short_name <- reactive({
      
      dat55 <- dat_final() %>% 
        filter(winner == input$Team) 
      
      dat55$name[[1]]
    })
    
    final_wins <- reactive({
      
      dat55 <- dat_final() %>% 
        filter(winner == input$Team) 
      
      dat55$wins[[1]]
    })
    
    final_losses <- reactive({
      
      dat55 <- dat_final() %>% 
        filter(winner == input$Team) 
      
      dat55$losses[[1]]
    })
    
    final_place <- reactive({
      
      dat55 <- dat_final() %>% 
        filter(winner == input$Team) 
      
      dat55$Place[[1]]
    })
    
    final_opp <- reactive({
      
      dat55 <- dat_final() %>% 
        filter(winner == input$Team) 
      
      dat55$name3[[1]]
    })
    
    
    
    
    output$results123 <- renderUI({
      
      if (counter() == newdata() & newdata() > 0){
        
        removeUI(
          selector = 'div:has(> #game-prediction)'
        )
        
      
      
     
      
        
        if(rank() < 9){
          HTML(paste('<div class="container2">
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
              <div class="confetti"></div>
             </div>
             
             <div id="results_good_page">
    <div id="results_good">
        <div class="results_text"><h1>The ',input$Team, 'Made The Playoffs</h1></div>
        <div class="results_row">
          <div class="results-column2">      
            <div class="results-column">
                <img src="',team_picture(),'">
            </div>
            <p>Congratulations! It looks like the ' ,input$Team,' had the right recipe for a winning team.</p>
        </div>
    </div>
          
    <table id="team_table" class="team_table1">
        <thead>
          <tr>
            <th class="uppercase">Team</th>
            <th class="uppercase">Wins</th>
            <th class="uppercase">Losses</th>
            <th class="uppercase">Place</th>
            <th class="uppercase">1st Round Matchup</th>
            
          </tr>
        </thead>
        <tbody>
          <tr>
            <td id="user-team" class="text-gray-4">',short_name(),'</td>
            <td id="user-q0">',final_wins(),'</td>
            <td id="user-q1">',final_losses(),'</td>
            <td id="user-q2">',final_place(),'</td>
            <td id="user-q3">',final_opp(),'</td>
            
          </tr>
         
        </tbody>
      </table>
          
          
                     
    
      </div>
      </div>
   

      '))
          
          
          
          
        }
        
        
        else {
          
          HTML(paste('<div id="results_good_page">
    <div id="results_good">
        <div class="results_text"><h1>The ',input$Team, 'DID NOT Make The Playoffs</h1></div>
        <div class="results_row">
    
    
          <div class="results-column2">      
            <div class="results-column">
                <img src="',team_picture(),'">
            </div>
            <p>Unfortunately, it looks like the ' ,input$Team,' did not have the right combination this year.</p>
        </div>
    </div>
          
    <table id="team_table" class="team_table1">
        <thead>
          <tr>
            <th class="uppercase">Team</th>
            <th class="uppercase">Wins</th>
            <th class="uppercase">Losses</th>
            <th class="uppercase">Place</th>
            <th class="uppercase">1st Round Matchup</th>
            
          </tr>
        </thead>
        <tbody>
          <tr>
           <td id="user-team" class="text-gray-4">',short_name(),'</td>
            <td id="user-q0">',final_wins(),'</td>
            <td id="user-q1">',final_losses(),'</td>
            <td id="user-q2">',final_place(),'</td>
            <td id="user-q3">',final_opp(),'</td>
            
            
          </tr>
         
        </tbody>
      </table>
          
                    
         
    
      </div>
      </div>
   

      '))
          
        }
        
       
      }
        
      }
    
    )
     
    
    
}




shinyApp(ui,server)
