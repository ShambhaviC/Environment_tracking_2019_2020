require(shiny)
require(rhandsontable)
require(lubridate)
require(tidyverse)
require(stringr)

ui <- fluidPage(
  titlePanel("Daily Analysis"), 
  
  sidebarLayout(position = "left",
                sidebarPanel(fileInput('file1', 'Choose CSV File',
                                       accept=c('text/csv', 
                                                'text/comma-separated-values,text/plain', 
                                                '.csv')),
                             #sliderInput(inputId = "Volume_visit", "Volume Per Visit", min = 0, max = 100, value = 20), 
                             sliderInput(inputId = "Conc", "Concentration of Nectar", 
                                         min = 0, max = 100, value = 17), #this needs to be an input where the user can enter in the volume per visit
                             #sliderInput(inputId = "dur", "Min duration for poke", 
                              #           min=0, max=1000, value=200),
                             sliderInput(inputId = "durlong", "Min duration for visits considered to be 'long'", 
                                         min=2000, max=10000, value=3000),
                             sliderInput(inputId = "pumpconv", "Number of pump steps per microLitre of liquid", 
                                         min=0, max=0.5, value= 0.324), 
                             sliderInput(inputId = "amplitude", "Amplitude of the sine function", 
                                         min = 0, max = 50, value = 35),
                             sliderInput(inputId = "displacement", "Displacement of the initial value of the wave above 0", 
                                         min = 0, max = 50, value = 41)
                ),
                mainPanel(h3("Volumes and Energy Consumed"),
                          plotOutput("Rewards"),
                          h3("Total Volume Consumed (mL)"),
                          textOutput("Total_Volume"),
                          h3("Total visits to the flowers"), 
                          plotOutput("Vis_flowers"),
                          h3("Visit Parameters"),
                          rHandsontableOutput("Visits"),
                          h3("Visit durations"), 
                          plotOutput("Vis_dur"),
                          h3("Rewarded Visits"),
                          rHandsontableOutput("Unrew_vis"),
                          h3("Pump Refills"),
                          tableOutput("refills"),
                         #h3("Light Barrier Breaks"),
                         #plotOutput("LS_breaks_count"),
                         #h3("Alternations"),
                         #rHandsontableOutput("Alternations")
                          h3("Summary of the training phase"),
                          plotOutput("Training"),
                          h3("Summary of the main experimental phase (tracking bats)"),
                          plotOutput("trackers"), 
                          h3("Summary of the main experimental phase (non-tracking bats)"),
                          plotOutput("nontrackers") 
                         
                         
                )
  )
)

server <- function(input, output) {
  
  dataplus <- reactive({
    
  inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
################
#General plots and summaries
################
#####
#Selection and Preparation of the CSV file for Analysis
#####
#Selection of data table

  data_table <- read.csv2(inFile$datapath, header=TRUE, dec=",",sep=";", 
                          fileEncoding="UTF-16LE", as.is = T, row.names=NULL) #read csv file with raw data
  data_table$DateTime <- sub(",",".",data_table$DateTime)
  data_table$DateTime <- as.character(as.POSIXct(as.numeric(data_table$DateTime) * (60*60*24)
                                                 , origin="1899-12-30", tz="GMT")) #Changing format of DateTime
  
  
  firstrow <- max(which(data_table$SystemMsg == "start")) + 1 
  lastrow <- nrow(data_table) 
  data_table <- data_table %>% slice(firstrow:lastrow) #Find useful data
  data_table <- data_table %>% arrange(DateTime) #Sort visits chronologically 
  
  })
  
  output$table <- renderTable({
    dataplus()
  })
  
#####
#Calculation of calories consumed and volume of reward consumed
#####
#Note the rewarded visits
output$Rewards <- renderPlot({
  mydata <- dataplus()
#Calculation of total volumes consumed
  mydata <- mydata %>% filter(str_detect(IdLabel, "Bat")) %>% #Remove those visits made by a wand
    mutate(reinforce1value = ifelse(is.na(reinforce1value), 0, reinforce1value)) %>% 
    mutate(reward = ifelse(reinforce1value != 0, 1, 0)) %>% 
    filter(reward == 1) #Insert variable for reinforcement
  
Rewards <- mydata %>% 
  group_by(IdLabel) %>% summarise(reward_vol = sum(reinforce1value)*input$pumpconv) 
#Rewards <- mydata %>% filter(reward == 1) %>% group_by(IdLabel) %>% summarise(reward = n()) %>%
    #mutate(reward_vol = reward*input$Volume_visit)

#Calculation of the number of kJ consumed
Rewards <- Rewards %>% mutate(kJ = 15.96/1000000*reward_vol*(0.05298*input$Conc^2+9.56955*input$Conc+3.32727))
Rewards <- Rewards %>% mutate(reward_vol = reward_vol/1000)
#  ggplot(aes(IdLabel, kJ, color = IdLabel, label = reward_vol)) + #Turning it into a graph
  ggplot(Rewards, aes(IdLabel, kJ, color = IdLabel, label = reward_vol)) + 
    geom_hline(yintercept = 25, linetype = 2) +
    geom_text() + xlab("Bat")
})
  
output$Total_Volume <- renderText({
  mydata <-dataplus()
  mydata <- mydata %>% filter(str_detect(IdLabel, "Bat")) %>% #Remove those visits made by a wand
    mutate(reinforce1value = ifelse(is.na(reinforce1value), 0, reinforce1value)) %>% 
    mutate(reward = ifelse(reinforce1value != 0, 1, 0)) %>% 
    filter(reward == 1) #Insert variable for reinforcement
  Total<- mydata %>% group_by(IdLabel) %>% summarise(reward_vol = sum(reinforce1value)*input$pumpconv)
  Total_Volume <- sum(Total$reward_vol)/1000
  })

#####
#Calculation of Visit Parameters 
#####

#Calculating total number of visits

output$Vis_flowers <- renderPlot({
  mydata <- dataplus()
  mydata <- mydata %>% filter(str_detect(IdLabel, "Bat")) %>% #Remove those visits made by a wand
    mutate(reinforce1value = ifelse(is.na(reinforce1value), 0, reinforce1value)) %>% 
    mutate(reward = ifelse(reinforce1value != 0, 1, 0)) %>% 
    filter(reward == 1) #Insert variable for reinforcement
  Vis_flowers <- subset(mydata, select = c(2:5)) %>% filter(eventDuration < 60000) %>% #Selecting the visits to the flowers
    mutate(Flower = as.numeric(str_extract(unitLabel, "[0-9]+"))) %>%
    filter(str_detect(unitLabel,"Cond")) %>%
    group_by(IdLabel, Flower) %>% summarise(Visits = n())
  colnames(Vis_flowers) <- c("IdLabel", "Flower", "Total_Visits") #Final table for visit parameters
  ggplot(Vis_flowers, aes(Flower, Total_Visits, label = Total_Visits, fill = IdLabel)) + 
    geom_bar(stat = "identity", color = "black") + 
    geom_text(vjust = -0.5) +
    ylab("Number of visits") + 
    scale_x_continuous(breaks = seq(0,12,1)) +
    theme_bw()
})

output$Visits <- renderRHandsontable ({
  mydata <- dataplus()
  mydata <- mydata %>% filter(str_detect(IdLabel, "Bat")) %>% #Remove those visits made by a wand
    mutate(reinforce1value = ifelse(is.na(reinforce1value), 0, reinforce1value)) %>% 
    mutate(reward = ifelse(reinforce1value != 0, 1, 0)) %>% 
    filter(reward == 1) #Insert variable for reinforcement
  Vis_flowers <- subset(mydata, select = c(2:5)) %>% 
    filter(eventDuration < 60000) %>% #Selecting the visits to the flowers
    mutate(Flower = as.numeric(str_extract(unitLabel, "[0-9]+"))) %>%
    group_by(Flower) %>% summarise(Visits = n())
  Visits <- select(mydata, IdLabel, unitLabel, eventDuration) %>% mutate(Flower = as.numeric(str_extract(unitLabel, "[0-9]+"))) %>%
    mutate(Cage_number = case_when(Flower == 1| Flower == 2 ~ 1, 
                                   Flower == 3| Flower == 4 ~ 2, 
                                   Flower == 5| Flower == 6 ~ 3, 
                                   Flower == 7| Flower == 8 ~ 4, 
                                   Flower == 9| Flower == 10 ~ 5, 
                                   Flower == 11 | Flower == 12 ~ 6))
  Visits <- Visits %>% filter(eventDuration < 60000) %>% 
    filter(str_detect(unitLabel,"Cond")) %>% 
    group_by(Cage_number, IdLabel) 
  #Calculating percentage of long visits and the length of the longest visit
  Visits <- Visits %>%   
    mutate(is.long = ifelse(eventDuration > input$durlong, 1, 0)) %>%
    summarise(perclong = sum(is.long)/n(), maxduration = max(eventDuration)/1000, eventDuration = n())
  names(Visits)[5]<-"Total_Visits"
  names(Visits)[1]<-"Cage Number" #Final table for visit parameters
  Visits <- Visits[,c(2,1,5,3,4)]
  rhandsontable(Visits, rowHeaders = NULL) %>% hot_heatmap()
})

output$Vis_dur <- renderPlot({
  mydata <- dataplus()
  mydata <- mydata %>% filter(str_detect(IdLabel, "Bat")) %>% #Remove those visits made by a wand
    mutate(reinforce1value = ifelse(is.na(reinforce1value), 0, reinforce1value)) %>% 
    mutate(reward = ifelse(reinforce1value != 0, 1, 0)) %>% 
    filter(reward == 1) #Insert variable for reinforcement
  Vis_dur <- subset(mydata, select = c(2:5)) %>% filter(eventDuration < 60000) %>% #Selecting the visits to the flowers
    mutate(Flower = as.numeric(str_extract(unitLabel, "[0-9]+"))) %>% 
    group_by(IdLabel) %>% mutate(eventDuration = eventDuration/1000)
  Vis_dur %>%
    ggplot(aes(as.numeric(eventDuration))) +
    geom_density() +
    facet_wrap( IdLabel ~., scales = 'free') +
    scale_x_log10() +
    theme_bw() +
    xlab("Visit durations [s]") 
})

#Calculating the number of unrewarded visits
output$Unrew_vis <- renderRHandsontable({
  mydata <- dataplus()
  Unrew_vis <- mydata 
  a <- which(Unrew_vis$SystemMsg == "start pump")
  b <- which(Unrew_vis$SystemMsg == "end pump")
  
  for   (i in 1:(length(a))) { 
    
    k<-Unrew_vis[a[i]:b[i],] 
    
    if(length(k$SystemMsg)!=0) {#if the length between two occurrences is not 0, i.e., discounting RFID events
      
      Unrew_vis$SystemMsg[a[i]:b[i]] <-"refill"
    }
  }
  
  Unrew_vis <- Unrew_vis %>% filter(SystemMsg != "refill")
  
  Unrew_vis <- Unrew_vis %>%
    filter(str_detect(IdLabel, "Bat")) %>% #remove test transponder visits
    filter(str_detect(unitLabel, "CondMod")) %>% 
    mutate(reinforce1value = ifelse(is.na(reinforce1value), 0, 1), 
           Rew = ifelse(reinforce1value == 0, 0, 1), 
           Flower = as.numeric(str_extract(unitLabel, "[0-9]+"))) %>% 
    select(IdLabel,unitLabel,Rew, Flower) 
  
  Flw_bat <- Unrew_vis %>% select(IdLabel, Flower, unitLabel) %>% 
    group_by(unitLabel, IdLabel) %>% 
    count(Flower = mean(Flower)) %>% 
    ungroup() %>%
    select(Flower, IdLabel)
  
  Unrew_vis <- Unrew_vis %>% 
    group_by(Flower, IdLabel) %>% 
    summarise(Rewarded = mean(Rew)) %>% 
    transmute(Rewarded = Rewarded*100)
  
  Unrew_vis <- left_join(Unrew_vis, Flw_bat, by = "Flower", all.x = TRUE) %>% 
    select(IdLabel, Flower, Rewarded) %>% rename(Bat = IdLabel, PercRewardedVisits = Rewarded)
  
rhandsontable(Unrew_vis, rowHeaders = NULL) %>% hot_heatmap()
  
})

#####
#Number of Refill Events
#####

output$refills <- renderTable({
  mydata <- dataplus()
  refills <- mydata %>% filter(str_detect(SystemMsg, "start pump")) %>% summarise(Events = n())
})

#####
#Experiment-specific plots
#####
output$Training <- renderPlot ({
  mydata <- dataplus()
  mydata <- mydata %>% filter(str_detect(IdLabel, "Bat")) %>% #Remove those visits made by a wand
    mutate(reinforce1value = ifelse(is.na(reinforce1value), 0, reinforce1value)) %>% 
    mutate(reward = ifelse(reinforce1value != 0, 1, 0)) %>% 
    filter(reward == 1) #Insert variable for reinforcement
    mydata <- mydata %>% 
    mutate(Exp_stage = ifelse(str_detect(outFuncLabel, "out1|out2"), "Training", "Main")) 
  
    Training <- mydata %>% filter(Exp_stage == "Training")
  
  if (dim(Training)[1] != 0) {
    Training <- subset(Training, select = c(2,3,4,5,9,12)) %>% 
      filter(str_detect(unitLabel, "Cond")) #Selecting the visits to the flowers
     Training <- Training %>% #Selecting the visits to the flowers
      mutate(Flower = as.numeric(str_extract(unitLabel, "[0-9]+")), 
             Phase = str_remove_all(outFuncLabel, c("out1|out2")))
     
    Training <- Training %>% filter(Phase != "NA") %>%
      mutate(Flower = factor(Flower, levels = c(1,2,3,4,5,6,7,8,9,10,11,12)), 
             Phase = factor(Phase, levels = c("Initial", "Forced1", "Free1", "Forced2", "Free2"))) %>%
      group_by(IdLabel, Phase, Flower,reinforce1value) %>% 
      summarise(unitLabel = n()) %>%
      mutate(reinforce1value = round(reinforce1value*input$pumpconv))
    ggplot(Training) + 
      geom_bar(
        aes(x = Phase, y = unitLabel, fill = Flower, group = Flower), 
        stat='identity', position = 'dodge'
      ) +
      geom_text(
        aes(x = Phase, y = unitLabel, label = unitLabel, group = Flower),
        position = position_dodge(width = 1),
        vjust = -0.5, size = 2
      ) +
      geom_text(
        aes(x = Phase, y = unitLabel, label = reinforce1value, group = Flower, fontface = "bold"), 
        position = position_dodge(width = 1), 
        vjust = -1.5, size = 3
      ) + 
      facet_wrap(IdLabel~., scales = "free_y") +
      theme_bw() +
      xlab("Experiment Phase") + 
      ylab("Visits") +
      theme(axis.text.x = element_text(angle = 30, vjust = 1.25, hjust = 1)) + 
      coord_cartesian(ylim = c(0, 375), clip = "on")
    
  } else {
    text = paste("There is no animal in the training phase")
    ggplot() + 
      annotate("text", x = 4, y = 25, size=8, label = text) + 
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
  }
  
})


output$trackers <- renderPlot ({  
  mydata <- dataplus()
  mydata <- mydata %>% filter(str_detect(IdLabel, "Bat")) %>% #Remove those visits made by a wand
    mutate(reinforce1value = ifelse(is.na(reinforce1value), 0, reinforce1value)) %>% 
    mutate(reward = ifelse(reinforce1value != 0, 1, 0)) %>% 
    filter(reward == 1) #Insert variable for reinforcement
  #Insert variable for reinforcement
  mydata <- mydata %>% 
    mutate(Exp_stage = ifelse(str_detect(outFuncLabel, "out1|out2"), "Training", "Main"))
  
  Main <- mydata %>% filter(Exp_stage == "Main")
  
  if (dim(Main)[1] != 0) {
    Main <- Main %>% group_by(IdLabel) %>%  
      mutate(sinewave = as.numeric(ifelse(outFuncLabel == "fixRewOut", SystemMsg, reinforce1value)),
             vol = 0.324 * sinewave, 
             timediff = (as.numeric(difftime(DateTime, min(DateTime), units = "secs")))*1000)
    Main <- Main %>% mutate(timediff = timediff/3600000)
    Main <- Main %>% mutate(Amplitude = 35, Disp = 41)
    Main <- Main %>% 
      mutate(side = ifelse(outFuncLabel == "sineRewOut", (Amplitude + Disp)*0.324, (Disp-Amplitude)*0.324))
    
    #finding the non-tracking animals
    Tracking <- Main %>% group_by(IdLabel, outFuncLabel) %>% summarise(unitLabel = n())
    Bats_s<- Tracking %>% select(IdLabel) %>% distinct(IdLabel) %>% mutate(outFuncLabel = c("sineRewOut"))
    Bats_f<- Tracking %>% select(IdLabel) %>% distinct(IdLabel) %>% mutate(outFuncLabel = c("fixRewOut"))
    Bats <- bind_rows(Bats_s, Bats_f)
    Tracking <- left_join(Bats, Tracking, by = c("IdLabel", "outFuncLabel")) %>% 
      mutate(Tracking = ifelse(unitLabel < 2 | is.na(unitLabel), "non-tracker", "tracker"))
    Bats <- Bats %>% select(IdLabel) %>% distinct() 
    Nontrackers <- Tracking %>%  filter(Tracking == "non-tracker") %>% 
      select(IdLabel, Tracking)
    Bats <- left_join(Bats, Nontrackers, by = "IdLabel") %>% 
      mutate(Tracking = ifelse(is.na(Tracking), "tracker", "non-tracker"))
    Main <- left_join(Main, Bats, by = "IdLabel") 

    Main %>%
      group_by(IdLabel) %>%
      filter(Tracking == "tracker") %>%
      mutate(smooth = as.numeric(tsSmooth(StructTS(side, type = "level")))) %>%
      ggplot(aes(timediff)) +
      geom_line(aes(y = vol)) +
      geom_point(aes(y = side), colour = "red", alpha = 0.05, size = 1) +
      scale_x_continuous(breaks = seq(0,12,1)) +
      ggtitle("Tracking bats in the main experimental phase") +
      xlab("Hour") +
      ylab("Volume output of the variable option [microL]") +
      facet_grid(IdLabel ~., scales = "free_y") +
      theme_classic() +
      geom_line(aes(y = smooth), color = "cornflowerblue") +
      geom_hline(yintercept = 2, linetype = 2)
    
} else {
    text = paste("There is no tracking animal in the main experimental phase")
    ggplot() + 
      annotate("text", x = 4, y = 25, size = 7, label = text) + 
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
  }
  
})

output$nontrackers <- renderPlot ({  
  mydata <- dataplus()
  mydata <- mydata %>% filter(str_detect(IdLabel, "Bat")) %>% #Remove those visits made by a wand
    mutate(reinforce1value = ifelse(is.na(reinforce1value), 0, reinforce1value)) %>% 
    mutate(reward = ifelse(reinforce1value != 0, 1, 0)) %>% 
    filter(reward == 1) #Insert variable for reinforcement
  #Insert variable for reinforcement
  mydata <- mydata %>% 
    mutate(Exp_stage = ifelse(str_detect(outFuncLabel, "out1|out2"), "Training", "Main"))
  
  Main <- mydata %>% filter(Exp_stage == "Main")
  
  if (dim(Main)[1] != 0) {
    Main <- Main %>% group_by(IdLabel) %>%  
      mutate(sinewave = as.numeric(ifelse(outFuncLabel == "fixRewOut", SystemMsg, reinforce1value)),
             vol = 0.324 * sinewave, 
             timediff = (as.numeric(difftime(DateTime, min(DateTime), units = "secs")))*1000)
    Main <- Main %>% mutate(timediff = timediff/3600000)
    
    Main <- Main %>% mutate(Amplitude = 35, Disp = 41)
    
    Main <- Main %>% 
      mutate(side = ifelse(outFuncLabel == "sineRewOut", (Amplitude + Disp)*0.324, (Disp-Amplitude)*0.324))
    
    #finding the non-tracking animals
    Tracking <- Main %>% group_by(IdLabel, outFuncLabel) %>% summarise(unitLabel = n())
    Bats_s<- Tracking %>% select(IdLabel) %>% distinct(IdLabel) %>% mutate(outFuncLabel = c("sineRewOut"))
    Bats_f<- Tracking %>% select(IdLabel) %>% distinct(IdLabel) %>% mutate(outFuncLabel = c("fixRewOut"))
    Bats <- bind_rows(Bats_s, Bats_f)
    Tracking <- left_join(Bats, Tracking, by = c("IdLabel", "outFuncLabel")) %>% 
      mutate(Tracking = ifelse(unitLabel < 2 | is.na(unitLabel), "non-tracker", "tracker"))
    Bats <- Bats %>% select(IdLabel) %>% distinct() 
    Nontrackers <- Tracking %>%  filter(Tracking == "non-tracker") %>% 
      select(IdLabel, Tracking)
    Bats <- left_join(Bats, Nontrackers, by = "IdLabel") %>% 
      mutate(Tracking = ifelse(is.na(Tracking), "tracker", "non-tracker"))
    
    Main <- left_join(Main, Bats, by = "IdLabel") %>% filter(Tracking == "non-tracker")
    
    if (dim(Main)[1] != 0) {
    
    Main %>%
      group_by(IdLabel) %>%
      filter(Tracking == "non-tracker") %>%
      #mutate(smooth = as.numeric(tsSmooth(StructTS(side, type = "level")))) %>%
      ggplot(aes(timediff)) +
      geom_line(aes(y = vol)) +
      geom_point(aes(y = side), colour = "red", alpha = 0.05, size = 1) +
      scale_x_continuous(breaks = seq(0,12,1)) +
      ggtitle("Non-tracking bats in the main experimental phase") +
      xlab("Hour") +
      ylab("Volume output of the variable option [microL]") +
      facet_grid(IdLabel ~., scales = "free_y") +
      theme_classic() +
      #geom_line(aes(y = smooth), color = "cornflowerblue") +
      geom_hline(yintercept = 2, linetype = 2)
      
    } else {
      text = paste("There is no non-tracking animal in the experimental phase")
      ggplot() + 
        annotate("text", x = 4, y = 25, size = 7, label = text) + 
        theme_bw() +
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank())
    }
    
  } else {
    text = paste("There is no non-tracking animal in the experimental phase")
    ggplot() + 
      annotate("text", x = 4, y = 25, size = 7, label = text) + 
      theme_bw() +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank())
  }
  
})

}

shinyApp(ui = ui, server = server)