#define libraries
library(tidyverse)
library(ggthemes)
library(hrbrthemes)


#define table of dates/events to create horizontal lines in graph
table <- data.frame("date" = as.Date(c("2020-03-21", "2020-03-27", "2020-05-11", "2020-06-01", "2020-07-04")),
                    "event" = c("entertainment venues \nclose", "lockdown", "unlimited exercise", "free to leave home", "lockdown eased"))


#colour libraries for the line graph - I went for Google colours! :) 
myColors = c("#4285F4", "#DB4437", "#F4B400", "#0F9D58")

#----------------------------------------------------------------------------------------
#defining graph function, this will return a graph with country/county mentioned 
#in arguments
get_graph <- function(data, country, region) {  
  
filtered <- data %>% 
              select(date,
              "Retail and Recreation" = 9,
              "Parks" = 11,
              "Residential" = 14,
             "Work" = 13) 

filtered$date <- as.Date(filtered$date)
    
melted <- filtered %>%
  gather(key = "variable", value = "value", -date)

#slice data to indicate bank holiday - will use later in ggplot
bank <- melted %>%
  filter(variable == "Parks") %>%
  slice(which(date == "2020-05-25"))

#slice data to indicate hottest day of the year - will use later in ggplot
hottest <- melted %>%
  filter(variable == "Parks") %>%
  slice(which(date == "2020-07-31"))

#create dynamic chart title according to arguments passed
chart_title <- substitute(paste("Tracking movement behaviour in ", region,", ",country))

#call ggplot 
graph <- ggplot(melted, aes(x=date, y=value), color = variable) + 
  geom_line(aes(color = variable), size=1, alpha=0.8) + 
  labs(title = chart_title, 
       caption = "Data Source: Google Covid-19 Community Mobility Reports") + 
       ylab("Percentage change") + 
  #adding dashed lines for key dates
  geom_vline(data = table, mapping=aes(xintercept = date), linetype = "dashed", color = "lightgrey") + 
  geom_text(data = table, mapping=aes(x = date, y=100, label=event), size=4,angle=90, vjust=-0.4, hjust=0, color = "grey") + 
  
  #here is the code to highlight peaks 
  geom_text(data = bank, mapping = aes(x=date, y=value, label = "Spring Bank Holiday"), hjust = 1.2) + 
  geom_point(data = bank, mapping = aes(x=date, y=value), shape = 21, size=10, fill = "transparent")+
  geom_text(data = hottest, mapping = aes(x=date, y=value, label = "Hottest day of the year"), hjust = 1.2) + 
  geom_point(data = hottest, mapping = aes(x=date, y=value), shape = 21, size=10, fill = "transparent")+
  scale_color_manual(values = myColors) +  
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), text = element_text())

return(graph)

}

#----------------------------------------------------------------------

#read in data 
raw_data <- read.csv("Global_Mobility_Report.csv")

#------example use---------- 
  
#subset for WYorks
WYorks <- raw_data %>% filter(country_region == "United Kingdom" & 
                                sub_region_1 == "West Yorkshire")
#return graph 
get_graph(WYorks, "UK", "West Yorkshire")



#subset for WYorks
Cumbria <- raw_data %>% filter(country_region == "United Kingdom" & 
                                sub_region_1 == "Cumbria")
#return graph
get_graph(Cumbria, "UK", "Cumbria")


#Subset for London
london <- raw_data %>% filter(country_region == "United Kingdom" & 
                                 sub_region_1 == "Greater London")
#return graph
get_graph(london, "UK", "Greater London")


#Dorset
dorset <- raw_data %>% filter(country_region == "United Kingdom" & 
                                 sub_region_1 == "Dorset")
#return graph
get_graph(dorset, "UK", "Dorset")


