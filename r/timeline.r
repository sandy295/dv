library(scales)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(knitr)
library(timevis)
Merkel <- data.frame(
  Year = c(rep(c(2018), times =12), rep(c(2019), times =2)), 
  Months = c(1,2,2,3,6,9,9,10,11,11,12,12,1,3), 
  Days = c(1,2,15,2,2,8,29,20,10,27,1,23,15,10),
  Milestones = c("saasddsf", "PET-CT (No evidence of metastatic disease)", "WLE and SLNBx", "PET-CT (No evidence of disease)", "PET-CT (No evidence of disease)", "PET-CT (Concerning for Recurrence)", "Cycle 1", "Cycle 2", "Cycle 3","PET-CT (Partial Response)","Cycle 4", "Cycle 5",  "Cycle 6","PET-CT (Complete Response)"), 
  Event_type= c("Biopsy", "Imaging", "Surgery", "Imaging", "Imaging", "Imaging", "Immunotherapy", "Immunotherapy","Immunotherapy","Imaging","Immunotherapy", "Immunotherapy", "Immunotherapy", "Imaging")) #The data set was created with the year, month and day in separate columns. Let's add the complete date column now

Merkel$date <- with(Merkel, ymd(sprintf('%04d%02d%02d', Merkel$Year, Merkel$Months, Merkel$Days))) 
Merkel <- Merkel[with(Merkel, order(date)), ]
Event_type_levels <- c("Biopsy", "Surgery", "Imaging", "Immunotherapy") 
Event_type_colors <- c("#C00000", "#FFC000",  "#00B050", "#0070C0" ) 
Merkel$Event_type <- factor(Merkel$Event_type, levels= Event_type_levels, ordered=TRUE)
positions <- c(0.5, -0.5, 1.0, -1.0, 1.25, -1.25, 1.5, -1.5) 
directions <- c(1, -1) 
line_pos <- data.frame(
  "date"=unique(Merkel$date),
  "position"=rep(positions, length.out=length(unique(Merkel$date))),
  "direction"=rep(directions, length.out=length(unique(Merkel$date))))
# Create columns with the specified positions and directions for each milestone event
Merkel <- merge(x=Merkel, y=line_pos, by="date", all = TRUE) 
kable(head(Merkel))
month_buffer <- 1 

month_date_range <- seq(min(Merkel$date) - months(month_buffer), max(Merkel$date) + months(month_buffer), by='month')
month_format <- format(month_date_range, '%b') 
month_df <- data.frame(month_date_range, month_format)


year_date_range <- seq(min(Merkel$date) - months(month_buffer), max(Merkel$date) + months(month_buffer), by='year')

year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="year"),
    floor_date(year_date_range, unit="year")),  
  origin = "1970-01-01") 
year_format <- format(year_date_range, '%Y') 
year_df <- data.frame(year_date_range, year_format)
timeline_plot<-ggplot(Merkel,aes(x=date,y= position, col=Event_type, label=Merkel$Milestones)) 
timeline_plot<-timeline_plot+labs(col="Milestones") 
timeline_plot
timeline_plot<-timeline_plot+scale_color_manual(values=Event_type_colors, labels=Event_type_levels, drop = FALSE) 
timeline_plot<-timeline_plot+theme_classic() 

timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                                        color = "black", size=0.3)

timeline_plot<-timeline_plot+geom_segment(data=Merkel, aes(y=Merkel$position,yend=0,xend=Merkel$date), color='black', size=0.2) 


# Now let's plot the scatter points at the tips of the vertical lines and date
timeline_plot<-timeline_plot+geom_point(aes(y=Merkel$position), size=3) 

# Let's remove the axis since this is a horizontal timeline and postion the legend to the bottom
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "bottom"
) 
timeline_plot
timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.15,label=month_format),size=3.5,vjust=0.5, color='black', angle=90) 
timeline_plot<-timeline_plot+geom_text(data=year_df, aes(x=year_date_range,y=-0.25,label=year_format, fontface="bold"),size=3.5, color='black') 
print(timeline_plot)
text_offset <- 0.2 
absolute_value<-(abs(Merkel$position)) 
text_position<- absolute_value + text_offset
Merkel$text_position<- text_position * Merkel$direction 
kable(head(Merkel))

timeline_plot<-timeline_plot+geom_text(aes(y=Merkel$text_position,label=Merkel$Milestones),size=3.5, vjust=0.6)

print(timeline_plot)