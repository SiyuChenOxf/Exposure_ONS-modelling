library(ggplot2)
library(readxl)
library(RColorBrewer)
library(gridExtra)
library(scales)

## common values for plotting
colors_Dark<-brewer.pal(7,"Dark2")
colors_Spectral<-brewer.pal(7,"Spectral")
xlab_font_size=20
legend_font_size=15
font_size = 20
font_size_title = 20
lwd = 0.5
pt_size = 0.4
right_margin=1

df<-read_excel("comparison_data.xlsx")

df_plot = data.frame(output = c(rep("Exposure model", length(df$date)), rep("ONS infection survey", length(df$date))), 
                         t=c(as.Date(df$date),as.Date(df$date)), 
                         median = c(df$model_med,df$ons_med), 
                         lower = c(df$model_low,df$ons_low), 
                         upper = c(df$model_upp,df$ons_upp))

ggplot(df_plot, aes(x=t,y=median, group = output, colour = output)) +
  geom_line(size = 1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper,fill=output),alpha=0.2,colour=NA) +
  scale_x_date(breaks = as.Date(c("2020-01-01","2020-03-01", "2020-05-01", "2020-07-01","2020-09-01",  "2020-11-01")), labels=c("Jan","Mar", "May", "Jul","Sep","Nov"), limit = as.Date(c("2020-01-01","2020-11-07")))+
  scale_y_continuous(labels = comma)+
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  ylab(" Model predicted daily incidence  ") +
  xlab(" 2020 ")+
  theme(
    axis.text=element_text(size=xlab_font_size),
    plot.title = element_text(face = "bold", size = font_size_title,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = c(0.5,0.95),
    legend.title = element_blank(),
    legend.text=element_text(size=legend_font_size),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=xlab_font_size),
    axis.title.x = element_text(angle = 0, vjust = -0.005, hjust = 1,size=xlab_font_size),
    panel.grid.major = element_line(colour = "grey50", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t=0, r=right_margin, b=0, l=0, "cm")
  )
 
 
p1<-ggplot(df, aes(x=as.Date(date))) +
  geom_line(aes(y=model_med),size = 1) + 
  geom_line(aes(y=reported_cases),size=1,colour="blue")+
  geom_ribbon(aes(ymin=model_low, ymax=model_upp),alpha=0.2,colour=NA) +
  scale_x_date(breaks = as.Date(c("2020-01-01","2020-03-01", "2020-05-01", "2020-07-01","2020-09-01",  "2020-11-01")), labels=c("Jan","Mar", "May", "Jul","Sep","Nov"), limit = as.Date(c("2020-01-01","2020-11-07")))+
  scale_y_continuous(labels = comma)+
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  ylab(" Incidence vs Cases  ") +
  xlab(" 2020 ")+
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text=element_text(size=xlab_font_size),
    plot.title = element_text(face = "bold", size = font_size_title,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = c(0.5,0.95),
    legend.title = element_blank(),
    legend.text=element_text(size=legend_font_size),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=xlab_font_size),
    # axis.title.x = element_text(angle = 0, vjust = -0.005, hjust = 1,size=xlab_font_size),
    panel.grid.major = element_line(colour = "grey50", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t=0, r=right_margin, b=0, l=0, "cm")
  )
df<-read_excel("comparison_data2.xlsx")

df_plot = data.frame( 
  t=as.Date(df$date), 
  median = df$rate_med, 
  lower = df$rate_low, 
  upper = df$rate_upp)

p2<-ggplot(df_plot, aes(x=t,y=median)) +
  geom_line(size = 1,color="#69b3a2") + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.2,fill="#69b3a2") +
  scale_x_date(breaks = as.Date(c("2020-01-01","2020-03-01", "2020-05-01", "2020-07-01","2020-09-01",  "2020-11-01")), labels=c("Jan","Mar", "May", "Jul","Sep","Nov"), limit = as.Date(c("2020-01-01","2020-11-07")))+
  scale_y_continuous(labels = comma)+
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  ylab(" Estimated Case Reporting Rate  ") +
  xlab(" 2020 ")+
  theme(
    axis.text=element_text(size=xlab_font_size),
    plot.title = element_text(face = "bold", size = font_size_title,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = c(0.5,0.95),
    legend.title = element_blank(),
    legend.text=element_text(size=legend_font_size),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=xlab_font_size),
    axis.title.x = element_text(angle = 0, vjust = -0.005, hjust = 1,size=xlab_font_size),
    panel.grid.major = element_line(colour = "grey50", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t=0, r=right_margin, b=0, l=0, "cm")
  )

((p1 / p2 + plot_layout(guides = 'auto')) ) + plot_layout(guides = 'collect')
