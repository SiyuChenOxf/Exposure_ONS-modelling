saveRDS(styled1London,"styled1London.rds")
saveRDS(styled1London2,"styled1London2.rds")
saveRDS(styled1London3,"styled1London3.rds")
saveRDS(styled1London4,"styled1London4.rds")
saveRDS(styled1London5,"styled1London5.rds")
saveRDS(styled1London6,"styled1London6.rds")

saveRDS(styled1NorthWest,"styled1NorthWest.rds")
saveRDS(styled1NorthWest2,"styled1NorthWest2.rds")
saveRDS(styled1NorthWest3,"styled1NorthWest3.rds")
saveRDS(styled1NorthWest4,"styled1NorthWest4.rds")
saveRDS(styled1NorthWest5,"styled1NorthWest5.rds")
saveRDS(styled1NorthWest6,"styled1NorthWest6.rds")

saveRDS(styled1SouthWest,"styled1SouthWest.rds")
saveRDS(styled1SouthWest2,"styled1SouthWest2.rds")
saveRDS(styled1SouthWest3,"styled1SouthWest3.rds")
saveRDS(styled1SouthWest4,"styled1SouthWest4.rds")
saveRDS(styled1SouthWest5,"styled1SouthWest5.rds")
saveRDS(styled1SouthWest6,"styled1SouthWest6.rds")

saveRDS(styled1SouthEast,"styled1SouthEast.rds")
saveRDS(styled1SouthEast2,"styled1SouthEast2.rds")
saveRDS(styled1SouthEast3,"styled1SouthEast3.rds")
saveRDS(styled1SouthEast4,"styled1SouthEast4.rds")
saveRDS(styled1SouthEast5,"styled1SouthEast5.rds")
saveRDS(styled1SouthEast6,"styled1SouthEast6.rds")

saveRDS(styled1NorthEastYorkshireHumber,"styled1NorthEastYorkshireHumber.rds")
saveRDS(styled1NorthEastYorkshireHumber2,"styled1NorthEastYorkshireHumber2.rds")
saveRDS(styled1NorthEastYorkshireHumber3,"styled1NorthEastYorkshireHumber3.rds")
saveRDS(styled1NorthEastYorkshireHumber4,"styled1NorthEastYorkshireHumber4.rds")
saveRDS(styled1NorthEastYorkshireHumber5,"styled1NorthEastYorkshireHumber5.rds")
saveRDS(styled1NorthEastYorkshireHumber6,"styled1NorthEastYorkshireHumber6.rds")

saveRDS(styled1Midlands,"styled1Midlands.rds")
saveRDS(styled1Midlands2,"styled1Midlands2.rds")
saveRDS(styled1Midlands3,"styled1Midlands3.rds")
saveRDS(styled1Midlands4,"styled1Midlands4.rds")
saveRDS(styled1Midlands5,"styled1Midlands5.rds")
saveRDS(styled1Midlands6,"styled1Midlands6.rds")

saveRDS(styled1EastofEngland,"styled1EastofEngland.rds")
saveRDS(styled1EastofEngland2,"styled1EastofEngland2.rds")
saveRDS(styled1EastofEngland3,"styled1EastofEngland3.rds")
saveRDS(styled1EastofEngland4,"styled1EastofEngland4.rds")
saveRDS(styled1EastofEngland5,"styled1EastofEngland5.rds")
saveRDS(styled1EastofEngland6,"styled1EastofEngland6.rds")


styled1London2<-readRDS("styled1London2.rds")
styled1London3<-readRDS("styled1London3.rds")
styled1London4<-readRDS("styled1London4.rds")
styled1London5<-readRDS("styled1London5.rds")
styled1London6<-readRDS("styled1London6.rds")
styled1London<-readRDS("styled1London.rds")

styled1NorthWest2<-readRDS("styled1NorthWest2.rds")
styled1NorthWest3<-readRDS("styled1NorthWest3.rds")
styled1NorthWest4<-readRDS("styled1NorthWest4.rds")
styled1NorthWest5<-readRDS("styled1NorthWest5.rds")
styled1NorthWest6<-readRDS("styled1NorthWest6.rds")
styled1NorthWest<-readRDS("styled1NorthWest.rds")

grid.arrange(styled1London2,styled1London3,styled1London4,styled1London5,styled1London6,styled1London, 
             styled1SouthEast2,styled1SouthEast3,styled1SouthEast4,styled1SouthEast5,styled1SouthEast6,styled1SouthEast,ncol=6)

grid.arrange(styled1NorthWest2,styled1NorthWest3,styled1NorthWest4,styled1NorthWest5,styled1NorthWest6,styled1NorthWest,
             styled1SouthWest2,styled1SouthWest3,styled1SouthWest4,styled1SouthWest5,styled1SouthWest6,styled1SouthWest,
             ncol=6)

grid.arrange(styled1NorthEastYorkshireHumber2,styled1NorthEastYorkshireHumber3,styled1NorthEastYorkshireHumber4,styled1NorthEastYorkshireHumber5,styled1NorthEastYorkshireHumber6,styled1NorthEastYorkshireHumber,
             styled1EastofEngland2,styled1EastofEngland3,styled1EastofEngland4,styled1EastofEngland5,styled1EastofEngland6,styled1EastofEngland,
             ncol=6)

styled1NorthWest2<-styled1NorthWest2 + 
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  ylab(" Percentage (%)  ") +
  xlab("  ")+
  theme(
    axis.text.x = element_blank(),
    text = element_text(size=font_size),
    plot.title = element_text(face = "bold", size = font_size_title,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = "none",
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=12),
    axis.title.x = element_text(angle = 0, vjust = -0.005, hjust = 1,size=xlab_font_size),
    panel.grid.major = element_line(colour = "grey50", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t=0, r=right_margin, b=0, l=0, "cm")
  )
styled1NorthWest3<-styled1NorthWest3 + ylab("") +xlab("")+ theme(axis.text = element_blank(),legend.position = "none")
styled1NorthWest4<-styled1NorthWest4 + ylab("") +xlab("")+ theme(axis.text = element_blank(),legend.position = "none")
styled1NorthWest5<-styled1NorthWest5 + ylab("") +xlab("")+ theme(axis.text = element_blank(),legend.position = "none")
styled1NorthWest6<-styled1NorthWest6 + ylab("") +xlab("")+ theme(axis.text = element_blank(),legend.position = "none")
styled1NorthWest<-styled1NorthWest + ylab("") +xlab("")+ theme(axis.text = element_blank(),legend.position = "none")



styled1SouthWest2<-styled1SouthWest2 + 
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  ylab(" Percentage (%)  ") +
  xlab("  ")+
  theme(
    text = element_text(size=font_size),
    plot.title = element_text(face = "bold", size = font_size_title,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = "none",
    legend.title = element_blank(),
    axis.text.x =  element_text(size=xlab_font_size),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=xlab_font_size),
    axis.title.x = element_text(angle = 0, vjust = -0.005, hjust = 1,size=xlab_font_size),
    panel.grid.major = element_line(colour = "grey50", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t=0, r=right_margin, b=0, l=0, "cm")
  )
styled1SouthWest3<-styled1SouthWest3 + ylab("") +xlab("")+ theme(legend.position = "none")
styled1SouthWest4<-styled1SouthWest4 + ylab("") +xlab("")+ theme(legend.position = "none")
styled1SouthWest5<-styled1SouthWest5 + ylab("") +xlab("")+ theme(legend.position = "none")
styled1SouthWest6<-styled1SouthWest6 + ylab("") +xlab("")+ theme(legend.position = "none")
styled1SouthWest<-styled1SouthWest + ylab("") +xlab("")+ theme(legend.position = "none")


styled1SouthEast2<-styled1SouthEast2 + 
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  ylab(" Percentage (%)  ") +
  xlab("  ")+
  theme(
    text = element_text(size=font_size),
    plot.title = element_text(face = "bold", size = font_size_title,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = "none",
    legend.title = element_blank(),
    axis.text.x =  element_text(size=xlab_font_size),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=xlab_font_size),
    axis.title.x = element_text(angle = 0, vjust = -0.005, hjust = 1,size=xlab_font_size),
    panel.grid.major = element_line(colour = "grey50", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t=0, r=right_margin, b=0, l=0, "cm")
  )
styled1SouthEast3<-styled1SouthEast3 + ylab("") +xlab("")+ theme(axis.text.y = element_blank())
styled1SouthEast4<-styled1SouthEast4 + ylab("") +xlab("")+ theme(axis.text.y = element_blank())
styled1SouthEast5<-styled1SouthEast5 + ylab("") +xlab("")+ theme(axis.text.y = element_blank())
styled1SouthEast6<-styled1SouthEast6 + ylab("") +xlab("")+ theme(axis.text.y = element_blank())
styled1SouthEast<-styled1SouthEast + ylab("") +xlab("")+ theme(axis.text.y = element_blank())

styled1NorthEastYorkshireHumber2<-styled1NorthEastYorkshireHumber2 + 
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  ylab(" Percentage (%)  ") +
  xlab("  ")+
  theme(
    text = element_text(size=font_size),
    plot.title = element_text(face = "bold", size = font_size_title,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = "none",
    legend.title = element_blank(),
    axis.text.x =  element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=xlab_font_size),
    axis.title.x = element_text(angle = 0, vjust = -0.005, hjust = 1,size=xlab_font_size),
    panel.grid.major = element_line(colour = "grey50", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t=0, r=right_margin, b=0, l=0, "cm")
  )
styled1NorthEastYorkshireHumber3<-styled1NorthEastYorkshireHumber3 + ylab("") +xlab("")+ theme(axis.text = element_blank())
styled1NorthEastYorkshireHumber4<-styled1NorthEastYorkshireHumber4 + ylab("") +xlab("")+ theme(axis.text = element_blank())
styled1NorthEastYorkshireHumber5<-styled1NorthEastYorkshireHumber5 + ylab("") +xlab("")+ theme(axis.text = element_blank())
styled1NorthEastYorkshireHumber6<-styled1NorthEastYorkshireHumber6 + ylab("") +xlab("")+ theme(axis.text = element_blank())
styled1NorthEastYorkshireHumber<-styled1NorthEastYorkshireHumber + ylab("") +xlab("")+ theme(axis.text = element_blank())

styled1EastofEngland2<-styled1EastofEngland2 + 
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  ylab(" Percentage (%)  ") +
  xlab("  ")+
  theme(
    text = element_text(size=font_size),
    plot.title = element_text(face = "bold", size = font_size_title,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = c(0.2,0.95),
    legend.title = element_blank(),
    axis.text.x =  element_text(size=xlab_font_size),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=xlab_font_size),
    axis.title.x = element_text(angle = 0, vjust = -0.005, hjust = 1,size=xlab_font_size),
    panel.grid.major = element_line(colour = "grey50", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t=0, r=right_margin, b=0, l=0, "cm")
  )
styled1EastofEngland3<-styled1EastofEngland3 + ylab("") +xlab("")+ theme(axis.text.y = element_blank())
styled1EastofEngland4<-styled1EastofEngland4 + ylab("") +xlab("")+ theme(axis.text.y = element_blank())
styled1EastofEngland5<-styled1EastofEngland5 + ylab("") +xlab("")+ theme(axis.text.y = element_blank())
styled1EastofEngland6<-styled1EastofEngland6 + ylab("") +xlab("")+ theme(axis.text.y = element_blank())
styled1EastofEngland<-styled1EastofEngland + ylab("") +xlab("")+ theme(axis.text.y = element_blank())


styled1NorthWest2<-styled1NorthWest2 + 
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  ylab(" Percentage (%)  ") +
  xlab("  ")+
  theme(
    text = element_text(size=font_size),
    plot.title = element_text(face = "bold", size = font_size_title,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = "none",
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=12),
    axis.title.x = element_text(angle = 0, vjust = -0.005, hjust = 1,size=xlab_font_size),
    panel.grid.major = element_line(colour = "grey50", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t=0, r=right_margin, b=0, l=0, "cm")
  )
styled1NorthWest3<-styled1NorthWest3 + ylab("") +xlab("")+ theme(axis.text = element_blank(),legend.position = "none")
styled1NorthWest4<-styled1NorthWest4 + ylab("") +xlab("")+ theme(axis.text = element_blank(),legend.position = "none")
styled1NorthWest5<-styled1NorthWest5 + ylab("") +xlab("")+ theme(axis.text = element_blank(),legend.position = "none")
styled1NorthWest6<-styled1NorthWest6 + ylab("") +xlab("")+ theme(axis.text = element_blank(),legend.position = "none")
styled1NorthWest<-styled1NorthWest + ylab("") +xlab("")+ theme(axis.text = element_blank(),legend.position = "none")


styled1London2<-styled1London2 + 
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  ylab(" Percentage (%)  ") +
  xlab("  ")+
  theme(
    text = element_text(size=font_size),
    plot.title = element_text(face = "bold", size = font_size_title,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = "none",
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.text.x = element_blank(),
    axis.title.x = element_text(angle = 0, vjust = -0.005, hjust = 1,size=xlab_font_size),
    panel.grid.major = element_line(colour = "grey50", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t=0, r=right_margin, b=0, l=0, "cm")
  )
styled1London3<-styled1London3 + ylab("") +xlab("")+ theme(axis.text = element_blank(),legend.position = "none")
styled1London4<-styled1London4 + ylab("") +xlab("")+ theme(axis.text = element_blank(),legend.position = "none")
styled1London5<-styled1London5 + ylab("") +xlab("")+ theme(axis.text = element_blank(),legend.position = "none")
styled1London6<-styled1London6 + ylab("") +xlab("")+ theme(axis.text = element_blank(),legend.position = "none")
styled1London<-styled1London + ylab("") +xlab("")+ theme(axis.text = element_blank(),legend.position = "none")


