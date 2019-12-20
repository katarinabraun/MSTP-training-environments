#use dat from seminar_data_cleaning.r
require(ggplot2)
require(ggthemes)

#for shading
df_poly <- data.frame(
  PercentWomenPresent=c(-Inf, Inf, Inf),
  PercentWomenQuestions=c(-Inf, Inf, -Inf),
  speaker = c("male", "male", "male")
)


#Ratios
#Interaction between Attendee Gender and Speaker Gender 
IP<-data.frame(attendee = factor(c("male", "male", "female", "female")))
IP$speaker <- c("female", "male", "female", "male")
IP$P1orMoreQ <- c(39.58, 24.8, 24.72, 12.22)

## 4B ##
pI_notext <- ggplot(data = IP, aes(x = attendee, y = P1orMoreQ, group = speaker, color = speaker, shape = speaker), mgpg, aes(cty,hwy)) + 
  geom_point(size = 3) + geom_line(size = 1) + 
  ggtitle("") +
  ylim(0, 42) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  xlab("\nStudent attendee gender") + ylab("Proportion who asked least one question\n") +
  scale_color_manual(labels=c("female","male"), values = c("darkgrey", "black")) +
  scale_shape_manual(name = "Speaker Gender", labels=c("female","male"), values = c(19, 15)) + 
  theme_bw() + labs(color = "Speaker Gender") + theme(text=element_text(family="Helvetica", size=10)) + 
  theme(title = element_text(size = 16), legend.title = element_text(size = 12), legend.position = c(0.15, 0.875), 
        #legend.background = element_rect(color = "black", fill = "white", size = 1),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        legend.text=element_text(size=12), axis.text=element_text(size=12)); pI_notext
ggsave(filename = "../figures/figure4B_speaker_question-asker_interaction.png", width = 7, height = 7, plot = pI_notext)             

## 4A ##
f1 <- ggplot(dat, aes(x = PercentWomenPresent, y = PercentQuestionsFromWomen, colour = as.factor(speaker), shape = as.factor(speaker))) + 
  geom_point(size = 3) + geom_abline(slope=1, intercept=0, size = 1.6) + 
  xlim(c(0, 50)) + ylim(c(0, 50)) +
  ggtitle("") + xlab("\nPercent audience female") + 
  ylab("Percent question-askers female\n") + labs(color = "Speaker Gender") + 
  scale_color_manual(name = "Speaker Gender", labels=c("female","male"), values = c("darkgrey", "black")) + 
  scale_shape_manual(name = "Speaker Gender", labels=c("female","male"), values = c(19, 15)) +
  theme_bw() + theme(text=element_text(family="Helvetica", size=12)) + 
  theme(title = element_text(size = 16), legend.title = element_text(size = 12), legend.position = c(0.15, 0.875),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        legend.text=element_text(size=12)); f1 
#without text
f3 <- f1 + geom_polygon(data=df_poly, aes(PercentWomenPresent, PercentWomenQuestions), fill="grey", colour = "black", alpha=0.2); f3
ggsave(filename = "../figures/figure4A_speakers_gender.png", width = 7, height = 7, plot = f3)             

