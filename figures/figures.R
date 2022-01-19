library(tidyverse)
library(extrafont)
library(patchwork)

###Data
end_survey <- read_csv("end_survey.csv")
end_survey_reshape <- gather(end_survey, "question", "answer", "Learn", "Recognize", "Interpret", "Create", "FutureUse", -"Feedback")

confidence <- read_csv("confidence_over_modules.csv")
confidence_reshape <- gather(confidence, "module", "answer", "Module 1", "Module 2", "End of Modules", -"Feedback")

chart_familiarity <- read_csv("chart_familiarity.csv")
familiarity_reshape <- gather(chart_familiarity, "Chart", "Answer", "Line", "Bar", "Stacked Bar", "Percent Stacked Bar", "Pie", "Histogram","Scatterplot","Bubble","Stacked Area","Choropleth","Treemap","Area", -"Feedback")

apply_chart_scores <- read_csv("3_apply_scores.csv")
apply_chart_reshape <- gather(apply_chart_scores, "Feedback", "Grade", "CIF", "NF")

duration <- read_csv("duration.csv")
duration$minutes <- duration$`Total Time`/60
#duration_reshape <- gather(duration, "module", "time", "Mod1", "Mod2a", "Mod2b", -"Feedback")

view(duration)

#Grading
remember_grades <- read_csv("1_remember_grades.csv")
remember_reshape <- gather(remember_grades, "Feedback", "Grade", "CIF", "NF")

understand_grades <- read_csv("2_understand_grades.csv")
understand_reshape <- gather(understand_grades, "Feedback", "Grade", "CIF", "NF")

apply_grades <- read_csv("3_apply_grades.csv")
apply_reshape <- gather(apply_grades, "Feedback", "Grade", "CIF", "NF")

analyze_grades <- read_csv("4_analyze_grades.csv")
analyze_reshape <- gather(analyze_grades, "Feedback", "Grade", "CIF", "NF")

critique_grades <- read_csv("5_critique_grades.csv")
critique_reshape <- gather(critique_grades, "Feedback", "Grade", "CIF", "NF")

create_grades <- read_csv("6_create_grades.csv")
create_reshape <- gather(create_grades, "Feedback", "Grade", "CIF", "NF")

###Figures in paper
#Fig. 12: Apply Module Scores
plot <- 
  ggplot(apply_chart_reshape, aes(y=Feedback, x=Grade)) +
  geom_boxplot(fill=c("#6F54A7","#6FAC36")) +
  scale_y_discrete(labels=c("CIF","NF")) +
  theme_minimal()+
  theme(
    text=element_text(color="black", family="Roboto"),
    plot.title = element_text(face="bold",size=22),
    plot.subtitle=element_text(size=16),
    axis.text=element_text(color="black",size=16),
    axis.title.x=element_blank()
  ) +
  labs (title="Apply Module Scores") +
  xlab("") + ylab("")
print(plot)
ggsave("apply_grades.png")

#Fig. 18: Confidence with Parallel Coordinates Charts After Completion
plot <- end_survey_reshape %>% filter(question !="Learn") %>% 
  mutate(question = fct_relevel(question, "Recognize", "Interpret", "Create", "FutureUse")) %>% 
  ggplot(aes(x=question, y=answer, fill=Feedback)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(1:7), labels=c("1 (Low)", 2:6, "7 (High)")) +
  scale_fill_manual(values=c("#6F54A7","#6FAC36")) +
  theme_minimal()+
  theme(
    text=element_text(color="black", family="Roboto"),
    plot.title = element_text(face="bold",size=22),
    axis.text=element_text(color="black",size=16),
    axis.title=element_text(color="black",face="bold",size=16),
    legend.title=element_text(color="black",size=16),
    legend.text=element_text(color="black",size=16)
  ) +
  labs(title="Confidence with Parallel Coordinates after Completion", fill="Feedback") +
  xlab("") + ylab("Confidence")
print(plot)
ggsave("end_survey_boxplot.png", width=9, height=6, unit="in")

#Fig. 19: Confidence in Recognizing Parallel Coordinates Charts
plot <- 
  ggplot(confidence_reshape, aes(x=module, y=answer, fill=Feedback)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(1:7), labels=c("1 (Low)", 2:6, "7 (High)")) +
  scale_x_discrete(breaks=c("Module 1","Module 2","End of Modules"),labels=c("After Module 1", "After Module 2","End of Modules")) +
  scale_fill_manual(values=c("#6F54A7","#6FAC36")) +
  theme_minimal()+
  theme(
    text=element_text(color="black", family="Roboto"),
    plot.title = element_text(face="bold",size=22),
    axis.text=element_text(color="black",size=16),
    axis.title=element_text(color="black",face="bold",size=16),
    legend.title=element_text(color="black",size=16),
    legend.text=element_text(color="black",size=16)
  ) +
  labs(title="Confidence in Recognizing Parallel Coordinates Charts", fill="Feedback") +
  xlab("") + ylab("Confidence")
print(plot)
ggsave("confidence_boxplot.png", width=9, height=6, unit="in")

#Fig. 21: Summary Figure
#Remember
p1 <- 
  ggplot(remember_reshape, aes(y=Feedback, x=Grade)) +
  geom_boxplot(fill=c("#6F54A7","#6FAC36")) +
  theme_minimal()+
  xlim(c(0, 100)) +
  theme(
    text=element_text(color="black", family="Roboto"),
    plot.title = element_text(face="bold",size=22),
    plot.subtitle=element_text(size=16),
    axis.text.x=element_text(color="black",size=16),
    axis.text.y=element_blank(),
    axis.title.x=element_blank()
  ) +
  labs(title="Summary Outcomes", subtitle="Remember module") +
  xlab("") + ylab("")
print(p1)

#Understand
p2 <- 
  ggplot(understand_reshape, aes(y=Feedback, x=Grade)) +
  geom_boxplot(fill=c("#6F54A7","#6FAC36")) +
  theme_minimal() +
  xlim(c(0, 100)) +
  theme(
    text=element_text(color="black", family="Roboto"),
    plot.title = element_text(face="bold",size=22),
    plot.subtitle=element_text(size=16),
    axis.text.x=element_text(color="black",size=16),
    axis.text.y=element_blank(),
    axis.title.x=element_blank()
  ) +
  labs(subtitle="Understand Module") +
  xlab("") + ylab("")
print(p2)

#Apply
p3 <- 
  ggplot(apply_reshape, aes(y=Feedback, x=Grade)) +
  geom_boxplot(fill=c("#6F54A7","#6FAC36")) +
  theme_minimal() +
  xlim(c(0, 100)) +
  theme(
    text=element_text(color="black", family="Roboto"),
    plot.title = element_text(face="bold",size=22),
    plot.subtitle=element_text(size=16),
    axis.text.x=element_text(color="black",size=16),
    axis.text.y=element_blank(),
    axis.title.x=element_blank()
  ) +
  labs(subtitle="Apply Module") +
  xlab("") + ylab("")
print(p3)

view(apply_grades)
#Analyze
p4 <- 
  ggplot(analyze_reshape, aes(y=Feedback, x=Grade)) +
  geom_boxplot(fill=c("#6F54A7","#6FAC36")) +
  theme_minimal() +
  xlim(c(0, 100)) +
  theme(
    text=element_text(color="black", family="Roboto"),
    plot.title = element_text(face="bold",size=22),
    plot.subtitle=element_text(size=16),
    axis.text.x=element_text(color="black",size=16),
    axis.text.y=element_blank(),
    axis.title.x=element_blank()
  ) +
  labs(subtitle="Analyze Module") +
  xlab("") + ylab("")
print(p4)

#Critique
p5 <- 
  ggplot(critique_reshape, aes(y=Feedback, x=Grade)) +
  geom_boxplot(fill=c("#6F54A7","#6FAC36")) +
  theme_minimal() +
  xlim(c(0, 100)) +
  theme(
    text=element_text(color="black", family="Roboto"),
    plot.title = element_text(face="bold",size=22),
    plot.subtitle=element_text(size=16),
    axis.text.x=element_text(color="black",size=16),
    axis.text.y=element_blank(),
    axis.title.x=element_blank()
  ) +
  labs(subtitle="Critique Module") +
  xlab("") + ylab("")
print(p5)

#Create
p6 <- 
  ggplot(create_reshape, aes(y=Feedback, x=Grade)) +
  geom_boxplot(fill=c("#6F54A7","#6FAC36")) +
  theme_minimal() +
  xlim(c(0, 100)) +
  theme(
    text=element_text(color="black", family="Roboto"),
    plot.title = element_text(face="bold",size=22),
    plot.subtitle=element_text(size=16),
    axis.text.x=element_text(color="black",size=16),
    axis.text.y=element_blank(),
    axis.title.x=element_blank()
  ) +
  labs(subtitle="Create Module") +
  xlab("") + ylab("")
print(p6)

p1/p2/p3/p4/p5/p6
ggsave("summaryfigure.png", height=9, width=6, unit="in")

###Supplemental figures
#Familiarity with Chart Types, Sorted by Highest Mean Familiarity
plotCIF <- familiarity_reshape %>% filter(Feedback=="CIF") %>%
  ggplot(aes(x=reorder(Chart, Answer, mean), y=Answer)) +
  geom_boxplot(fill="#6F54A7") +
  coord_flip() +
  theme_minimal()+
  theme(
    text=element_text(color="black", family="Roboto"),
    plot.title = element_text(face="bold",size=22),
    plot.subtitle=element_text(size=16),
    axis.text=element_text(color="black",size=16),
    axis.text.x=element_blank(),
    axis.title.x=element_blank()
  ) +
  labs (title="Familiarity with Chart Types, Sorted by Highest Mean Familiarity", subtitle="CIF") +
  xlab("") + ylab("")
print(plotCIF)

plotNF <- familiarity_reshape %>% filter(Feedback=="NF") %>%
  ggplot(aes(x=reorder(Chart, Answer, mean), y=Answer)) +
  geom_boxplot(fill="#6FAC36") +
  coord_flip() +
  scale_y_continuous(breaks=c(1,2,3), labels=c("Not at all","Somewhat","Very")) +
  theme_minimal()+
  theme(
    text=element_text(color="black", family="Roboto"),
    plot.subtitle=element_text(size=16),
    axis.text=element_text(color="black",size=16),
    axis.title=element_text(color="black",size=16, face="bold")
  ) +
  labs (subtitle="NF") +
  xlab("") + ylab("Familiarity")
print(plotNF)

plotCIF/plotNF
ggsave("familiarityall_boxplot_mean.png",height=9, width=12, unit="in")

#Minutes spent on modules
plot <- duration %>% filter(`Total Time`!=168628) %>%
  ggplot(aes(x=minutes, y=Feedback)) +
  geom_boxplot(fill=c("#6F54A7","#6FAC36")) +
  scale_y_discrete(labels=c("CIF", "NF")) +
  scale_x_continuous(n.breaks=10) +
  theme_minimal()+
  theme(
    text=element_text(color="black", family="Roboto"),
    plot.title = element_text(face="bold",size=22),
    plot.subtitle=element_text(size=16),
    axis.text=element_text(color="black",size=16),
    axis.title.x=element_blank()
  ) +
  labs (title="Minutes Spent on Modules") +
  xlab("") + ylab("")
print(plot)
ggsave("duration_boxplot.png", width=10, height=6, unit="in")

#How Much Respondents Felt They Had Learned after Completion
plot <- end_survey_reshape %>% filter(question=="Learn") %>%
  ggplot(aes(x=question, y=answer, fill=Feedback)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(1:5), labels=c("1 (Learned little)", 2:4, "5 (Learned a lot)")) +
  scale_fill_manual(values=c("#6F54A7","#6FAC36")) +
  theme_minimal()+
  theme(
    text=element_text(color="black", family="Roboto"),
    plot.title = element_text(face="bold",size=16),
    axis.text=element_text(color="black",size=16),
    axis.text.x=element_blank(),
    axis.title=element_text(color="black",face="bold",size=16),
    legend.title=element_text(color="black",size=16),
    legend.text=element_text(color="black",size=16)
  ) +
  labs(title="How Much Respondents Felt They Had Learned after Completion", fill="Feedback") +
  xlab("") + ylab("Confidence")
print(plot)
ggsave("end_survey_boxplot_learn.png", width=9, height=6, unit="in")
