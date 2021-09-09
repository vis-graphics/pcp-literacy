library(tidyverse)
library(extrafont)
library(patchwork)

#Data
end_survey <- read_csv("end_survey.csv")
end_survey_reshape <- gather(end_survey, "question", "answer", "Recognize", "Interpret", "Create", "Future Use", -"MasteryLearning")

confidence <- read_csv("confidence_over_modules.csv")
confidence_reshape <- gather(confidence, "module", "answer", "Module 1", "Module 2", "End of Modules", -"MasteryLearning")

chart_familiarity <- read_csv("chart_familiarity.csv")
familiarity_reshape <- gather(chart_familiarity, "Chart", "Answer", "Line", "Bar", "Stacked Bar", "Percent Stacked Bar", "Pie", "Histogram","Scatterplot","Bubble","Stacked Area","Choropleth","Treemap","Area", -"MasteryLearning")

#Grading
remember_grades <- read_csv("1_remember_grades.csv")
remember_reshape <- gather(remember_grades, "MasteryLearning", "Grade", "Mastery Learning", "No Mastery Learning")

understand_grades <- read_csv("2_understand_grades.csv")
understand_reshape <- gather(understand_grades, "MasteryLearning", "Grade", "Mastery Learning", "No Mastery Learning")

apply_grades <- read_csv("3_apply_grades.csv")
apply_reshape <- gather(apply_grades, "MasteryLearning", "Grade", "Mastery Learning", "No Mastery Learning")

analyze_grades <- read_csv("4_analyze_grades.csv")
analyze_reshape <- gather(analyze_grades, "MasteryLearning", "Grade", "Mastery Learning", "No Mastery Learning")

critique_grades <- read_csv("5_critique_grades.csv")
critique_reshape <- gather(critique_grades, "MasteryLearning", "Grade", "Mastery Learning", "No Mastery Learning")

create_grades <- read_csv("6_create_grades.csv")
create_reshape <- gather(create_grades, "MasteryLearning", "Grade", "Mastery Learning", "No Mastery Learning")

#Fig. 12: Apply Module Scores
plot <- 
  ggplot(apply_reshape, aes(y=MasteryLearning, x=Grade)) +
  geom_boxplot(fill=c("#6F54A7","#6FAC36")) +
  scale_y_discrete(labels=c("Mastery Learning","No Mastery Learning")) +
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
plot <- end_survey_reshape %>%   
  mutate(question = fct_relevel(question, "Recognize", "Interpret", "Create", "Future Use")) %>% 
  ggplot(aes(x=question, y=answer, fill=MasteryLearning)) +
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
  labs(title="Confidence with Parallel Coordinates after Completion", fill="Mastery Learning") +
  xlab("") + ylab("Confidence")
print(plot)
ggsave("end_survey_boxplot.png")

#Fig. 19: Confidence in Recognizing Parallel Coordinates Charts
plot <- 
  ggplot(confidence_reshape, aes(x=module, y=answer, fill=MasteryLearning)) +
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
  labs(title="Confidence in Recognizing Parallel Coordinates Charts", fill="Mastery Learning") +
  xlab("") + ylab("Confidence")
print(plot)
ggsave("confidence_boxplot.png")

#Fig. 21: Summary Figure
#Remember
p1 <- 
  ggplot(remember_reshape, aes(y=MasteryLearning, x=Grade)) +
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
  ggplot(understand_reshape, aes(y=MasteryLearning, x=Grade)) +
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
  ggplot(apply_reshape, aes(y=MasteryLearning, x=Grade)) +
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

#Analyze
p4 <- 
  ggplot(analyze_reshape, aes(y=MasteryLearning, x=Grade)) +
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
  ggplot(critique_reshape, aes(y=MasteryLearning, x=Grade)) +
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
  ggplot(create_reshape, aes(y=MasteryLearning, x=Grade)) +
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
