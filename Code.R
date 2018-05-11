library(dplyr)
library(zipcode)
library(plyr)
library(data.table)
library(ggplot2)
library(plotly)

set.seed(123)

# Import ------------------------------------------------------------------

d.import <- read.csv("C:/Users/Boris/Desktop/Speed Dating Data.csv")
data(zipcode)


# Clean the Data -----------------------------------------------------------------

d0.0 <- d.import
d0.0$zipcode <- sapply(as.character(d0.0$zipcode),function(x){ifelse(nchar(x)==4,paste0("0",x),x)})
d0.0$state <- left_join(d0.0, zipcode, by = c("zipcode" = "zip"))$state


sapply(names(which(sapply(subset(d0.0, select = 
                                   names(d0.0) %like% c("pf_o")
                                 | names(d0.0) %like% c("1_1")
                                 | names(d0.0) %like% c("2_1")
                                 | names(d0.0) %like% c("3_1")
                                 | names(d0.0) %like% c("4_1")
                                 | names(d0.0) %like% c("5_1")
), max , na.rm = TRUE) > 10))
,function(x){d0.0[,x] <<- d0.0[,x]/10})

d0.0$gender <- plyr::mapvalues(d0.0$gender, from = c(0,1), to = c("Female","Male"))
d0.0$condtn <- plyr::mapvalues(d0.0$condtn, from = c(1,2), to = c("Scarcity","Abundance"))
d0.0$field_cd <- plyr::mapvalues(d0.0$field_cd, from = c(1:18), to = c("Law","Math","Social Science","Medical Science, Pharmaceuticals, and Bio Tech",
                                                                       "Engineering","English/Creative Writing/ Journalism","History/Religion/Philosophy",
                                                                       "Business/Econ/Finance","Education, Academia","Biological Sciences/Chemistry/Physics",
                                                                       "Social Work","Undergrad/undecided","Political Science/International Affairs",
                                                                       "Film","Fine Arts/Arts Administration","Languages","Architecture","Other"))
d0.0$career_c <- plyr::mapvalues(d0.0$career_c, from = c(1:17), to = c(
  "Lawyer","Academic/Research","Psychologist","Doctor/Medicine","Engineer","Creative Arts/Entertainment"
  ,"Bank/Consul/Business/Entrep/Admin","Real Estate","International/Humanitarian Affairs" 
  ,"Undecided","Social Work","Speech Pathology","Politics","Pro sports/Athletics","Other","Journalism","Architecture"))

d0.0$race <- plyr::mapvalues(d0.0$race, from = c(1:6), to = c("Black/African American","European/Caucasian-American","Latino/Hispanic American",
                                                              "Asian/Pacific Islander/Asian-American","Native American","Other"))
d0.0$goal <- plyr::mapvalues(d0.0$goal, from = c(1:6), to = c("Seemed like a fun night out","To meet new people","To get a date",
                                                              "Looking for a serious relationship","To say I did it","Other"))
d0.0$date <- plyr::mapvalues(d0.0$date, from = c(1:7), to = c("Several times a week","Twice a week","Once a week","Twice a month","Once a month",
                                                              "Several times a year","Almost never"))
d0.0$go_out <- plyr::mapvalues(d0.0$go_out, from = c(1:7), to = c("Several times a week","Twice a week","Once a week","Twice a month","Once a month",
                                                                  "Several times a year","Almost never"))

null.func <- function(y){
  setDT(data.frame(n.Count = sapply(y,function(x){sum(is.na(x))}), n.Perc = sapply(y,function(x){sum(is.na(x))/nrow(d0.0)})), keep.rownames = TRUE)[] %>% 
    dplyr::filter(n.Count > 0) %>% dplyr::arrange(desc(n.Count))
}


d.Null <- null.func(d0.0)


# Answering Questions -----------------------------------------------------

## Question 1 - Let's look at the kind of people we are dealing with. I want to see the demographics I am dealing with



d0.dem <- data.frame(d0.0 %>% 
                       select(iid, gender, wave, age, race, field, field_cd, career, career_c, mn_sat, from, zipcode, state, income, date, go_out, imprace, imprelig, exphappy, expnum, goal,attr1_1,sinc1_1,intel1_1,fun1_1,amb1_1,shar1_1,attr2_1,sinc2_1,intel2_1,fun2_1,amb2_1,shar2_1) %>% 
                       group_by(iid, gender, wave, age, race, field, field_cd, career, career_c, mn_sat, from, zipcode, state, income, date, go_out, imprace, imprelig, exphappy, expnum, goal,attr1_1,sinc1_1,intel1_1,fun1_1,amb1_1,shar1_1,attr2_1,sinc2_1,intel2_1,fun2_1,amb2_1,shar2_1) %>% 
                       dplyr::summarise())


## Graphical Histograms
ggplotly(ggplot(subset(d0.dem,!is.na(race)), aes(x = race, fill = gender)) +
           geom_histogram(stat = 'count') + 
           facet_wrap(~wave) +
           theme(axis.text.x = element_text(angle = 40, hjust = 1),legend.position="none"))

ggplotly(ggplot(subset(d0.dem,!is.na(career_c)), aes(x = career_c, fill = gender)) +
           geom_histogram(stat = "count") +
           theme(axis.text.x = element_text(angle = 20, hjust = 1),legend.position="none"))



## Graphical Distributions
ggplotly(ggplot(subset(d0.dem,!is.na(age)), aes(x = age, fill = gender)) +
           geom_density(alpha = .6) + 
           facet_wrap(~wave) +
           theme(legend.position="top"))


ggplotly(
  cbind(
    d0.dem %>% 
      select(gender, contains("1_")) %>% 
      melt(id = "gender") %>%
      dplyr::rename(gender_1 = gender, variable_1 = variable, value_1 = value),
    d0.dem %>% 
      select(gender, contains("2_")) %>% 
      melt(id = "gender") %>%
      dplyr::rename(gender_2 = gender, variable_2 = variable, value_2 = value)
  ) %>%
    select(gender_1, variable_1, value_1, value_2) %>%
    mutate(variable_1 = plyr::mapvalues(.$variable_1, from = c("attr1_1","sinc1_1","intel1_1","fun1_1","amb1_1","shar1_1"), to = c("Attractive","Sincere","Intelligent","Fun","Ambitious","Share Commonalities"))) %>%
    split(.$gender) %>%
    within(Female$index <- with(Female, ave(seq_along(variable_1), variable_1, FUN=seq_along))) %>%
    within(Male$index <- with(Male, ave(seq_along(variable_1), variable_1, FUN=seq_along))) %>%
    with(inner_join(Male,Female, by = c("index" = "index", "variable_1" = "variable_1"))) %>%
    with(na.omit(subset(.,select = c(variable_1, value_1.x, value_2.x, value_1.y, value_2.y)))) %>%
    melt(id = "variable_1") %>% mutate(variable_1 = ifelse(variable %like% "_1",paste(variable_1,1),paste(variable_1,2))
                                       ,variable = ifelse(variable %like% "_1.x" | variable %like% "_2.y", "Male","Female")) %>%
    ggplot(aes(x = value, fill = variable_1)) + 
    geom_density(alpha = 0.5) + 
    facet_wrap( ~  variable) + 
    scale_fill_brewer(palette="Paired", direction = -1))



## Graphical 3d cube
plot_ly(data = d0.dem, x = ~gender, y = ~ age, z = ~income, type = "scatter3d", color = ~ field_cd)



## Graphical Violin Plots
d0.dem %>% 
  select(gender,attr1_1,sinc1_1,intel1_1,fun1_1,amb1_1,shar1_1) %>% 
  melt(id = "gender") %>%
  mutate(variable = plyr::mapvalues(.$variable, from = c("attr1_1","sinc1_1","intel1_1","fun1_1","amb1_1","shar1_1"), to = c("Attractive","Sincere","Intelligent","Fun","Ambitious","Share Commonalities"))) %>%
  ggplot(aes(x = variable, y = value, fill = gender)) +
  geom_violin(position = position_dodge(width = 0.5)) + 
  geom_boxplot(width=0.1,position = position_dodge(width = 0.5)) +
  theme(axis.title=element_blank(), legend.position = "top")


## Graphical TreeMaps and Tile Graphs
ggplotly(
  subset(d0.dem, subset =!is.na(date) & !is.na(go_out), select = c(gender, date, go_out)) %>% 
    mutate(go_out = factor(go_out, levels = c("Almost never","Several times a year","Once a month","Twice a month","Once a week","Twice a week","Several times a week")),
           date = factor(date, levels = c("Almost never","Several times a year","Once a month","Twice a month","Once a week","Twice a week","Several times a week"))) %>%
    group_by(gender, go_out, date) %>% 
    dplyr::summarise(count = n()) %>% 
    ggplot(aes(x = go_out, y = date, fill = count)) +
    geom_tile() +
    facet_wrap(~ gender) +
    theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
    theme(panel.background=element_rect(fill="white")))



subset(d0.dem, subset =!is.na(date) & !is.na(go_out), select = c(gender, date, go_out)) %>% 
  mutate(go_out = factor(go_out, levels = c("Almost never","Several times a year","Once a month","Twice a month","Once a week","Twice a week","Several times a week")),
         date = factor(date, levels = c("Almost never","Several times a year","Once a month","Twice a month","Once a week","Twice a week","Several times a week"))) %>%
  group_by(gender, go_out, date) %>% 
  dplyr::summarise(count = n()) %>% 
  ggplot(aes(area = count, fill = date, subgroup = go_out)) + 
  geom_treemap() +
  geom_treemap_subgroup_border(color = "black") +
  facet_wrap(~ gender) +
  geom_treemap_subgroup_text(place = "centre", grow = T, colour = "white", min.size = 0)


## Graphical Geom Bar 

ggplotly(subset(d0.dem, subset =!is.na(date) & !is.na(go_out), select = c(gender, date, go_out)) %>% 
           mutate(go_out = factor(go_out, levels = c("Almost never","Several times a year","Once a month","Twice a month","Once a week","Twice a week","Several times a week")),
                  date = factor(date, levels = c("Almost never","Several times a year","Once a month","Twice a month","Once a week","Twice a week","Several times a week"))) %>%
           group_by(gender, go_out, date) %>% 
           dplyr::summarise(count = n()) %>% data.frame(.) %>%
           ggplot(aes(x = go_out, y = count, fill = date)) +
           geom_bar(position = "fill", stat = "identity") +
           facet_wrap(~gender) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)))


ggplotly(ggplot(subset(d0.dem,!is.na(goal)), aes(x = age, fill = goal)) +
           geom_histogram(alpha = .6, position = "identity", binwidth = 1) +
           theme(legend.position="top") + 
           facet_wrap(~gender) 
)



# Quick Analysis ----------------------------------------------------------

prop.table(table(d0.dem$race ,d0.dem$imprace),1)
prop.table(table(d0.dem$race ,d0.dem$imprelig),1)



# Predicting what influences level of attractiveness desire ---------------

pred.attr.0 <- d0.dem %>% select(-iid, -wave, -field, -career, -mn_sat, -from, -zipcode, -state, -income, -contains("2_"), -contains("1_"), -expnum, attr1_1)
null.attr <- null.func(pred.attr.0)
pred.attr.1 <- na.omit(pred.attr.0)

partition1 <- createDataPartition(pred.attr.0$gender, p=.8, list = FALSE)

train.attr <- pred.attr.0[partition1,]
test.attr <- pred.attr.0[-partition1,]

tree.attr <- rpart(formula = attr1_1 ~ ., data = train.attr, parms = list(split = "information"))
fancyRpartPlot(tree.attr,cex=.6)
rsq.rpart(tree.attr)

model.summary <- data.frame(cbind(actual = test.attr$attr1_1, predicted = predict(tree.attr, test.attr)))

ggplot(model.summary, aes(x = actual, y = predicted)) + 
  geom_point() + 
  geom_abline() + 
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) 


© 2018 GitHub, Inc.
Terms
Privacy
Security
Status
Help

Contact GitHub
API
Training
Shop
Blog
About

Press h to open a hovercard with more details.
