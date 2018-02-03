library(tidyr, warn.conflicts=FALSE, quietly=TRUE)
library(readr, warn.conflicts=FALSE, quietly=TRUE)
library(dplyr, warn.conflicts=FALSE, quietly=TRUE)
library(ggplot2, warn.conflicts=FALSE, quietly=TRUE)
library(scales, warn.conflicts=FALSE, quietly=TRUE)
library(shiny, warn.conflicts=FALSE, quietly=TRUE)
library(plotly, warn.conflicts=FALSE, quietly=TRUE)
library(htmlwidgets, warn.conflicts=FALSE, quietly=TRUE)
library(devtools, warn.conflicts=FALSE, quietly=TRUE)
library(streamgraph, warn.conflicts=FALSE, quietly=TRUE)

library(zoo, warn.conflicts=FALSE, quietly=TRUE)
library(xts, warn.conflicts=FALSE, quietly=TRUE)
library(tseries, warn.conflicts=FALSE, quietly=TRUE)
library(forecast, warn.conflicts=FALSE, quietly=TRUE)
library(vcd, warn.conflicts=FALSE, quietly=TRUE)

# devtools::install_github("hrbrmstr/streamgraph")
# devtools::install_github("ramnathv/rCharts")

# api key credententials (moved to .Rprofile)
# Sys.setenv("plotly_username"="thekotecha")

# --------------------------------------------
# Loading
# --------------------------------------------
setwd("D:/Github/Data_Yogee_Analysis/data")

# data from three years worth of data from Yogee App (not released), dumped into .csv
df_habits <- tbl_df(read.csv("extract1.csv", stringsAsFactors=FALSE))
df_info <- tbl_df(read.csv("extract2.csv", stringsAsFactors=FALSE))

# --------------------------------------------
# Wrangling
# --------------------------------------------

# convert to date
df_habits$full_date <- as.Date(df_habits$full_date,"%d-%b-%y")

# clean up 'sick' / 'event' column
# codes:
# 1 - sick
# 9 - data not collected
# 2 - 
df_habits$sick[df_habits$sick == "s"] <- 1
df_habits$sick <- as.integer(df_habits$sick)
# df_habits$sick[is.na(df_habits$sick)] <- 9
summary(as.factor(df_habits$sick))

# clean up 'tlo' var data
df_habits$tlo <- as.integer(df_habits$tlo)
# df_habits$tlo[is.na(df_habits$tlo)] <- 0
# convert to a step f(x) for consistency of input
df_habits$tlo[df_habits$tlo > 6] <- 10
df_habits$tlo[df_habits$tlo <= 6 & df_habits$tlo > 2] <- 5
summary(as.factor(df_habits$tlo))

# clean up 'ent' var data
df_habits$ent <- as.integer(df_habits$ent)
# df_habits$ent[is.na(df_habits$ent)] <- 0
# convert to a step f(x) for consistency of input
df_habits$ent[df_habits$ent > 9] <- 12
df_habits$ent[df_habits$ent <= 9 & df_habits$ent > 6] <- 9
df_habits$ent[df_habits$ent <= 6 & df_habits$ent > 3] <- 6
df_habits$ent[df_habits$ent <= 3 & df_habits$ent > 0] <- 3
summary(as.factor(df_habits$ent))

# clean up 'tasks_done', 'habits_done', 'total_habits' and 'mood'
df_habits$tasks_done <- as.integer(df_habits$tasks_done)
summary(as.factor(df_habits$tasks_done))
# df_habits$tasks_done[is.na(df_habits$tasks_done)] <- 0
df_habits$habits_done <- as.integer(df_habits$habits_done)
summary(as.factor(df_habits$habits_done))
# df_habits$habits_done[is.na(df_habits$habits_done)] <- 0
df_habits$total_habits <- as.integer(df_habits$total_habits)
summary(as.factor(df_habits$total_habits))
# df_habits$total_habits[is.na(df_habits$total_habits)] <- 0
df_habits$mood <- as.integer(df_habits$mood)
summary(as.factor(df_habits$mood))
# df_habits$mood[is.na(df_habits$mood)] <- 0

# convert all character variables ("h_*") to int
names(df_habits[ , grepl("h_", names(df_habits))])
df_habits[ , grepl("h_", names(df_habits))] <- sapply(df_habits[ , grepl("h_", names(df_habits))],as.integer)

# fix wake and sleep variables



# combine all h_clean_* vars
df_habits$h_clean <- ifelse(df_habits$h_clean_bookmarks + df_habits$h_clean_room > 1, 1, df_habits$h_clean_bookmarks + df_habits$h_clean_room)
df_info$habit[df_info$habit == "h_clean_room"] <- "h_clean"

# combine all h_night_* workout vars
df_habits$h_night <- ifelse(df_habits$h_night_workout + df_habits$h_night_abs > 1, 1, df_habits$h_night_workout + df_habits$h_night_abs)
df_info$habit[df_info$habit == "h_night_workout"] <- "h_night"

# display all habit columns
names(df_habits[ , grepl("h_", names(df_habits))])


# --------------------------------------------
# Exploratory Data Analysis
# --------------------------------------------

# what does success look like?
# 1. discipline (look at habits over time)
# --------------------------------------------


# Determine habits with sufficient historical data
# --------------------------------------------
# remove all NA values from the df (?complete.cases)
df_habits <- na.omit(df_habits)

# select only habit variables that have a decent amount of data collected through the years
# create temp variable to subset totals on habits
temp <- as.data.frame(aggregate(df_habits[ , grepl("h_", names(df_habits))],by=df_habits["year"],FUN=sum))

# gather different habits into a single variable
temp <- gather(temp, "habits", "total",grep("h_", colnames(temp)), factor_key=TRUE)

# visualize baby, visualize!
# plot of all habits by year and count
ggplot(data = temp, aes(reorder(habits, total), total)) + 
  geom_bar(stat = "identity", aes(fill = year), position = "stack") + 
  geom_text(size = 2, position = position_stack(vjust = 0.5), aes(label = total), colour = "white") + 
  theme_bw(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.title = element_blank(),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.justification = "left"
  ) + 
  # scale_y_continuous (labels = percent, limits = c(0, 1.1), breaks = seq(0,1.1,0.2)) + 
  labs (x=NULL, y=NULL) +
  # scale_fill_manual(values=c(DEMS_C1, DEMS_C3, "grey75", REPS_C3, REPS_C1)) +
  coord_flip()

# based on graphical analysis the following variables have insufficient data for our analysis (arbitrary value of 70 picked)
df_habits_main <- select(df_habits, -c(h_night_workout, h_night_abs, h_clean_room, h_clean_bookmarks, h_gratif, h_procas, h_disc, 
                                       h_night_eat, h_cold_showers, h_focus, h_bed, h_study_cfa, h_morning_mental, 
                                       h_read_2wks, h_workout_nature, h_learning, h_growth, h_meditate_sleep, h_study_course, 
                                       h_writing, h_mind, h_meditate_twice, h_meditate_long, h_sports, h_ego, h_robin, 
                                       h_morn_workout, h_vlog, h_skills, h_hobbies, h_library, h_coffee))

# create temp variable to subset totals on habits
temp <- as.data.frame(aggregate(df_habits_main[ , grepl("h_", names(df_habits_main))],by=df_habits_main["year"],FUN=sum))

# gather different habits into a single variable
temp <- gather(temp, "habits", "total",grep("h_", colnames(temp)), factor_key=TRUE)

# visualize baby, visualize!
# plot of all habits by year and count
ggplot(data = temp, aes(reorder(habits, total), total)) + 
  geom_bar(stat = "identity", aes(fill = year), position = "stack") + 
  geom_text(data = subset(temp, total != 0), size = 3, position = position_stack(vjust = 0.5), aes(label = total), colour = "white") + 
  theme_bw(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.title = element_blank(),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.justification = "left"
  ) + 
  # scale_y_continuous (labels = percent, limits = c(0, 1.1), breaks = seq(0,1.1,0.2)) + 
  labs (x=NULL, y=NULL) +
  # scale_fill_manual(values=c(DEMS_C1, DEMS_C3, "grey75", REPS_C3, REPS_C1)) +
  coord_flip()



# divide up habits into lists for analysis
# main - habits from df_info
habits_main <- df_info$habit[df_info$main == "x"]

# create new variable for all main habits
df_habits$sum_main <- apply(df_habits[, colnames(df_habits) %in% habits_main], 1, sum, na.rm = TRUE)

# habits that were kept consistently for almost a year
habits_full_year <- c("h_yoga", "h_pranayama", "h_clean", "h_night", "h_ted")

# habits that were kept consistently for less than half a year
habits_half_year <- c("h_tv", "h_gratif", "h_night_eat", "h_cold_showers", "h_disc", "h_focus")


# Explore habit completion % by year for each type of habit
# --------------------------------------------
# create table of main habits
temp <- select(df_habits, one_of(habits_main), year)
temp <- gather(temp, "h_main", "total",grep("h_", colnames(temp)), factor_key=TRUE)
temp2 <- aggregate(. ~ year, data = temp, FUN = function(x) c(p = round((sum(x) / length(x)),2)))

# create table of secondary habits for at a year
temp <- select(df_habits, one_of(habits_full_year), year)
temp <- gather(temp, "h_full", "total",grep("h_", colnames(temp)), factor_key=TRUE)
temp <- aggregate(. ~ year, data = temp, FUN = function(x) c(p = round((sum(x) / length(x)),2)))
temp2 <- full_join(temp2, temp)

# create table of secondary habits for less than a year
temp <- select(df_habits, one_of(habits_half_year), year)
temp <- gather(temp, "h_half", "total",grep("h_", colnames(temp)), factor_key=TRUE)
temp <- aggregate(. ~ year, data = temp, FUN = function(x) c(p = round((sum(x) / length(x)),2)))
temp2 <- full_join(temp2, temp)

# combine all habit factors
temp <- gather(temp2, "habits", "del",grep("h_", colnames(temp2)), factor_key=TRUE)
temp <- na.omit(temp)
temp <- temp %>% select(-del)

# visualize baby, visualize!
# plot of all habit groups by year and percentage complete
ggplot(data = temp, aes(x = habits, y = total, fill = factor(year))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(size = 2, position = position_dodge(width = 0.9), vjust = -1, aes(label = paste0(total*100,"%"))) + 
  theme_minimal(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.title = element_blank(),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.justification = "left"
  ) + 
  scale_y_continuous (oob=rescale_none, labels = percent, limits = c(0, 1), breaks = seq(0,1,0.2)) + 
  labs (x=NULL, y=NULL) +
  scale_fill_brewer(palette="PuRd")


# Explore habit completion % by year for main habits
# --------------------------------------------
# create subset of main habits 
temp <- select(df_habits, one_of(habits_main), year)

# find percentage complete by year
temp <- aggregate(. ~ year, data = temp, FUN = function(x) c(p = round((sum(x) / length(x)),2)))

# gather different habits into a single variable
temp <- gather(temp, "habits", "total",grep("h_", colnames(temp)), factor_key=TRUE)

# visualize baby, visualize!
# plot of all main habits by year, month and percentage complete
ggplot(data = temp, aes(x = habits, y = total, fill = factor(year))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(size = 2, position = position_dodge(width = 0.9), vjust = -1, aes(label = paste0(total*100,"%"))) + 
  theme_minimal(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.title = element_blank(),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.justification = "left"
  ) + 
  scale_y_continuous (oob=rescale_none, labels = percent, limits = c(0, 1), breaks = seq(0,1,0.2)) + 
  labs (x=NULL, y=NULL) +
  scale_fill_brewer(palette="PuRd")


# Explore habit completion % by year for key 7 habits
# --------------------------------------------
# create graphs for a few habits by month / week / year
# pick main seven habits - for ease of graph readibility
habits_main_seven <- habits_main[c(3, 5, 8, 9, 11, 12, 13)]

# create subset of main habits 
temp <- select(df_habits, one_of(habits_main_seven), full_date)

# cut dates into intervals of weeks
#temp$month <- as.Date(cut(temp$full_date, breaks = "month"))
temp$week <- as.Date(cut(temp$full_date, breaks = "week", start.on.monday = TRUE))
temp <- temp %>% select(-full_date)

# find percentage complete by week
#temp <- aggregate(. ~ month, data = temp, FUN = function(x) c(p = round((sum(x) / length(x)),2)))
temp <- aggregate(. ~ week, data = temp, FUN = function(x) c(p = round((sum(x) / length(x)),2)))

# gather different habits into a single variable
temp <- gather(temp, "habits", "total",grep("h_", colnames(temp)), factor_key=TRUE)

# visualize baby, visualize!
# plot of main seven habits by week
ggplot(data = temp, aes(x = week, y = total, colour = habits)) + 
  geom_blank(na.rm = TRUE) +
  geom_smooth(method = 'loess', span = 1, se = FALSE, size = 2) + 
  theme_minimal(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.title = element_blank(),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.justification = "left"
  ) + 
  scale_y_continuous (oob=rescale_none, labels = percent, limits = c(0, 1), breaks = seq(0,1,0.2)) + 
  #scale_x_date(labels = date_format("%Y-%m"), breaks = "1 month") + 
  labs (x=NULL, y=NULL) +
  scale_colour_brewer(palette = "Set2")


# Explore habit completion % by year for secondary habits (almost a year)
# --------------------------------------------
# create subset of secondary habits maintained for almost a year
temp <- select(df_habits, one_of(habits_full_year), full_date)

# cut dates into intervals of weeks
#temp$month <- as.Date(cut(temp$full_date, breaks = "month"))
temp$week <- as.Date(cut(temp$full_date, breaks = "week", start.on.monday = TRUE))
temp <- temp %>% select(-full_date)

# find percentage complete by week
#temp <- aggregate(. ~ month, data = temp, FUN = function(x) c(p = round((sum(x) / length(x)),2)))
temp <- aggregate(. ~ week, data = temp, FUN = function(x) c(p = round((sum(x) / length(x)),2)))

# gather different habits into a single variable
temp <- gather(temp, "habits", "total",grep("h_", colnames(temp)), factor_key=TRUE)

# visualize baby, visualize!
# plot of main seven habits by week
ggplot(data = temp, aes(x = week, y = total, colour = habits)) + 
  geom_blank(na.rm = TRUE) +
  geom_smooth(method = 'loess', span = 1, se = FALSE, size = 2) + 
  theme_minimal(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.title = element_blank(),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.justification = "left"
  ) + 
  scale_y_continuous (oob=rescale_none, labels = percent, limits = c(0, 1), breaks = seq(0,1,0.2)) + 
  #scale_x_date(labels = date_format("%Y-%m"), breaks = "1 month") + 
  labs (x=NULL, y=NULL) +
  scale_colour_brewer(palette = "Set2")


# 2. focus (look at number of habits)
# ---------------------------------------

# Explore habit done # and % by year
# --------------------------------------------
# plot total habits and completion percentage with time
# create subset of neccessary columns
temp <- select(df_habits, full_date, habits_done, total_habits)

# cut dates into intervals of weeks
#temp$month <- as.Date(cut(temp$full_date, breaks = "month"))
temp$week <- as.Date(cut(temp$full_date, breaks = "week", start.on.monday = TRUE))
temp <- temp %>% select(-full_date)

# find percentage complete by week
#temp <- aggregate(. ~ month, data = temp, FUN = function(x) c(p = round((sum(x) / length(x)),2)))
temp <- aggregate(. ~ week, data = temp, FUN = function(x) c(mu = round(mean(x),2)))

# add variable for percentage completion
temp$p <- round(temp$habits_done / temp$total_habits, 2)
summary(temp)

# gather different vars to analyze into a single variable
temp <- gather(temp, "habits", "total",2:4, factor_key=TRUE)
head(temp)

# visualize baby, visualize!
# plot of completion percentage and total habits by week
ggplot(data = temp, aes(x = week, y = total, colour = habits)) + 
  geom_blank(na.rm = TRUE) +
  geom_smooth(method = 'loess', span = 1, se = FALSE, size = 2) + 
  theme_minimal(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.title = element_blank(),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.justification = "left"
  ) + 
  labs (x=NULL, y=NULL) +
  scale_colour_brewer(palette = "Set2") + 
  facet_wrap(~habits, ncol = 1, scales = "free")


df_habits_main

temp <- select(df_habits, full_date, year, h_workout, h_jam, sum_main)
table(temp$h_workout, temp$h_jam, temp$year)
ftable(temp$h_workout+temp$h_jam ~ temp$year)
table.one <- table(temp$h_workout, temp$h_jam)
table(temp$h_workout)
margin.table(table.one, 1)
table(temp$h_jam)
margin.table(table.one, 2)
margin.table(table.one)
prop.table(table.one, 1)
prop.table(table.one, 2)
prop.table(table.one)
barplot(table.one, beside = T, col = "white", names.arg=c("bad","good"))

prop.test(105, 129, .1)
# prop.test(105, 129, p = 0.1, alternative = "two.sided" (or greater), conf.level = 0.99, correct = TRUE (checks for continuity correciton))
prop.test(105, 129, p = 0.1, alternative = "two.sided", conf.level = 0.99, correct = TRUE)




temp.x <- xtabs( ~ h_workout + h_jam, temp)
temp.x
addmargins(temp.x)
prop.table(temp.x)
prop.table(temp.x, margin = 1) # proportion to row sum
prop.table(temp.x, margin = 2) # proportion to column sum

chisq.test(temp.x)
fisher.test(temp.x)

temp.x <- xtabs( ~ h_workout + sum_main + h_jam, temp)
temp.x
addmargins(temp.x)
tab1 <- prop.table(temp.x)
prop.table(temp.x, margin = 1) # proportion to row sum
prop.table(temp.x, margin = 2) # proportion to column sum

# encode weekend vs weekday
  

var(temp$h_workout)
var(temp$h_jam)
var(temp$sum_main)
cov(temp$h_workout, temp$h_jam)
cov(temp$h_workout, temp$sum_main)
cov(temp$h_jam, temp$sum_main)
cor(temp$h_workout, temp$h_jam)
cor(temp$h_workout, temp$sum_main)
cor(temp$h_jam, temp$sum_main)
hist(temp$sum_main)
plot(density(temp$sum_main))

# plot density graph

# box plot for good vs. bad habits for habits done, for mood

# look at max. number of habits sustained by periods of time
# calculate moving average / buckets of habits vs. percentage compl.
# frequency calcs (plot) for bucket of habits by time and percentage complete
- find mode, median


cdata <- ddply(data, c("sex", "condition"), summarise,
               N    = length(change),
               mean = mean(change),
               sd   = sd(change),
               se   = sd / sqrt(N)
)


# Get a count of number of subjects in each category (sex*condition)
cdata <- aggregate(data["subject"], by=data[c("sex","condition")], FUN=length)
# Rename "subject" column to "N"
names(cdata)[names(cdata)=="subject"] <- "N"
# Sort by sex first
cdata <- cdata[order(cdata$sex),]
# We also keep the __before__ and __after__ columns:
# Get the average effect size by sex and condition
cdata.means <- aggregate(data[c("before","after","change")], 
                         by = data[c("sex","condition")], FUN=mean)
# Merge the data frames
cdata <- merge(cdata, cdata.means)
# Get the sample (n-1) standard deviation for "change"
cdata.sd <- aggregate(data["change"],
                      by = data[c("sex","condition")], FUN=sd)
# Rename the column to change.sd
names(cdata.sd)[names(cdata.sd)=="change"] <- "change.sd"
# Merge
cdata <- merge(cdata, cdata.sd)
# Calculate standard error of the mean
cdata$change.se <- cdata$change.sd / sqrt(cdata$N)
cdata.means <- aggregate(data[c("before","after","change")], 
                         by = data[c("sex","condition")],
                         FUN=mean, na.rm=TRUE)

# 4 GRAPHS:
# frequency calc of habits by buckets of habit #s - focus
# cumulative / density plot by number of day (i.e. percentage % by number of days held) - grit
# calculate streaks for each main habit by year
# knn for habits - no fear
# logistic reg - work outside comfort zone


# look at number of habits completion percentages by time, effort (?)
# of habits that I can stick to at one time
# look at correlation between habits

# get rid of habits not being analyzed
# divide discipline graphs into habits continued vs. not
# divide discipline graphs into cohort averages









# find percentage complete by year and month
temp <- aggregate(. ~ year + month, data = temp, FUN = function(x) c(p = round((sum(x) / length(x)),2)))

# gather different habits into a single variable
temp <- gather(temp, "habits", "total",grep("h_", colnames(temp)), factor_key=TRUE)


# graph against mood
# see how this changes by time, effort, goal and type
# cumulative distribution of main habits - density plot or cumulative?

apply(df_habits[, colnames(df_habits) %in% habits_main], 1, sum, na.rm = TRUE)
nrow(df_habits[, colnames(df_habits) %in% habits_main])
aggregate(df_habits$h_wake, by=df_habits["year"],FUN=NROW)


# aggregate(Mydf$Qty,by=Mydf["DepPC"],FUN=sum) - Read more at: http://scq.io/fCPufvMf#gs.4JyYAYA



# - relation to mood
mood density plot by goal and type
# habits cumulative over time



# 3. 
# habits that go together
# predicting which habits lead to productivity





df_habits %>% group_by(year, month) %>% summarise(sum = sum(total, na.rm = TRUE))


df_habits.fixed %>% group_by(sick) %>% summarise(sum = sum(total, na.rm = TRUE))

habits_sum.total <- df_habits.fixed %>% group_by(habits) %>% summarise(sum = sum(total, na.rm = TRUE))
habits_sum.total <- habits_sum.total %>% filter(sum > 25)


# gather different habits into a single variable to tidy table
df_habits.fixed <- gather(df_habits, "habits", "total",grep("h_", colnames(df_habits)), factor_key=TRUE)

# create new variable for all summed habits
df_habits$sum_all <- apply(df_habits[ , grepl("h_", names(df_habits))], 1, sum, na.rm = TRUE)


trial <- df_habits %>% select(c(full_date, sum_all))

ts.trial <- ts(trial)
plot(ts.trial[,2])
ts.trial <- xts(trial$sum_all, trial$full_date)
plot(ts.trial)


ggplot(data = trial, aes(x = full_date, y = sum_all)) + 
  geom_line(size = 1, linetype = 'solid')


timeseries <- ts(trial[,2], start = 2015-05-10)

tail(trial)

plot(timeseries[,2], type ="l")
timeseries.1 <- stats::filter(timeseries[,2], rep(1/(2*40+1),(2*40+1)))
timeseries.2 <- stats::filter(timeseries[,2], rep(1/(2*20+1),(2*20+1)))
lines(timeseries.1, col = "blue")
lines(timeseries.2, col = "red")

timeseries <- ts(select(trial, c(full_date, sum_main)), freq = 12, start = 2014)
HoltWinters(timeseries)
plot(timeseries[,2], type ="l")
lines(HoltWinters(timeseries[,2])$fitted, col = "red")



# get habit totals
habits_sum.total <- df_habits.fixed %>% group_by(habits) %>% summarise(sum = sum(total, na.rm = TRUE))
habits_sum.total <- habits_sum.total %>% filter(sum > 25)


df_info

# likelihood of sustaining habit if kept for x consecutive days

# 1. discipline (look at habits over time)
# 2. focus (look at number of habits)
# 3. grit (look at how good habits stick)
# 4. no fear (look at how bad habits die)
# 5. get uncomfortable (look at transients and sustainability)


# gather different habits into a single variable to tidy table
df_habits.fixed <- gather(df_habits, "habits", "total",grep("h_", colnames(df_habits)), factor_key=TRUE)

# 
df_habits.fixed %>% group_by(month) %>% summarise(sum = sum(total, na.rm = TRUE))
df_habits.fixed %>% group_by(year) %>% summarise(sum = sum(total, na.rm = TRUE))

df_habits.fixed %>% group_by(year) %>% summarise(sum = sum(total, na.rm = TRUE))
df_habits.fixed %>% group_by(day) %>% summarise(sum = sum(total, na.rm = TRUE))
df_habits.fixed %>% group_by(sick) %>% summarise(sum = sum(total, na.rm = TRUE))


df_info
View(df_info)


# pollution %>% group_by(city) %>% summarise(mean = mean(value))
# use mean, sd, var, n(), median, sum, min, max, quantile, sum (x > 10)





trial.month <- trial %>% group_by(month) %>% summarise(sum=sum(trial[grepl("h_", names(trial)), ],na.rm=FALSE))

trial.month <- by(trial[grepl("h_", names(trial))], trial$month, rowSums)




trial <- df_habits %>% select(c(full_date,sum_main, sum_all))

timeseries <- xts(trial$sum_main, trial$full_date)
trial <- trial %>% select(-sum_main,-full_date)
timeseries <- ts(trial)

timeseries <- ts(trial[,2], start = 2015-05-10)

tail(trial)

plot.ts(timeseries)
plot(timeseries[,2], type ="l")
  timeseries.1 <- stats::filter(timeseries[,2], rep(1/(2*40+1),(2*40+1)))
  timeseries.2 <- stats::filter(timeseries[,2], rep(1/(2*20+1),(2*20+1)))
lines(timeseries.1, col = "blue")
lines(timeseries.2, col = "red")


timeseries <- ts(select(trial, c(full_date, sum_main)), freq = 12, start = 2014)
HoltWinters(timeseries)
plot(timeseries[,2], type ="l")
lines(HoltWinters(timeseries[,2])$fitted, col = "red")

ma(timeseries, order = 12)


pairs(trial)
trial$x <- seq(1, 983, by = 1)

fit <- lm(trial$sum_all ~ trial$sum_main)
names(fit)
res <- fit$residuals
hist(res, breaks = 20, prob = TRUE, col = "red")
lines(density(res), col = "blue")

mu <- mean (res)
sigma <- sd (res)
x <- seq(-60,60, length = 500)
y <- dnorm(x, mu, sigma)
lines(x,y,col = 6)

qqnorm(res)
ks.test(res,"pnorm", mu, sigma)
shapiro.test(res)

plot(trial$sum_main, trial$sum_all)
betahat <- fit$coefficients
x <- seq(0, 200, length = 500)
y <- betahat[1] + betahat[2]*x
lines(x, y, col = "red")
summary(fit)

boxplot(split(trial$sum_main, trial$sum_all))

fitlb <- aov(trial$sum_main ~ trial$sum_all)
summary(fitlb)

fit2 <- aov(trial$sum_main~trial$sum_all + trial$x + trial$sum_main*trial$x)
summary(fit2)


plot(stl(log(timeseries),s.window="periodic"))

warnings()      


# the usual suspects - rough checks on the dataframe
summary(df_habits)
dim(df_habits)
sapply(df_habits, class)
tail(df_habits, 3)
head(df_habits, 3)
View(df_habits)




# abline(reg=lm(AirPassengers~time(AirPassengers)))
# cycle(AirPassengers)
# boxplot(AirPassengers~cycle(AirPassengers))
# Box plot across months will give us a sense on seasonal effect



# pollution %>% group_by(city) %>% summarise(mean = mean(value))
# use mean, sd, var, n(), median, sum, min, max, quantile, sum (x > 10)
# 
# apply (df, 1/2, sum) 1 - to rows, 2 to columns
# lapply(MyList,"[", 1, 2) sapply is a wrapper fcn for lappy, which returns simplest data format
# rep(df,c(3,1,2)) or rep (1, 3) - repeat 1 three times
# 
# normalization:
# MyPoints_Trans1=sweep(MyPoints,2,MyPoints_means,"-")
# MyPoints_Trans2=sweep(MyPoints_Trans1,2,MyPoints_sdev,"/")
#
# aggregate(Mydf$Qty,by=Mydf["DepPC"],FUN=sum) - Read more at: http://scq.io/fCPufvMf#gs.4JyYAYA
# use aggregrate to group and sum
#
# summing by grouping:
# aggregate(Frequency ~ Category, x, sum)
# aggregate(. ~ Category, x, sum) - aggregrate for multiple columns
# tapply(x$Frequency, x$Category, FUN=sum)
# 
# 
# tapply works on a vector, for a data.frame 
# you can use by (which is a wrapper for tapply, take a look at the code):
# by(df[,c(3:5)], df$state, FUN=colSums)
# 
# Base function   Input   Output   plyr function 
# --------------------------------------------------
# aggregate        d       d       ddply + colwise 
# apply            a       a/l     aaply / alply 
# by               d       l       dlply 
# lapply           l       l       llply  
# mapply           a       a/l     maply / mlply 
# replicate        r       a/l     raply / rlply 
# sapply           l       a       laply 





# Explore habit done # and % by year
# --------------------------------------------
# create subset of main habits 
temp <- select(df_habits, sum_main, total_habits, tasks_done)
temp$p <- round(temp$sum_main / 18, 2)

# find mode of total_habits, answer: 19
temp2 <- aggregate(. ~ total_habits, data = temp, FUN = length)

# find percentage complete by total habits
temp <- aggregate(. ~ total_habits, data = temp, FUN = function(x) c(mean = round(mean(x),2)))

# visualize baby, visualize!
# plot of all main habits percentage complete by total number of habits
ggplot(data = temp, aes(x = total_habits, y = p)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(size = 2, position = position_dodge(width = 0.9), vjust = -1, aes(label = paste0(p*100,"%"))) + 
  theme_minimal(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.title = element_blank(),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.justification = "left"
  ) + 
  scale_y_continuous (oob=rescale_none, labels = percent, limits = c(0, 1), breaks = seq(0,1,0.2)) + 
  labs (x=NULL, y=NULL) +
  scale_fill_brewer(palette="PuRd")

# plot of all main habit count by total number of habits
ggplot(data = temp2, aes(x = total_habits, y = sum_main)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  #geom_text(size = 2, position = position_dodge(width = 0.9), vjust = -1, aes(label = paste0(p*100,"%"))) + 
  theme_minimal(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.title = element_blank(),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.justification = "left"
  ) + 
  # scale_y_continuous (oob=rescale_none, labels = percent, limits = c(0, 1), breaks = seq(0,1,0.2)) + 
  labs (x=NULL, y=NULL) +
  scale_fill_brewer(palette="PuRd")


















# create new column to separate out starting year for senate session
df_senate_maj <- separate(df_senate_maj,"period",c("year","end.year"),sep=" - ")

# convert columns to neccessary data type
df_senate_maj$year <- as.integer(df_senate_maj$year)
df_house_maj$incumbents[df_house_maj$incumbents =="391a"] <- 391
df_house_maj$incumbents <- as.integer(df_house_maj$incumbents)

# rename columns
names(df_senate_maj)[6] <- "percentage"

# create new column to identify house vs senate district data
df_house_maj$chamber <- "house"
df_senate_maj$chamber <- "senate"

# remove unnecessary columns
df_senate_maj <- subset(df_senate_maj, select=-c(end.year,south,north))

# combine senate and house datasets for majority districts
df_majority <- full_join(df_house_maj, df_senate_maj)

# create factors
df_majority$chamber <- factor(df_majority$chamber)

# fix percentages
df_majority$percentage <- df_majority$percentage / 100

# gather row headings into column variable by type of victory in district
df_swing_seats <- gather(df_swing_seats, "type", "total", 2:4, factor_key=TRUE)

# remove unnecessary columns for nominal figures
df_costs <- subset(df_costs, select=-c(house.nominal, senate.nominal))

# rename columns
names(df_costs)[2] <- "house"
names(df_costs)[3] <- "senate"

# gather row headings into column variable for chamber
df_costs <- gather(df_costs, "chamber", "cost", 2:3, factor_key=TRUE)
df_costs$cost <- as.integer(gsub("*,*", "", df_costs$cost))

# convert cost to millions
df_costs$cost <- df_costs$cost / 1000000

# for House races
# gather all years into columns to vectorize the table
df_house_incum <- gather (df_house_incum, "year", "cost", -c(result,type1,type2))

# filter for just the rows on cost
df_house_incum <- filter(df_house_incum, grepl("Cost",type2))
df_house_incum$cost <- as.integer(gsub("*,*", "", df_house_incum$cost))

# fix year column to remove extra 'X'
df_house_incum$year <- as.integer(gsub("X*", "", df_house_incum$year))

# rename columns
names(df_house_incum)[2] <- "candidate"

# create factors
df_house_incum$result <- factor(df_house_incum$result)
df_house_incum$candidate <- factor(df_house_incum$candidate)

# filter for just cost for incumbents in house elections by total and then remove column
df_house_incum <- df_house_incum %>% filter(type2 == "Total Cost")
df_house_incum <- subset(df_house_incum, select=-c(type2))

# create separate dataframes for type of victory
df_house_incum_win60plus <- df_house_incum %>% filter(result == "Incumbent won with 60% or more")
df_house_incum_win <- df_house_incum %>% filter(result == "Incumbent won with <60%")
df_house_incum_loss <- df_house_incum %>% filter(result == "Incumbent was defeated")
df_house_incum_win60plus <- subset(df_house_incum_win60plus, select=-c(result))
df_house_incum_win <- subset(df_house_incum_win, select=-c(result))
df_house_incum_loss <- subset(df_house_incum_loss, select=-c(result))

# spread out incumbent and challengers column
df_house_incum_win60plus <- spread (df_house_incum_win60plus, candidate, cost)
df_house_incum_win <- spread (df_house_incum_win, candidate, cost)
df_house_incum_loss <- spread (df_house_incum_loss, candidate, cost)

# create new column for differential of election spending
df_house_incum_win60plus$diffw60 <- round(df_house_incum_win60plus$Incumbents / df_house_incum_win60plus$Challengers, 2)
df_house_incum_win$diffw <- round(df_house_incum_win$Incumbents / df_house_incum_win$Challengers, 2)
df_house_incum_loss$diffl <- round(df_house_incum_loss$Incumbents / df_house_incum_loss$Challengers, 2)

# create new dataset for all differentials
df_house_incumbents <- inner_join(df_house_incum_win60plus,df_house_incum_win, b = "year")
df_house_incumbents <- inner_join(df_house_incumbents,df_house_incum_loss, b = "year")
df_house_incumbents <- subset(df_house_incumbents, select = c(year, diffw60,diffw,diffl))

# for Senate races
# gather all years into columns to vectorize the table
df_senate_incum <- gather (df_senate_incum, "year", "cost", -c(result,type1,type2))

# filter for just the rows on cost
df_senate_incum <- filter(df_senate_incum, grepl("Cost",type2))
df_senate_incum$cost[is.na(df_senate_incum$cost)] <- 0
df_senate_incum$cost <- as.integer(gsub("*,*", "", df_senate_incum$cost))
df_senate_incum$cost[is.na(df_senate_incum$cost)] <- 0

# fix year column to remove extra 'X'
df_senate_incum$year <- as.integer(gsub("X*", "", df_senate_incum$year))

# rename columns
names(df_senate_incum)[2] <- "candidate"

# create factors
df_senate_incum$result <- factor(df_senate_incum$result)
df_senate_incum$candidate <- factor(df_senate_incum$candidate)

# filter for just cost for incumbents in house elections by total and then remove column
df_senate_incum <- df_senate_incum %>% filter(type2 == "Total Cost")
df_senate_incum <- subset(df_senate_incum, select=-c(type2))

# create separate dataframes for type of victory
df_senate_incum_win60plus <- df_senate_incum %>% filter(result == "Incumbent won with 60% or more")
df_senate_incum_win <- df_senate_incum %>% filter(result == "Incumbent won with <60%")
df_senate_incum_loss <- df_senate_incum %>% filter(result == "Incumbent was defeated")
df_senate_incum_win60plus <- subset(df_senate_incum_win60plus, select=-c(result))
df_senate_incum_win <- subset(df_senate_incum_win, select=-c(result))
df_senate_incum_loss <- subset(df_senate_incum_loss, select=-c(result))

# spread out incumbent and challengers column
df_senate_incum_win60plus <- spread (df_senate_incum_win60plus, candidate, cost)
df_senate_incum_win <- spread (df_senate_incum_win, candidate, cost)
df_senate_incum_loss <- spread (df_senate_incum_loss, candidate, cost)

# create new column for differential of election spending
df_senate_incum_win60plus$diffw60 <- round(df_senate_incum_win60plus$Incumbents / df_senate_incum_win60plus$Challengers, 2)
df_senate_incum_win$diffw <- round(df_senate_incum_win$Incumbents / df_senate_incum_win$Challengers, 2)
df_senate_incum_loss$diffl <- round(df_senate_incum_loss$Incumbents / df_senate_incum_loss$Challengers, 2)

# create new dataset for all differentials
df_senate_incumbents <- inner_join(df_senate_incum_win60plus,df_senate_incum_win, b = "year")
df_senate_incumbents <- inner_join(df_senate_incumbents,df_senate_incum_loss, b = "year")
df_senate_incumbents <- subset(df_senate_incumbents, select = c(year, diffw60,diffw,diffl))

# convert text to numbers
df_pac$for.dems <- as.integer(gsub("*,*", "", df_pac$for.dems))
df_pac$for.repub <- as.integer(gsub("*,*", "", df_pac$for.repub))
df_pac$against.dems <- as.integer(gsub("*,*", "", df_pac$against.dems))
df_pac$against.repub <- as.integer(gsub("*,*", "", df_pac$against.repub))
df_pac$total <- as.integer(gsub("*,*", "", df_pac$total))

# remove extra whitespace
df_pac$congress <- gsub("* *", "", df_pac$congress)

# create factors
df_pac$congress <- factor(df_pac$congress)

# convert cost figures for graphic purposes
df_pac$against.dems = df_pac$against.dems * -1
df_pac$against.repub = df_pac$against.repub * -1

# remove extra columns
df_pac <- subset(df_pac, select=-total)

# create separate data frames for house and senate data
df_pac_house <- df_pac %>% filter(congress == "House")
df_pac_senate <- df_pac %>% filter(congress == "Senate")

# # rename columns for House
# names(df_pac_house)[3] <- "house.for.dems"
# names(df_pac_house)[4] <- "house.against.dems"
# names(df_pac_house)[5] <- "house.for.rep"
# names(df_pac_house)[6] <- "house.against.rep"
# 
# # rename columns for Senate
# names(df_pac_senate)[3] <- "sen.for.dems"
# names(df_pac_senate)[4] <- "sen.against.dems"
# names(df_pac_senate)[5] <- "sen.for.rep"
# names(df_pac_senate)[6] <- "sen.against.rep"
# 
# # create new dataframes for democrats and republicans
# df_pac_comb <- full_join(df_pac_senate, df_pac_house, by = 'year')
# df_pac_dems <- subset(df_pac_comb, select = c(year, sen.for.dems, sen.against.dems, house.for.dems, house.against.dems))
# df_pac_rep <- subset(df_pac_comb, select = c(year, sen.for.rep, sen.against.rep, house.for.rep, house.against.rep))

# remove unnecessary columns
df_polar_house <- subset(df_polar_house, select = -c(Nonsouthern.Democrats, Southern.Democrats))
df_polar_senate <- subset(df_polar_senate, select = -c(Nonsouthern.Democrats, Southern.Democrats))

# gather columns
df_polar_house <- gather(df_polar_house, "type", "total", 3:5, factor_key=TRUE)
df_polar_senate <- gather(df_polar_senate, "type", "total", 3:5, factor_key=TRUE)                         

# remove unnecessary columns
df_media_trust <- subset(df_media_trust, select = -Overall)

# rename columns for ease of use
names(df_media_trust)[2] <- "cons.lib"
names(df_media_trust)[3] <- "most.lib"
names(df_media_trust)[4] <- "mixed"
names(df_media_trust)[5] <- "most.con"
names(df_media_trust)[6] <- "cons.con"

# convert to integer
df_media_trust$cons.lib <- as.integer(gsub("%", "", df_media_trust$cons.lib))
df_media_trust$most.lib <- as.integer(gsub("%", "", df_media_trust$most.lib))
df_media_trust$mixed <- as.integer(gsub("%", "", df_media_trust$mixed))
df_media_trust$most.con <- as.integer(gsub("%", "", df_media_trust$most.con))
df_media_trust$cons.con <- as.integer(gsub("%", "", df_media_trust$cons.con))

# calculate total percentage of respondents that have a trust / distrust opinion
df_media_trust$total <- rowSums(df_media_trust[2:6])

# calculate percentages
df_media_trust$cons.lib <- round(df_media_trust$cons.lib / df_media_trust$total, 2)
df_media_trust$most.lib <- round(df_media_trust$most.lib / df_media_trust$total, 2)
df_media_trust$mixed <- round(df_media_trust$mixed / df_media_trust$total, 2)
df_media_trust$most.con <- round(df_media_trust$most.con / df_media_trust$total, 2)
df_media_trust$cons.con <- round(df_media_trust$cons.con / df_media_trust$total, 2)

# update total column
df_media_trust$total <- rowSums(df_media_trust[2:6])

# fix differential due to rounding error
df_media_trust$diff <- 1 - df_media_trust$total
df_media_trust$mixed <- df_media_trust$mixed + df_media_trust$diff
df_media_trust$total <- rowSums(df_media_trust[2:6])

# set variable to order bar graph by
df_media_trust$order <- rowSums(df_media_trust[2:4])

# remove unnecessary columns
df_media_trust <- subset(df_media_trust, select = -c(total, diff))

# rename columns for legend purposes
names(df_media_trust)[2] <- "Consistently Liberal"
names(df_media_trust)[3] <- "Mostly Liberal"
names(df_media_trust)[4] <- "Mixed"
names(df_media_trust)[5] <- "Mostly Conservative"
names(df_media_trust)[6] <- "Consistently Conservative"

# gather columns
df_media_trust <- gather(df_media_trust, "type", "total", 2:6, factor_key=TRUE)

# to remove zero labels during graphing bar graph
df_media_trust$total[df_media_trust$total == 0] <- NA


# --------------------------------------------
# Graphing
# --------------------------------------------

# Styling
# theme constants for graphing
HOUSE_C1 <- "#32b200" # green
SENATE_C1 <- "#a100ff" # purple
HOUSE_C2 <- "#56ff3f" # light green
SENATE_C2 <- "#d284ff" # light purple
HOUSE_C3 <- "#0b6000" # dark green
SENATE_C3 <- "#5a008e" # dark purple
DEMS_C1 <- 'rgba(0, 107, 201, 0.75)' # dark blue
REPS_C1 <- 'rgba(244, 0, 0, 0.75)' # dark red
DEMS_C2 <- 'rgba(201, 229, 255, 0.75)' # light blue
REPS_C2 <- 'rgba(255, 201, 201, 0.75)' # light red
DEMS_C3 <- "#54aaf9" # blue
REPS_C3 <- "#ff5b5b" # red

# set axis styling
f1 <- list(
  family = "serif",
  color = "black", 
  size = 10
)
ax <- list(
  title = "",
  titlefont = f1,
  showticklabels = TRUE,
  tickfont = f1, 
  showgrid = FALSE
)
ay <- list(
  title = "$ in millions",
  titlefont = f1,
  showticklabels = TRUE,
  tickfont = f1, 
  showgrid = FALSE
)
ay2 <- list(
  title = "",
  titlefont = f1,
  showticklabels = TRUE,
  tickfont = f1, 
  showgrid = FALSE
)

# set title annotations for subplots
title1 <- list(
  x = 0.5,
  y = 1,
  text = "Incumbents win with >=60%",
  xref = "paper",
  yref = "paper",
  xanchor = "center",
  yanchor = "top",
  font = list(color = 'black', size = 12, family = 'serif'), 
  showarrow = FALSE
)
title2 <- list(
  x = 0.5,
  y = 1,
  text = "Incumbents win with <60%",
  xref = "paper",
  yref = "paper",
  xanchor = "center",
  yanchor = "top",
  font = list(color = 'black', size = 12, family = 'serif'), 
  showarrow = FALSE
)
title3 <- list(
  x = 0.5,
  y = 1,
  text = "Incumbents lose",
  xref = "paper",
  yref = "paper",
  xanchor = "center",
  yanchor = "top",
  font = list(color = 'black', size = 12, family = 'serif'), 
  showarrow = FALSE
)
title4 <- list(
  x = 0.5,
  y = 1,
  text = "House",
  xref = "paper",
  yref = "paper",
  xanchor = "center",
  yanchor = "top",
  font = list(color = 'black', size = 12, family = 'serif'), 
  showarrow = FALSE
)
title5 <- list(
  x = 0.5,
  y = 1,
  text = "Senate",
  xref = "paper",
  yref = "paper",
  xanchor = "center",
  yanchor = "top",
  font = list(color = 'black', size = 12, family = 'serif'), 
  showarrow = FALSE
)


# Plots
# Plot - Districts Won with 60% of Major Party Vote in Both Chambers, 1944 - 2012
p <- ggplot(data = df_majority, aes(x = year, y = percentage, colour = chamber)) + 
  geom_point(size = 2, stroke = 1, shape = 21) +
  geom_smooth(method = 'lm', span = 5, se = FALSE) + 
  theme_bw(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.position = "none"
  ) + 
  scale_x_continuous (limits = c(1940, 2012), breaks = seq(1940,2012,10)) + 
  scale_y_continuous (labels = percent, limits = c(0.2, 1), breaks = seq(0.2,1,.2)) + 
  labs (x=NULL, y=NULL) + 
  scale_colour_manual(values = c(HOUSE_C1,SENATE_C1)) + 
  annotate("text", x = 1995, y = 0.85, label = "House", colour = HOUSE_C1, size = 4, fontface = 'bold') + 
  annotate("text", x = 1965, y = 0.35, label = "Senate", colour = SENATE_C1, size = 4, fontface = 'bold') 
ggplotly(p)
link <- plotly_POST(p, filename = "g1")

# Plot - Swing Districts in the House, 1998 - 2014
p <- ggplot(data = df_swing_seats, aes(x = year, y = total, colour = type)) + 
  geom_line(size = 2, linetype = 'solid') + 
  theme_bw(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.position = "none"
  ) + 
  scale_x_continuous (limits = c(1998, 2014), breaks = seq(1998,2014,4)) + 
  scale_y_continuous (limits = c(80, 200), breaks = seq(80,200,20)) + 
  labs (x=NULL, y="# of districts") + 
  scale_colour_manual(values = c(DEMS_C3, REPS_C3, 'grey')) + 
  annotate("text", x = 2006, y = 187, label = "Republicans win by >5%", colour = REPS_C3, size = 4, fontface = 'bold') + 
  annotate("text", x = 2006, y = 155, label = "Democrats win by >5%", colour = DEMS_C3, size = 4, fontface = 'bold') + 
  annotate("text", x = 2004, y = 100, label = "Swing seats", colour = 'grey', size = 4, fontface = 'bold')
ggplotly(p)
link <- plotly_POST(p, filename = "g2")

# Plot - Cost of Running for Senate and House (inflation adjusted based on 2012 CPI), 1986-2012
p <- ggplot(data = df_costs, aes(x = year, y = cost, colour = chamber)) + 
  geom_line(size = 2, linetype = 'solid') + 
  theme_bw(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.position = "none"
  ) + 
  scale_x_continuous (limits = c(1986, 2012), breaks = seq(1986,2012,4)) + 
  scale_y_continuous (labels = dollar, limits = c(0, 11), breaks = seq(0,11,1)) + 
  labs (x=NULL, y="$ in millions") + 
  scale_colour_manual(values = c(HOUSE_C1,SENATE_C1)) + 
  annotate("text", x = 1994, y = 1.5, label = "House", colour = HOUSE_C1, size = 4, fontface = 'bold') + 
  annotate("text", x = 1996, y = 7, label = "Senate", colour = SENATE_C1, size = 4, fontface = 'bold') 
ggplotly(p)
link <- plotly_POST(p, filename = "g3")
#layout(gg, dragmode = "pan")
#rangeslider(gg)

# Plot - Comparison of spending by House incumbents and challengers based on election outcomes, 1980 - 2012
# p <- ggplot(data = df_house_incum_total, aes(x = year, y = cost, fill = candidate)) +
#   geom_area()
# gg <- ggplotly(p)
# p <- streamgraph(data = df_house_incum, key = "candidate", value = "cost", date = "year", offset = "zero")

# Outcome: House incumbent wins with 60%+
p1 <- plot_ly(df_house_incum_win60plus, x = ~year, y = ~Incumbents, name = 'House Incumbents', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = HOUSE_C2) %>%
  add_trace(y = ~Challengers, name = 'Challengers', fillcolor = HOUSE_C1)  %>%
  layout(xaxis = ax, yaxis = ay, legend = list(x = 0, y = 0.95, font = list(family = 'serif', size = 11)), 
         annotations = title1)
# Outcome: House incumbent wins with <60%
p2 <- plot_ly(df_house_incum_win, x = ~year, y = ~Incumbents, name = 'Incumbents', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = HOUSE_C2, showlegend = FALSE) %>%
  add_trace(y = ~Challengers, name = 'Challengers', fillcolor = HOUSE_C1)  %>%
  layout(xaxis = ax, yaxis = ay2, annotations = title2)
# Outcome: House incumbent loses
p3 <- plot_ly(df_house_incum_loss, x = ~year, y = ~Incumbents, name = 'Incumbents', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = HOUSE_C2, showlegend = FALSE) %>%
  add_trace(y = ~Challengers, name = 'Challengers', fillcolor = HOUSE_C1)  %>%
  layout(xaxis = ax, yaxis = ay2, annotations = title3)
p <- subplot(p1, p2, p3, titleY = TRUE)
link <- plotly_POST(p, filename = "g4")

# Plot - Comparison of spending by Senate incumbents and challengers based on election outcomes, 1980 - 2012
# Outcome: Senate incumbent wins with 60%+
p1 <- plot_ly(df_senate_incum_win60plus, x = ~year, y = ~Incumbents, name = 'Senate Incumbents', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = SENATE_C2) %>%
  add_trace(y = ~Challengers, name = 'Challengers', fillcolor = SENATE_C1)  %>%
  layout(xaxis = ax, yaxis = ay, legend = list(x = 0, y = 0.95, font = list(family = 'serif', size = 11)), 
         annotations = title1)
# Outcome: House incumbent wins with <60%
p2 <- plot_ly(df_senate_incum_win, x = ~year, y = ~Incumbents, name = 'Incumbents', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = SENATE_C2, showlegend = FALSE) %>%
  add_trace(y = ~Challengers, name = 'Challengers', fillcolor = SENATE_C1)  %>%
  layout(xaxis = ax, yaxis = ay2, annotations = title2)
# Outcome: House incumbent loses
p3 <- plot_ly(df_senate_incum_loss, x = ~year, y = ~Incumbents, name = 'Incumbents', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = SENATE_C2, showlegend = FALSE) %>%
  add_trace(y = ~Challengers, name = 'Challengers', fillcolor = SENATE_C1)  %>%
  layout(xaxis = ax, yaxis = ay2, annotations = title3)
p <- subplot(p1, p2, p3, titleY = TRUE)
link <- plotly_POST(p, filename = "g5")

# Plot - House and Senate spending differentials by election outcomes, 1980 - 2012
p1 <- plot_ly(df_house_incumbents, x = ~year, y = ~diffw60, name = 'Win >=60%', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = HOUSE_C2) %>%
  add_trace(y = ~diffw, name = 'Win <60%', fillcolor = HOUSE_C1)  %>%
  add_trace(y = ~diffl, name = 'Lose', fillcolor = HOUSE_C3)  %>%
  layout(xaxis = ax, yaxis = ay, legend = list(x = 0, y = 0.95, font = list(family = 'serif', size = 11)), 
         annotations = title4)
p2 <- plot_ly(df_senate_incumbents, x = ~year, y = ~diffw60, name = 'Win >=60%', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = SENATE_C2) %>%
  add_trace(y = ~diffw, name = 'Win <60%', fillcolor = SENATE_C1)  %>%
  add_trace(y = ~diffl, name = 'Lose', fillcolor = SENATE_C3)  %>%
  layout(xaxis = ax, yaxis = ay2, annotations = title5)
p <- subplot(p1, p2, titleY = TRUE)
link <- plotly_POST(p, filename = "g6")

# Plot - Comparison of House and Senate independent expenditure spending by PACs for / against Democrats and Republicans, 1978-2012
p1 <- plot_ly(df_pac_house, x = ~year, y = ~for.repub, name = 'For Republicans', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = REPS_C2) %>%
  add_trace(y = ~against.repub, name = 'Against Republicans', fillcolor = REPS_C1)  %>%
  add_trace(y = ~for.dems, name = 'For Democrats', fillcolor = DEMS_C2)  %>%
  add_trace(y = ~against.dems, name = 'Against Democrats', fillcolor = DEMS_C1)  %>%
  layout(xaxis = ax, yaxis = ay, legend = list(x = 0.01, y = 0.1, font = list(family = 'serif', size = 11)), 
         annotations = title4)
p2 <- plot_ly(df_pac_senate, x = ~year, y = ~for.repub, name = 'For Republicans', type = 'scatter', mode = 'none', fill = 'tozeroy', fillcolor = REPS_C2, showlegend = FALSE) %>%
  add_trace(y = ~against.repub, name = 'Against Republicans', fillcolor = REPS_C1)  %>%
  add_trace(y = ~for.dems, name = 'For Democrats', fillcolor = DEMS_C2)  %>%
  add_trace(y = ~against.dems, name = 'Against Democrats', fillcolor = DEMS_C1)  %>%
  layout(xaxis = ax, yaxis = ay2, annotations = title5)
p <- subplot(p1, p2, titleY = TRUE)
link <- plotly_POST(p, filename = "g7")
#rangeslider(gg)


# Plot - Senate spending streamgraph
# df_pac_senate_stream <- subset(df_pac_senate, select=-congress)
# df_pac_senate_stream <- gather(df_pac_senate_stream, "type", "total", 2:5, factor_key=TRUE)
# df_pac_senate_stream %>%
#   streamgraph("type", "total", "year", interpolate="cardinal") %>%
#   sg_axis_x(1, "year", "%Y") %>%
#   sg_fill_brewer("PuOr")

# Plot - Ideological polarization in House and Senate, 1947 - 2013
p <- ggplot(data = df_polar_house, aes(x = Year, y = total, colour = type)) + 
  geom_line(size = 2, linetype = 'solid') + 
  theme_bw(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.position = "none"
  ) + 
  scale_x_continuous (limits = c(1945, 2015), breaks = seq(1945,2015,10)) + 
  scale_y_continuous (limits = c(-1, 1), breaks = seq(-1,1,0.25)) + 
  labs (x=NULL, y="avg. idealogical position") +
  scale_colour_manual(values = c(HOUSE_C1, DEMS_C3, REPS_C3)) + 
  annotate("text", x = 1960, y = 0.4, label = "Republicans", colour = REPS_C3, size = 4, fontface = 'bold') + 
  annotate("text", x = 1975, y = -0.4, label = "Democrats", colour = DEMS_C3, size = 4, fontface = 'bold') + 
  annotate("text", x = 2002, y = -0.2, label = "Entire House", colour = HOUSE_C1, size = 4, fontface = 'bold')
p1 <- ggplotly(p)
p <- ggplot(data = df_polar_senate, aes(x = Year, y = total, colour = type)) + 
  geom_line(size = 2, linetype = 'solid') + 
  theme_bw(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.position = "none"
  ) + 
  scale_x_continuous (limits = c(1945, 2015), breaks = seq(1945,2015,10)) + 
  scale_y_continuous (limits = c(-1, 1), breaks = seq(-1,1,0.25)) + 
  labs (x=NULL, y=NULL) +
  scale_colour_manual(values = c(SENATE_C1, DEMS_C3, REPS_C3)) + 
  annotate("text", x = 1960, y = 0.4, label = "Republicans", colour = REPS_C3, size = 4, fontface = 'bold') + 
  annotate("text", x = 1975, y = -0.45, label = "Democrats", colour = DEMS_C3, size = 4, fontface = 'bold') + 
  annotate("text", x = 2000, y = 0.15, label = "Entire Senate", colour = SENATE_C1, size = 4, fontface = 'bold')
p2 <- ggplotly(p)
p <- subplot(p1, p2, titleY = TRUE)
link <- plotly_POST(p, filename = "g8")

# Plot - Media trust by news sources and ideological position, 2014
p <- ggplot(data = df_media_trust, aes(reorder(Source, order), total)) + 
  geom_bar(stat = "identity", aes(fill = type), position = "fill") + 
  geom_text(size = 2, position = position_stack(vjust = 0.5), aes(label = paste0(round(total*100,1),"%")), colour = "white") + 
  theme_bw(base_family="serif") +
  theme (
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_line(colour = "black"),
    legend.position = "right", 
    legend.title = element_blank(),
    legend.direction = "vertical", 
    legend.justification = "left"
  ) + 
  scale_y_continuous (labels = percent, limits = c(0, 1.1), breaks = seq(0,1.1,0.2)) + 
  labs (x=NULL, y=NULL) +
  scale_fill_manual(values=c(DEMS_C1, DEMS_C3, "grey75", REPS_C3, REPS_C1)) +
  coord_flip()
p <- ggplotly(p)
link <- plotly_POST(p, filename = "g9")

# Plotly version (no good)
# p <- plot_ly(df_media_trust, x = ~df_media_trust$Consistently.liberal, y = ~df_media_trust$Source, type = 'bar', orientation = 'h',
#              marker = list(color = 'rgba(38, 24, 74, 0.8)',
#                            line = list(color = 'rgb(248, 248, 249)', width = 1))) %>%
#   add_trace(x = ~df_media_trust$Mostly.liberal, marker = list(color = 'rgba(71, 58, 131, 0.8)')) %>%
#   add_trace(x = ~df_media_trust$Mixed, marker = list(color = 'rgba(122, 120, 168, 0.8)')) %>%
#   add_trace(x = ~df_media_trust$Mostly.conservative, marker = list(color = 'rgba(164, 163, 204, 0.85)')) %>%
#   add_trace(x = ~df_media_trust$Consistently.conservative, marker = list(color = 'rgba(190, 192, 213, 1)')) %>%
#   layout(xaxis = list(title = "",
#                       showgrid = FALSE,
#                       showline = FALSE,
#                       showticklabels = FALSE,
#                       zeroline = FALSE,
#                       domain = c(0.15, 1)),
#          yaxis = list(title = "",
#                       showgrid = FALSE,
#                       showline = FALSE,
#                       showticklabels = FALSE,
#                       zeroline = FALSE),
#          barmode = 'stack',
#          paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
#          margin = list(l = 120, r = 10, t = 140, b = 80),
#          showlegend = FALSE) %>%
#   # labeling the y-axis
#   add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = df_media_trust$Source,
#                   xanchor = 'right',
#                   text = df_media_trust$Source,
#                   font = list(family = 'Arial', size = 12,
#                               color = 'rgb(67, 67, 67)'),
#                   showarrow = FALSE, align = 'right') %>%
#   # labeling the top legend
#   add_annotations(xref = 'x', yref = 'paper',
#                   x = c(25, 75, 125, 175, 225),
#                   y = 1.15,
#                   text = c('A', 'B', 'C', 'D', 'E'),
#                   font = list(family = 'Arial', size = 12,
#                               color = 'rgb(67, 67, 67)'),
#                   showarrow = FALSE)