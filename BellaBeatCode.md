BellaBeat Capstone Project
================
Justin White
2023-06-21

# Introduction

Bellabeat is a company that specializes in producing health-based
technology products for women. One of their products, the Leaf, can be
attached to common accessories like bracelets, necklaces, or clips. The
Leaf communicates health data to the user’s Bellabeat app so that the
user can monitor their health data. This product bears a lot of
similarity to the Fitbit, a smart watch that records health information
about the users while they wear it. This project explores some Fitbit
user data in order to find trends that Bellabeat can take advantage of
to improve product growth and user satisfaction.

The dataset used in this project is called “FitBit Fitness Tracker
Data”, posted to Kaggle by the user MÖBIUS. The data include calories
burned, levels of intensity, steps taken, heartrate, weight, and their
recorded times of the day/hour/minute. I will use the data to explain
how Bellabeat can adjust their app/Leaf communication and notifactions
to improve current user experience and make their service more
attractive to the broader market.

## Setting Up Data and Data Processing Tools

``` r
# Establish a CRAN mirror

options(repos=c(CRAN="https://mirror.las.iastate.edu/CRAN/"))

# Install tidyverse to help clean, filter, and manipulate data.
# Install lubridate to help clean and standardize the time data.
# Install ggplot2 to help create plots of relationships between the data.

install.packages("tidyverse")
```

    ## Installing package into '/Users/jayadmin/Library/R/x86_64/4.2/library'
    ## (as 'lib' is unspecified)

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/pd/bg10fw_s4cxf9sld37jlqctw0000gn/T//RtmpYl1zEv/downloaded_packages

``` r
install.packages("lubridate")
```

    ## Installing package into '/Users/jayadmin/Library/R/x86_64/4.2/library'
    ## (as 'lib' is unspecified)

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/pd/bg10fw_s4cxf9sld37jlqctw0000gn/T//RtmpYl1zEv/downloaded_packages

``` r
install.packages("ggplot2")
```

    ## Installing package into '/Users/jayadmin/Library/R/x86_64/4.2/library'
    ## (as 'lib' is unspecified)

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/pd/bg10fw_s4cxf9sld37jlqctw0000gn/T//RtmpYl1zEv/downloaded_packages

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.1     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(lubridate)
library(ggplot2)
```

``` r
# Change the working directory so that it is easier to read the csv files into dataframes

#setwd("/Users/Johnny Drama/Documents/CourseraGoogleDataAnalyticsCertification/Fitabase Data 4.12.16-5.12.16")
setwd("/Users/jayadmin/Documents/CourseraCapstoneProject/Fitbit_data")
#setwd('/kaggle/')

activity_daily_df <- read.csv("dailyActivity_merged.csv",header=TRUE)

calories_daily_df <- read.csv("dailyCalories_merged.csv",header=TRUE)
calories_hourly_df <- read.csv("hourlyCalories_merged.csv",header=TRUE)
calories_minute_narrow_df <- read.csv("minuteCaloriesNarrow_merged.csv",header=TRUE)
calories_minute_wide_df <- read.csv("minuteCaloriesWide_merged.csv",header=TRUE)

intensities_daily_df <- read.csv("dailyIntensities_merged.csv",header=TRUE)
intensities_hourly_df <- read.csv("hourlyIntensities_merged.csv",header=TRUE)
intensities_minute_narrow_df <- read.csv("minuteIntensitiesNarrow_merged.csv",header=TRUE)
intensities_minute_wide_df <- read.csv("minuteIntensitiesWide_merged.csv",header=TRUE)

steps_daily_df <- read.csv("dailySteps_merged.csv",header=TRUE)
steps_hourly_df <- read.csv("hourlySteps_merged.csv",header=TRUE)
steps_minute_narrow_df <- read.csv("minuteStepsNarrow_merged.csv",header=TRUE)
steps_minute_wide_df <- read.csv("minuteStepsWide_merged.csv",header=TRUE)

heartrate_seconds_df <- read.csv("heartrate_seconds_merged.csv",header=TRUE)

mets_minute_narrow_df <- read.csv("minuteMETsNarrow_merged.csv",header=TRUE)

sleep_day_df <- read.csv("sleepDay_merged.csv",header=TRUE)
sleep_minute_df <- read.csv("minuteSleep_merged.csv",header=TRUE)

weightlog_df <- read.csv("weightLogInfo_merged.csv",header=TRUE)
```

# Exploring Fitbit Data

## When Are People Most Active During the Day?

``` r
# Visualize how the users' intensities vary throughout the hours of an average day.
# Isolate the hour information of the ActivityHour column, then convert this into military time to make it easier to display in a bar chart.

intensities_hourly_df2 <- intensities_hourly_df %>% 
  separate(ActivityHour, into=c("date","time","am_pm"), sep=" ") %>% 
  separate(time, into=c("hr","min","sec"), sep=":") %>% 
  mutate(mil_shift = case_when(am_pm == "AM" & hr != "12" ~ 0, (am_pm == "PM" & hr != "12") ~ 12, (am_pm == "AM" & hr == "12") ~ -12, (am_pm == "PM" & hr == "12") ~ 0)) %>% 
  mutate(mil_time = as.numeric(hr)+mil_shift)

# Once the original data frame has been appropriately mutated so that each intensity corresponds with a particular military time from 0 to 23, I can group the data by military time and then create a summary data frame that includes the average intensity for each hour across all hourly intensity data.

summary_intensity_hr <- intensities_hourly_df2 %>% 
  group_by(mil_time) %>% 
  summarize(avg_intensity = mean(TotalIntensity))

# Create a bar chart that shows the average intensity of an hour as the height of the bar, while the x position of the bars tells you which hour of the day, from 0 - 23, the intensity is recorded in. Now add in a column that will write the hours of the day in am/pm notation which is more easily recognizable than 0-23.

x_hrs = c("12 am","1 am","2 am","3 am","4 am","5 am","6 am","7 am","8 am","9 am","10 am","11 am","12 pm","1 pm","2 pm","3 pm","4 pm","5 pm","6 pm","7 pm","8 pm","9 pm","10 pm","11 pm")

summary_intensity_hr$x_hrs <- x_hrs

ggplot(data = summary_intensity_hr) + 
  geom_bar(aes(x=fct_inorder(x_hrs),y=avg_intensity), stat = "identity") +
  labs(x = "Hour of Day", y = "Average User Intensity", title = "User Intensity vs Hours of the Day") +
  theme(axis.text.x = element_text(angle = 45))
```

![](BellaBeatCode_files/figure-gfm/user%20hourly%20intensities-1.png)<!-- -->

In this figure the hourly intensity is averaged over all users and
plotted against the hour of the day. What we see looks like an
approximately bimodal distribution where the modes appear to be around
12 pm and 6 pm. Identifying the most popular hours during the day in
which people are active is crucial for Bellabeat to understand. For
example, knowing that there are peak intensities around 12 pm and 6 pm
means they can focus sending reminders/notifications around those times
since they could be convenient times for people to be active already.
There is very little activity between 10 pm and 6 am (likely in large
part due to sleeping), so it would not behoove Bellabeat to notify users
during those times.

One important limitation of the dataset is that it does not indicate
whether users were all in the same time zone when collecting data, or if
the data shown was already converted to a common timezone. Analyzing
data from sources that include time zone information would be important
before drawing confident conclusions from this chart.

## Are People More Active During Certain Days of the Week?

``` r
# Create a data summary that shows average intensity levels of each type during each day of the week.
# Create new columns to include standardized time and day name so that we can see how the intensity varies throughout the week
# Group by day of the week and then create a summary table that shows the average value of movement for each day of the week

intensities_daily_df2 <- intensities_daily_df %>% 
  mutate(std_time = mdy(ActivityDay), day_name = wday(std_time)) %>%
  group_by(day_name) %>% 
  summarize(sedentary_p = mean(SedentaryMinutes),lightly_active_p = mean(LightlyActiveMinutes),fairly_active_p = mean(FairlyActiveMinutes),very_active_p = mean(VeryActiveMinutes))

# Create a stacked bar chart to show how much of each level of intensity the average person engages in each day
# Need to create a new data frame that matches up the calculated averages with labels for days of week and activity type

c1 = unlist(intensities_daily_df2[1,2:5],use.names = FALSE)
c2 = unlist(intensities_daily_df2[2,2:5],use.names = FALSE)
c3 = unlist(intensities_daily_df2[3,2:5],use.names = FALSE)
c4 = unlist(intensities_daily_df2[4,2:5],use.names = FALSE)
c5 = unlist(intensities_daily_df2[5,2:5],use.names = FALSE)
c6 = unlist(intensities_daily_df2[6,2:5],use.names = FALSE)
c7 = unlist(intensities_daily_df2[7,2:5],use.names = FALSE)

y1 = as.numeric(c(c1,c2,c3,c4,c5,c6,c7));

x1 = c(rep("Sunday",4),rep("Monday",4),rep("Tuesday",4),rep("Wednesday",4),rep("Thursday",4),rep("Friday",4),rep("Saturday",4));

fill1 = rep(c("Sedentary","Lightly Active","Fairly Active","Very Active"),7);

plot_df01 <- data.frame(x1, y1, fill1)

ggplot(data=plot_df01, aes(x = fct_inorder(x1),y = y1,fill = fct_inorder(fill1))) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Day of the Week", y = "Minutes of Activity", title = "Relative Spread of Activity Type Throughout Week", fill = "Activity Type")
```

![](BellaBeatCode_files/figure-gfm/intensity%20types%20throughout%20the%20week-1.png)<!-- -->

The stacked bar chart shows how the activity types vary throughout an
average week and how they vary relative to each other. The main takeaway
is that in an average week people have approximately the same
distribution of activity type every day. This information is very useful
because it shows that there are not particular days of the week where
Bellabeat should focus on notifying their users. Additionally, we can
see that people engage in light activity significantly more frequently
than fair/high activity.

## Relationshp Between Calories Burned, Steps Taken, Distance Traveled, and Percentage of Activity

``` r
# Inner join the calories and steps hourly data

calories_steps_hourly_df <- inner_join(calories_hourly_df, steps_hourly_df, by = c("Id", "ActivityHour")) %>% 
  filter(StepTotal != 0)

plot_df03 <- calories_steps_hourly_df[c(1:nrow(calories_steps_hourly_df)),c(3,4)]

ggplot(data = plot_df03, aes(x=StepTotal, y=Calories)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + 
  labs(x = "Steps Taken in One Hour", y = "Calories Burned in One Hour", title = "Correlation Between Steps Taken and Calories Burned")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](BellaBeatCode_files/figure-gfm/correlate%20calories%20burned%20with%20total%20distance%20traveled%20and%20percent%20active-1.png)<!-- -->

``` r
#

plot_df05 <- activity_daily_df[c(1:nrow(activity_daily_df)),c(4,15)] %>% 
  filter(TotalDistance != 0)

ggplot(data=plot_df05, aes(x=TotalDistance, y=Calories)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + 
  labs(x = "Distance Traveled in One Hour", y = "Calories Burned in One Hour", title = "Correlation Between Distance Traveled and Calories Burned")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](BellaBeatCode_files/figure-gfm/correlate%20calories%20burned%20with%20total%20distance%20traveled%20and%20percent%20active-2.png)<!-- -->

``` r
#

plot_df06 <- activity_daily_df %>% 
  mutate(percent_active = (LightlyActiveMinutes+FairlyActiveMinutes+VeryActiveMinutes)) %>% 
  filter(percent_active != 0)

ggplot(data=plot_df06, aes(x=percent_active, y=Calories)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + 
  labs(x = "Minutes of Daily Activity", y = "Calories Burned in One Day", title = "Correlation Between Minutes of Daily Activity and Calories Burned")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](BellaBeatCode_files/figure-gfm/correlate%20calories%20burned%20with%20total%20distance%20traveled%20and%20percent%20active-3.png)<!-- -->

In all three scatter plots, we see clear positive correlations between
steps taken, distance traveled, and amount of activity. Further analysis
could be done on the slopes of these lines of best fit to more
concretely demonstrate how changes in activity can affect calories
burned, a metric that many people look to on their health journeys.
Bellabeat can use that information to encourage users to increase their
steps/distance/minutes of activity and provide them a predicted increase
of calories burned if they can do so.

# Conclusions

Based on the analysis of this Fitbit data, my high-level recommendations
to Bellabeat are: focus their communications and notifications to their
users around 12 pm and 6pm (local user time) every day and provide users
quantifiable encouragement to improve on various health metrics (steps,
distance, amount of activity). Since activity appears to be evenly
spread throughout the week with peaks at 12 pm and 6 pm, increasing
communication at those times will be more likely to reach users when
they are already more active or have fewer obstacles in their day. It is
crucial for Bellabeat’s app to reach users when they are more active
because they will be more likely to interact with the activity features
of the app when they are actually in the mindset of being active.
Additionally, the positive correlations shown in my scatter plots
suggest that Bellabeat can give their users quantifiable recommendations
on increasing their activity in different ways. For example, if one line
of best fit suggested that increasing one’s minutes of daily activity by
15 mins would increase burned calories by 100 on average, then Bellabeat
could notify a user around 6pm that if they started engaging in activity
for even 15 mins then they could expect to lose 100 calories. I think
this could be a powerful incentivizer for users.
