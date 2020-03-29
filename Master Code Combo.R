
################################ ESM 206: Lab 1 ###########################################

#header------------------------------------------------------------------------------------------------------------

# ESM 206 Lab 1
# intro to projects, scripts, and basic wrangling
#Anne-Marie Parkinson

# add packages-----------------------------------------------------------------------------------------------------

library(tidyverse)

#add data----------------------------------------------------------------------------------------------------------
data <- read.csv(file.choose(), header=T)
data<- read_csv("hp_aggression.csv") #easier

#basic exploration-------------------------------------------------------------------------------------------------
names(data)
tail(data)
head(data)

#wrangling to select columns--------------------------------------------------------------------------------------

# dplyr::select() = for columns
# dplyr::filter() = for rows

#create subset where only keep data from 2 columns (character and book)
ex1 <- select(data, character, book) # if want more than 2 columns,just keep adding names separated by a comma

# operate goint to use alot = pipe operater whose sympol is: %>%  (do control+shift+m) 
# pipe operate means: do this thing AND THEN this (this %>% this)
ex2 <- data %>% select(character, book) # does same thing as ex1 code

# to select a range of columns, use : and to exclude a column put a - in front
ex3 <- data %>% select(abb:paragraphs, -book)

# dplyr::filter() to conditionally subset rows-----------------------------------------------------------------

#only keep observations (ie rows) where the book column matches the goblet of fire
ex4 <- data %>% filter(book == "The Goblet of Fire")
ex5 <- data %>% filter(character == "Harry", aggressions>2)

# or statements in filter: 1) add vertical line or 2) %in% (shortcut unknown) = look for matches to ANY del entries in this combined list of things
ex6 <- data %>% filter(character == "Harry" |character== "Voldemort" |character=="Peeves") # or statement option 1. Repeat same code too much
ex7 <- data %>% filter(character %in% c("Harry", "Voldemort", "Peeves")) # or statement option 2. Don't repeat same code. Code says "filter out observations in character column that match any entries in this (combined) list

#filter things numerically. ex/ keep observations in book deathly hallows with >5 aggressions
ex8 <- data %>% filter(book == "The Deathly Hallows", aggressions>5)

# determine most aggressive character (divide aggressions by # times mentioned)---------------------------------

#create new column with dplyr::mutate()
ex9 <- data %>% mutate(apm = aggressions/mentions) #apm= title of new column, aggressions/min=where the data is to come from

# use dplyr::group_by() and summarize() to find and report summary stats with defined groups---------------------

ex10 <- data %>% 
  group_by(abb) %>% 
  summarize(
    tot_agg = sum(aggressions),
    mean_agg =  mean(aggressions)
  )
# group_by does not visually change organization del data, but when use ex10 R will know that those groups exist and what data based on values in the groups
# summarize function makes new table with stats for grouped variables

# linking multiple steps together in a piped sequence -------------------------------------------------------

#keep observations for harry, hermine, voldemort, and severus 
#only seleect character, book, and mentions
#create summary table for each caharcters mentions across all books

ex11 <- data %>% 
  filter(character %in% c("Harry", "Hermione Granger", "Voldemort", "Severus Snape")) %>% 
  select (character, book, mentions) %>% 
  group_by(character) %>% 
  summarize(
    total_mentions=sum(mentions)
  )

# INTRO TO grpahs with ggplot2-------------------------------------------------------------------------------------

# 3 things to create a graph
# 1. ggplot
# 2. what data ur using
# 3. what type of graph making

ggplot(data=ex11, aes(x = character, y = total_mentions)) + 
  geom_col() +
  labs (x="Name", y="Mentions") +
  coord_flip()
#even though using ggplot2 function, its called just ggplot when using it. use aes (x=, y=) to name x an d y variables, and use + instead of %>% to link lines of the code. Use geom_type of graph to pick the type of graph
# coord flip used to flip orientation of the graph but keep () empty so the asiz labels switch with the graph
# DO NOT USE PIPER OPERATOR ( %>% ) IN GGPLOT

# scatterplot

ggplot(data=data, aes(x=mentions, y=aggressions)) +
  geom_point(aes(color=book)) +
  theme_minimal()
# graph shows that as a character is mentioned more, their recorded aggressions inc
#when want to color code points based on a feature, have to do it in geom_() as aes(...) bc your adjusting the aesthetic del already made graph. if just want to color the points a single color can just write color=__ NOT WITHIN aes()
#can add diff types of geom's as long as teyre compatiable 
#theme minimal affects background color and maybe other stuff idk yet. 

# histogram
ggplot (data=data, aes(x=aggressions)) +
  geom_histogram()
#would adjust things like bin width and color in geom_histogram paraenthesis 

#end of lab

############################################# ESM 206: Lab 2 #############################################

---
  title: "Lab 2"
author: "Amp"
date: "October 8, 2019"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

no hastags

# one # = large header
### 3 hastags= medium header
##### 5 hastags = small header
dont need hashtags in r markdown (when not in code chunk)

Make something *italicize* or **bold**
  
  make ^superscripts^ with ex 2^2^ or make ~subscripts~ ex 2~2~
  
  lists with bullet points (mkae sure theres a space between list title and the beginning of the lsit)

- item 1
- item 2
- item 3

for sub tabs, do 2 tabs in and make sure theres a space in the beginning of the list

1. or a numbered list
2. of things, can even have
a. sub tabs 
b. for lists
3. cool huh?
  
  
  
  make link can just copy and paste: https://www.yahoo.com/
  
  or just say heres the link to [yahoo](https://www.yahoo.com/)

heres an image of allisons dog being cute: 
  ![](teddy_utah.jpg)


### Add some code in r markdown
to make new code chunk (r markdown term) cna press the green +C button or alt+ctrl+i

attach packages 
```{r, warnings=FALSE} 
# need hastags for comments when in code chunk; its like coding in r
library(tidyverse)
library(janitor)
library(tidyr)

# code to keep errors and warnings from knit output didnt work
```


### read in data
world bank: environmental indicators
we're interested in CO~2~ emissions

Data source:
Date accessed:

```{r}
eb_env <- read_csv("wb_env.csv", skip=4) # data has unneeded file info, so to skip those, lines, 
#use skip=num rows to skip

country <-read_csv("country_list.csv") 

```
# use pivot_longer() to gather all the years together
```{r}

wb_tidy <- eb_env %>%  pivot_longer('1960':'2018', 
names_to="year", 
values_to = "value") # requires 3 parts: columns want to shift, ...?

class(wb_tidy$year) # asks what type of class this column is, turns out its a character


```
doing some cleaning an dwrangling. 

janitor::clean_names() to automatically convert ALL columns names to more R coder freindly names
```{r}
wb_df <- wb_tidy %>% clean_names() %>% 
select(-x64, -indicator_code, -country_code) %>% # - means remove this column 
mutate(year=as.numeric(year)) # updates year to numeric class


```

Let's check out a few things about the data. Like, what are the different indicators? What are the different countries listed? 
  
  We can use the unique() function: `unique(wb_df$country_name` or `unique(wb_df$indicator_name)` (run this in the Console, and ask yourself: why might I not want to put this in a code chunk that is then reported in my knitted document?).

## 5. More data wrangling

Let's say we're interested in finding the top 20 CO2 emitting countries (using indicator "CO2 emissions (kt)") from 2008 - 2018. 

To do that, let's: 

1. Make a subset that only contains data for "CO2 emissions (kt)" collected from 2008 - 2018
2. Group the data by Country Name
3. Find the sum of the CO2 emissions for each group
4. Make a nice plot of their total CO2 emissions from 2008 - 2018

now fina a summary statistic for total co2 emitted by country from 2008-2018

```{r}


wb_co2 <- wb_df %>% 
filter(indicator_name=="CO2 emissions (kt)") %>% 
filter(between(year, 2008, 2018)) %>% # using >=year and =< yeaar also works
group_by(country_name) %>% 
summarize(
total = sum(value, na.rm = TRUE)
) %>% 
arrange(-total)
```

But here we see a problem: the World Bank data, as downloaded, has a bunch of aggregated regions. We want to exclude those. 

To do that, there is another data frame we've read in called country_list, that only contains the names of the different countries (importantly, in the same format as they exist in our wb_co2 df). We will JOIN our data frames, in a way that only countries in wb_co2 that have a match in the 'country' column of country_list will still remain. 

But R will first look for columns with the *same name* to try to join dfs by - here, we have our countries in a column called "country_name" in wb_co2, but only called "country" in country_list. First, we'll update the column name to "country_name" in country_list so that there is a match for R to find when it tries to join them.

There is another option, to say "country_name" = "country" as an argument to join by.

```{r}
country_ls_new <- country %>% 
rename(country_name = country)

wb_join <- wb_co2 %>% 
inner_join(country_ls_new)

# Checking out what didn't match: 
  wb_antijoin <- wb_co2 %>% 
  dplyr::anti_join(country_ls_new)

# And what doesn't exist from the country list in our new wb_join df? What does this mean? These listed countries probably only had NA values in the original dataset. 
wb_antijoin2 <- country_ls_new %>% 
  anti_join(wb_join)
```

Now let's just find the top 20 CO2 emitters from 2000 - 2018

```{r}
co2_top20 <- wb_join %>% 
head(20)
```

What are some options? 
```{r}

ggplot(co2_top20, aes(x = country_name, y = total)) +
geom_col() + # GROSS! 
coord_flip() # Still so gross! 

# Why? We probably want this in order. And the bars are really bulky. Let's make a lollipop chart with our countries in order from highest to lowest for the top 50 to make this much better.

# Use forcats::fct_reorder() to change the order of levels

ggplot(co2_top20, aes(x = fct_reorder(country_name, total), y = total)) +
  geom_col() + 
  coord_flip()# Still ew! But at least this is in order? 

```

Now, let's make something actually nice: 

```{r}

ggplot(co2_top20, aes(y = fct_reorder(country_name, total), 
x = total)) +
geom_point(aes(color = total),
size = 3,
show.legend = FALSE) +
geom_segment(aes(x = 0, 
y = country_name, 
xend = total, 
yend = country_name,
color = total),
show.legend = FALSE
) +
theme_light() +
theme(panel.grid.minor.x = element_blank(), 
panel.grid.minor.y = element_blank()) +
scale_color_gradientn(colors = c("orange","magenta","blueviolet")) +
labs(x = expression(Total~CO[2]~emitted~(kilotons)~(2000-2018)), 
title = "Top 20 carbon dioxide emitting countries",
subtitle = "Total emitted, 2008 - 2018",
y = "") # Not sure if showing this...



```

Knit your document! Now it's all saved, and the fully reproducible code means that if I closed this project and reopened, all I'd have to do to recreate this beautiful graph is to run the code again. Yay! 

################################################### ESM 206: Lab 3 ############################################

### Part 1: Attach Packages

```{r}
library(here)
library(tidyverse)
library(janitor)
library(ggridges)
```
### Part 2: Add in Data (plain text file)
```{r}
sb_buoy <- readr::read_table(here::here("Raw_Data", "sb_buoy_2018.txt"), 
na=c("99", "999", "99.00", "999.0", "99.0")) %>% 
janitor::clean_names() %>% 
dplyr::slice(-1) %>%  # removes 1st row
select(number_yy:gst, atmp)

# here::here tells r where to look in order. first name in "" = first level (ie the sub folder in section 3 folder), 2nd name= name of the file. 
# use read_table for plain text files. 
# when data is not saved in same location as the R project file, then to tell R where to look. Here package is good for this. 
# data has multiple values for na data, so the c bind tells r to consider all these na. 
# slice is used to remove rows based on position


```

### Part 3: write sb_bouy to a csv file within the inermediate_data subfolder

```{r}

write_csv(sb_buoy, here("intermediate_data", "sb_buoy_cleaned.csv"))

#use write_csv to export data
#then can tell r where to export the data

```

### Part 4: exploring, and update "mm" month column to an ordered factor with month names

```{r}
#class(sb_buoy$mm) --> =character
# unique(sb_buoy$mm)

#make class of mm numeric nd pull month names from month.abb (which=sometihng already in R's memory)

sb_buoy_month <- sb_buoy %>%
  mutate(mm=as.numeric(mm)) %>% # says want mm colum to be rcognized as a numeric value 
  mutate(month_name=month.abb[mm]) #go through mm column, where ever see a 1, replace with 1st entry in month.abb vector/data, etc. #s 2-12

#class(sb_buoy_month$month_name) --> = character, so when plotted r will plot it alphabetically which we dont want for months. instead want r to recognize month_name as a factor with a specific order

sb_buoy_fct <- sb_buoy_month %>% 
  mutate(month_name= fct_relevel(month_name, levels=month.abb))


# fct_relevel = in this column (month name) i want the levels to be assoc with the values in this vector (month.abb)
# to mutate within the same column (dont create new column), the new column name (before equal sign) must be the same as the column name after the equal sign

## check: 
#class(sb_buoy_fct$month_name)
#levels(sb_buoy_fct$month_name)
```
### Part 5: Exploration and {purrr} intro

```{r}
ggplot(sb_buoy_fct, aes(x=month_name, y=atmp)) +
  geom_jitter()

# graph is not what monthly air temp values should look like on a graph, so theres something wrong with the data
```

now, for any columns stored as a character, i would like r to understand that they are numeric --> use purrr::modify_if() (purrr=in tidyverse)


```{r}
sb_buoy_num <- sb_buoy_fct %>% purrr::modify_if(is.character, as.numeric) # first entry= the argumentn, adn 2nd entry to what to do if a valule meets that condition, so here saying look at all columns and if r recognizes it as a character, make r recgonize it as numeric values
```

now lets look at the data again

```{r}
ggplot(sb_buoy_num, aes(x=month_name, y=atmp)) +
  geom_jitter(alpha=0.5) # alpha makes points transparent


# now graph looks normal

ggplot(sb_buoy_num, aes(x=month_name, y=atmp)) +
  geom_violin(aes(fill=month_name), 
              alpha=0.5,
              show.legend = F) + # remove legend bc it is redundant
  facet_wrap(~month_name) # plots each month indivisually, but make sit hard to compare

# violin plot is better than the jitter plot bc can see the dist of the data better (jitter had so many points that overlapped it was hard to see)
#facet_wrap says to make indi graphs based pn this varable
```

have monthly temps plotted separetly in facets but each of them overlayed on top of a pop histogram

```{r}

ggplot(sb_buoy_num, aes(x=atmp))+
  geom_histogram(data=transform(sb_buoy_num, month_name=NULL), fill="grey80") +
  geom_histogram(aes(fill=month_name), show.legend = F) +
  facet_wrap(~month_name) +
  theme_light()

#data=trasnform is saying within this data, make month_anme= unrecognizable JUST FOR THAT ONE LINE OF CODE (?) (wot affect other lines when reference month name), that way when try tofacet later, it wont separate the diff months into 12 diff graphs bc in this line of code

ggsave(here("Figures", "temp_hist.png")) # save the last ggplot in this folder with this name. Can pick from several file enders (jpg, png, pdf). if keep saving graph with same name and extension, it will be overrided with the most recent version

# no y ariable when ussing histogram
```

another option for comparing distribution with lots of observations is using ggridges

```{r}

ggplot(sb_buoy_num, aes(x=atmp, y=month_name)) +
  geom_density_ridges(aes(fill=month_name), show.legend=F) +
  scale_x_continuous(lim=c(5,25)) +# change values on x axis 
  scale_y_discrete(limits= rev(sb_buoy_num$ month_name)) # reverse order of the y axis values


ggsave(here("Figures", "temp_ridges.png"), height=6, width=6)

# this type of plot=good for have lots of observations, but wan to compare groups  (facet graph a litle harder to compare compared to the ridges grapah)
#when use scale, need to choose (ex) which axis, then which type of data you want to change (ie what type of data is the axis)

```

explore windspeed data! 
  
  ```{r}

ggplot(sb_buoy_num, aes(x=wdir, y=wspd)) +
  geom_hex()

#can see trends bc of the density plot, ,but not great way to display data (ex/ compass direction, 360 degrees)

```

now lets look at this with a polar coordinate system

```{r}
ggplot(data=sb_buoy_num, aes(x=wdir, y=wspd)) +
  geom_density_2d(size=0.2, aes(color=month_name), show.legend = F) +
  coord_polar() + # use this one line to change data to a more conceptual coord system, but have to add breaks (n, e, s, w) so an audience can better read it
  scale_x_continuous(breaks=c(0, 90, 180, 270), labels=c("N", "E", "S", "W"))+ # override existing break points, once have break points specified, can change the label names associated with them
  facet_wrap(~month_name) +
  theme_minimal()

#break points= tick marks on graphs
```

### Part 6: use dplyr::case_when() for long if-else statements 

```{r}
sb_buoy_season <- sb_buoy_num %>% 
  mutate(
    season=case_when(
      month_name %in% c("Mar", "Apr", "May") ~ "spring",
      month_name %in% c("Jun", "Jul", "Aug") ~ "summer", 
      month_name %in% c("Sep", "Oct", "Nov") ~ "fall",
      month_name %in% c("Dec", "Jan", "Feb") ~ "winter"
    )
  ) 

#season= name of new column; use mutate to create a new column
# what the case_when is saying: if month name contains anything within the combination of x,y,z,  then i want the new column to contain the word spring

```

############################################# ESM 206: Lab 4 interactive graphs ##################################################


```{r}
#attach packages

library(tidyverse)
library(plotly)
library(DT)
library(htmltools)


#change echo+t to echo=F in 1st r chunk so code checunks dont show up in knitted files in entire r markdown knitted file!

```

### A. Interactive grpahs of iris dimensions

using famos iris dataset, which is prebuilt into R

```{r, eval=F}

iris_graph <- ggplot(data=iris, aes(x=Petal.Width, y=Petal.Length)) +
  geom_point(aes(size=Sepal.Width, color=Species), alpha=0.5)

iris_graph

ggplotly(iris_graph) #plotly is used to create interactive grpahs/maps/etc

#highlight text and ctrl+shift+c so r will hashtag or unhastag the code

# in {r} add eval=F so r doesnt run this chunk of code 

```

## now make interactive function

```{r}

datatable(msleep)

```

##################################### ESM 206: Lab 4 some cool animals #######################################

##### A note on this project:

The organization of this document probably seems strange. That's because it's part of an entire multi-part lab for ESM 206, which students will create in multiple projects (but are combined here for easier sharing). Hence the weird unrelated pieces all put together in this project (students will create Parts 2 & 3 as separate projects).

[Link to the Lab 4 prompt](https://docs.google.com/document/d/1KX6_bKA8BOO0NdsUviVi0HCT-q9iTCtTYsS1dGNQJfU/edit?usp=sharing)

### Cool animal fun facts {.tabset .tabset-fade} 
```{r, inlculde=F}
#tabset makes tabs in knitted file. fade makes the tabs fadded (not needed)

```


#### Great white sharks 
```{r, inlcude=F}
# the num of hastags increased by 1, which tells r that this is one of the tab headers. Anything below the heder (but before the next header), is info that goes towards this tab.

# num of hastags for headers doesnt matter(?), so long as theyre consistent
```

![Great white at Seal Island, South Africa. Photographer: Chris Brunskill Ltd/Corbis News via Getty Images ](img/great_white.jpg) 
```{r, include=F}
# text in [] is the caption del photo
```

##### Some great white shark facts (from [NatGeo Kids](https://www.natgeokids.com/uk/discover/animals/sea-life/great-white-sharks/)):

- Great white sharks have ~ 300 teeth
- And swim way faster than you (25 mph)
- And are listed as vulnerable on the IUCN Red List

#### California condors

![Photo: Madison Roberts/Audubon Photography Awards](img/condor.jpg)

##### Some California condor facts (from [Animal Fact Guide](https://animalfactguide.com/animal-facts/california-condor/)):

- By 1987, there were only 10 California condors living in the wild
- They are the largest flying bird in North America
- Critically endangered on the IUCN Red List (> 400 today)

#### American pika

![Photo: J. MacKenzie / Pikaworks](img/pika.jpg)

##### Some American pika facts (from [OneKindPlanet.org](https://onekindplanet.org/animal/pika-american/)):

- Pika are of order *Lagomorpha* (which also includes rabbits)
- Pika live in high altitude talus slopes
- American pika are already disappearing from the Sierra Nevada


#### Ringtail Cats
![Photo: Courtesy of VA Zoo](img/ringtail_cat.jpg)

##### ringtail cats fun facts:

- can rotate teir feet 180^o^ to climb better
- closely related to racoons



```{r}
#img= file path; img is from the cloned file. 

# in Git tab, the files that appear are the ones that have had changes made to them, so those are the ones availble for committing

#alaways want to work on the most recent version of file, so click "pull" in git. IF the window says "already up to date", then tou have the mosr recent version. when working with other ppl, pull frequently. communicate with collaboratros so not working on the same script at the same time, bc there will be merge conoflicts

#git shows most recent file in the repository, but it saves all uploaded versions if go to history

#read mes in git follow same formating as r markdown
```


###

----------
  **Disclaimer:** This document is only for R Markdown & GitHub teaching purposes in ESM 206, Bren School of Environmental Science and Management (UCSB) 

####################################### ESM: Lab 5 ##################################################



```{r, include=F}

library(tidyverse)
library(here)
library(janitor)

lobster_abundance <- read_csv("lobster_abundance.csv", na="-99999") %>% 
  clean_names()


```

Use tidyr::uncount() function to convert the data form frequency format to case format

```{r}
lobster_tidy <- lobster_abundance %>% 
  tidyr::uncount(lobster_count)  # in () put in name of column with data you dont want counted (ex/ kinda the opposite of pivot_longer). The other observations will be repeated for the number of lobsters there are. ex/ here we dont want the number of lobsters bc it shouldnt be in its own row. for a value, its saying how many times that observation was seen. so if plot site by size, then will only get one point when there's actually 41. 

```

# exploratory data viz: Looking at data distribution

Only going to consider 'site' as our variable of interest

```{r}
#jitter
ggplot(lobster_tidy, aes(x=site, y=size_mm)) +
  geom_jitter(aes(color=site), alpha=0.5, width=0.2)

#histogram
ggplot(lobster_tidy, aes(x=size_mm)) +
  geom_histogram(aes(fill=site)) + #gives stacked histogram which is not useful for comparison
  facet_wrap(~site, scale="free")  #careful with scale=free bc can mislead audience if want to comapre graphs. but if dont care about comparing graphs (ie here where we just want to look at the dist ind of eahc other), then okay to have graphs with diff scales

# QQ plots

ggplot(lobster_tidy, aes(sample=size_mm)) +
  geom_qq() #looks at dist of all the observations across all sites. Looks pretty linear

ggplot(lobster_tidy, aes(sample=size_mm)) +
  geom_qq() +
  facet_wrap(~site)  # look at dist across the diff sites

#when interpreting qq plots, dont let a few outliers (esp when theres a small # of them relvative to the sample size) prevent you from determinng that the pop is not normal. All graphs below are normally distributed. slope doesnt matter, just looking at the linear-ness of the dist. If one of the sites was horribly skewed (def not normal), then can still use parametric test bc the others are normally dist, and thus most of the data (which has a large sample size) is normally dist

#ask if mean is good metric to use to compare samples. Maybe median is better. 

```

# Convert the 'date' column to class "Date"


Use the lubridate package to convery Date format and then to help easily parse month and year

```{r}

lobster_date <- lobster_tidy %>% 
  mutate(date_new= lubridate::mdy(date)) #mdy=tells r the order of the dates, theres other options to choose from

#`

#can typically do the parse dates in one r chunk. typically when read in data
```

Now lets parse year and month into separate columns 

```{r}

lobster_parse_date <- lobster_date %>% 
  mutate(
    obs_month=lubridate::month(date_new, label=T),
    obs_year=lubridate::year(date_new)) # use new date column (which is in a format that r understands) to create the new column. label =T means when parse the data, assign it the actual month name, not just the month number

#check class: class(lobster_parse-date$obs_month)  to see if column is an ordered factor, then levels(lobster_parse-date$obs_month) to check the order of the observations in that column

```


Find counts of observed lobsters based on diff heirarchical groupings

Couple ways to get counts. Count will group data, get te count, and ungroup data

```{r}
# first: count lobsters by year and month

lobster_ym <- lobster_parse_date %>% 
  dplyr::count(obs_year, obs_month)  #first tell r which columns you want to group by


lobster_yr <- lobster_parse_date %>% 
  dplyr::count(obs_year)


lobster_site <- lobster_parse_date %>% 
  dplyr::count(site)
```


create summary table that creates stats other than count by group, its easier to use group_by() + n()

```{r}
lobster_summary <- lobster_parse_date %>% 
  group_by(site) %>% 
  summarize(
    lobster_number=n(),   #get count, but doing it in the groupby, summarize funciton lets you find other statistics/summary table
    mean_size=mean(size_mm, na.rm=T),
    sd_size=sd(size_mm, na.rm=T)
  )

```


# Find confidence intervals

use t.test() function, which is in the basic r stats package, to find CI (for one sample) and perform t tests to compare means of 2 sampless

```{r}

ivee_lobster <- lobster_tidy %>% 
  dplyr::filter(site=="IVEE") %>% 
  pull(size_mm) # pull creates a vector instead of a data table

t.test(ivee_lobster)

#look at if true mean is equal to 0 (null hpy: lobster size at ivee=0)

#95% CI means if i took many more samples from this population, then i expect the pops to fall within this pop mean (72.99-74.12) 95% of the time?

```

# two sample t test to compare means

null: no diff btw pop means
null=defeault hyp

is there a sig diff in lobster lengths at naples and mohawk reefs. ttest does not tell if one is sig larger or smaller than the other, just if theyre sig different

we've  done the neceassry exploratory analyses (lookad at dist, large sample size) to det a 2 sample t test for means comparison is appropriate

```{r}

napl_sample <- lobster_tidy %>% 
filter(site=="NAPL") %>% 
pull(size_mm)

mohk_sample <- lobster_tidy %>% 
filter(site=="MOHK") %>% 
pull(size_mm)

mn_ttest <- t.test(mohk_sample, napl_sample)
mn_ttest

```
There is a sig difference in lobster lengths  btw naples and mohawk reef. 

Allison: signicance should be the least interesting thing in a description. Explain what the actual means are, the data distribution. 

```{r}
lobster_mn <- lobster_tidy %>% 
filter(site %in% c("NAPL", "MOHK"))

mn_test2 <- t.test(size_mm ~site, data=lobster_mn)

mn_test2

#several parts of the ttest ourput. can call them seperatly
mn_test2$p.value
mn_test2$statistic
mn_test2$parameter

```

Here is the t-statistic: `r mn_test2$p.value`  

use ` code ` (little ``` under ~~ to write code in rmkdown and have the value/output of that ocde the in the knitted file)

#Now a heatmap

```{r}
lobster_ys <- lobster_parse_date %>% 
count(obs_year, site)

ggplot (data=lobster_ys, aes(x=obs_year, y=site)) +
geom_tile(aes(fill=n))


```
lobster counts increaseing in IVEE

###################################### ESM 206: Lab 6 - lab 5 t_text review ################################


```{r}
library(tidyverse)

#review of previous lab (lab 5)

```

```{r}
ggplot() +
geom_histogram(data=beaver1, aes(x=temp), fill="red") +
geom_histogram(data=beaver2, aes(x=temp), fill="green", alpha=0.5)

# dist is mostly normal for beaver 1, but looks bimodal for beaver 2. when dist isnt normal, i know the sampling dist (the means if taken multiple times) would be normally distributed bc of the central limit theorom, so can compare means with parametric test



```

is there a significant difference btw mean body temp for beaver 1 and 2? --> Use t-test. 
Not asking if mean body temp is greater than or less than the other group, just want to know if theyre different, so do 2 tailed test (no directionality implied so use 2 tailed test)

```{r}
my_ttest <- t.test(beaver1$temp, beaver2$temp)
my_ttest

#default is assume ...variance is not normal? the same? 

#p value means its likely, but its very very very unlikely to be drawn from populations with the same mean. so have sufficient evicence we have pops  
```
Mean body temperatures for beaver 1 (`r my_ttest$estimate[1], 2`)

(means in my teest results, just pull the means (which in t.test means are stored in "estimate"), and within this parameter, only pull ...?). Make sure to use  backwards `  in the code. 2 afterwards refers to only report 2 digits after the decimal point
Never just report p value. its the least interesting part. alsways include other stats like means of a group

#################################### ESM 206: Lab 6 ###########################################


#Why create functions?

- they make it easy to do a test/repeat a code over and over without copy and pasting the code
- can take a complicated piece of code and give it a useful, descriptive name. 

#Making a basic function

make a function to take a number and add 1 to it

```{r}

add_one <- function (x) { y <- x + 1}   #functions function is to make functions
#y and x have are recognized by within the function add_one, but is not recognized outside of the function
# in this case, dont need "y <- ". function still works without it. 

z <- add_one(x=4) 
zz <- add_one(3:7) #means, for numbers 3-7 apply the funcion (ie add 1 to 3, 4, 5, 6, and 7)
#when theres only one variable in the function, dont necessrily need "x=...". but with more complicated functions def need. 

add_some <- function (x, a=1) {y <- x + a} 

z <- add_some(x=4)
zz <- add_some(x=4, a=3)


add_some <- function (x, a=1) {
y <- x + a
print("yay")  #dont need commas like with ggplot, but do make sure there are indents and that code is on diff lines than the {}
return(y)
} 

z <- add_some(x=4)
zz <- add_some(x=4, a=3)

```

#Make a useful function!

good practice= to use verb anmes fro functions. 

Lets amek a function to calculate the mean of a vector of values. First, lets crete a test vector of valueswe can use for testing. well ue `rnorm()` to generate a pseudo-random set of numbers as a working sample eith a known mean adn SD. 


```{r}
set.seed(12345) #r doesnt generate random numbers, so it randomly picks a location along a set of numbers. This locations changes each time the code is run. Set seed makes sure that we all start ar the same spot every time and we all get the same sample. 

test_vec <- rnorm(n=100, mean=1, sd=1)
head(test_vec)
mean(test_vec)
sd(test_vec)


#calculate a mean manually
calc_mean <- function (test_vec) {
sample_size <- length(test_vec) 
my_mean <-  sum(test_vec)/sample_size
return(my_mean)
}

calc_mean(test_vec)

#length is used to determien the sample size. can write out that n=100 but better to code incase we change the sample size. Good practices for reproducability

```
OR

calc_mean <- function (x) {
sample_size <- length(x) 
my_mean <-  sum(x)/sample_size
return(x)
}

calc_mean(x)


#Anatomy of a function:

- `function()` is a function that defines functions 
- inside the arguments for `function(...)` are the information we need to give the function for it to do its job
- after the arguments, inside the { }, we put the code that defines what the function does.
- at the end, the `return()` gives back to the user the desired output of the function

# Environments

- Most of the time we're working in the Global Environment. when excucuting a function, r starts a new ind env separate from the global en. the arguments (in this case, x) allow a windo through which we can pass information to the new env; the return() is a window to pass information back
- Note that the changes in variables inside the function are NOT communicated back to the global env, which is a good thing
- A well written fucntino should only communicate with hte global env through the arguments and return values. 
- once the function is finished, that new env/universe returns its value then disappears forever.

```{r}
x <- 100
y <- 200

z <- add_one(x=8) #z=9, not 108. 

x #x is still 100, not 108. 

#what happened in the function does not affect the global env. 

```

# Improving our basic functions

A well written fcuntion should anticipate when the user might have imperfect data. error checks or additional arguments can be helpful. when might out calc_mean() functino break down? 
  
  ```{r}
calc_mean(1:10) # works
calc_mean(test_vec=1:10) #works
calc_mean(x=1:10) #doesnt work. variabel we use in the function is test_vec, not x. 
calc_mean(y=1:10) #doent work
calc_mean() # doesnt work
calc_mean(x=c(1,2,3,4, NA)) #doesnt work. na can ruin calcualtions
```

#Lets ccreate a test vector with some NAs

also, lets learn about indexing to access specific elements in a vector. use "index" numbers to tell which elements of the vector to change

```{r}
test_vec_w_na <- test_vec

test_vec_w_na[5] <- NA # setting element #5 into a NA (number in 5th postion in the list now=NA)
test_vec_w_na[10:15] <- NA #turn elements 10-15 into NAs
test_vec_w_na[c(2,22,24)] <- NA

calc_mean(test_vec_w_na)

```

what do we need to do to make out function work with NAs?
  
  - design in an na.rm argument to allow user to decide whether to exclude NAs
- design in a way to exclude NAs if the user wants


# Another quick thing with vectors: How to exclude NAs

```{r}

z <- c(1:5, NA)
z
is.na(z) #asks if each value in a function is considered NA (T or F)
!is.na(z) # ! reverses the function. Asks what IS NOT considered an NA
zz <- z[!is.na(z)] # another way to filter. saying, within this function/group of numbers, only keep values that ARE NOT NA values. (! means ARE NOT)
zz

calc_mean2 <- function(x_vec, na.rm=T) {
  if (na.rm == T ){
    x_vec <- x_vec[!is.na(x_vec)]  #code to allow user to include ot exclude NA values. in this case remove em
  }
  n <- length(x_vec)
  mymean <- sum(x_vec)/n
  return(mymean)
}

calc_mean2(test_vec_w_na)  

```



```{r}

data(iris)

iris_mean <- iris %>% 
  janitor::clean_names() %>% 
  group_by(species) %>% 
  summarize(mean_sepal_w=calc_mean2(sepal_width))

```


#Why use loops?

loops are to do things over and over again (ie iterate), esp if you hve a vector of values and want to change osmething based on a value

```{r}
times <-  10 

for (i in 1:10) {
  print(i+1)
}

#says for numbers 1-10, add 1 to each number

```

```{r}

x <- 1

for (j in 1:times) {
  x <- x + j 
  print(x)
}


```

```{r}
#for (x in ...something about hello world
```

A standard die roll has equal chances to land on each of its six sides. use sample() to simulate this

```{r}


die_rolls <- data.frame(rolls=sample(x=1:6, size=1000, replace=T))  #x= what values to include in the data set to choose from. size= how many times to pick from the sample, but is limited by the sample size. so once pick a number, cant pick it again. so if sample size=7 get error bc want to pick from the sample 7 times but only ahve 6 numbers to pick from. so use replace=T so the picked numbers get returned to the sample

#plot die rolls

ggplot(die_rolls, aes(x=rolls)) + 
  geom_histogram(bins=6) 

# gaph should change each time run the die rolls function

```

now lets use a loop to make a bunch of simulations, each of 20 rolls, and calc mean of each

```{r}
set.seed(1234)
sins <- 1000

mean_results <- vector('numeric', length=sins)

for (i in 1:sins) {
  rolls_vec <- sample(1:6, size=20, replace=T)
  rolls_mean <- calc_mean2(rolls_vec)
  mean_results[i] <- rolls_mean
}

results_data_frame <- data.frame(sin=1:sins, mean=mean_results) # in column 1 (which is named sin) have the observation values equal to 1:sins whcih is the number of samples (1-1000), for column 2 (which is named mean) have the observations equl to mean)results)

ggplot(results_data_frame, aes(x=mean)) +
  geom_histogram(alpha=0.8, fill="red") +
  theme_minimal()

```

while the true mean of die rolls should be 3.5, the actual means of our samples are not = 3.5, BUT these are samples and the sampling distribution is normal bc of the central limit theorm

####################################### ESM 206: Lab 7 ##############################################

### Work through the following before lecture on Wednesday 11/13

#### Attach packages

Note: you probably need to install `effsize` and `kableExtra`
```{r}
library(tidyverse)
library(here)
library(janitor)
library("effsize") # you probably need to install this
library('kableExtra') # you probably need to install this
```

### Paired and one-sided t-tests

In class and labs so far, we've been doing two-sided t-tests (no directionality implied) to compare means based on two samples. We have been using *unpaired* data: data collected from two groups between which we have no reason to think that one observation in one group is associated with one (and only one) observation in the other group. 

For example: If we're comparing mean urchin sizes at two sites, there's no reason to think that each urchin at site A is associated with a single urchin at site B. The data are *unpaired.* 

In contrast, what if I have been monitoring the same 45 people to study the effects of a trial drug on blood pressure. Then we ask: Is there a significant difference in blood pressure before and after the drug trial? In that case, it doesn't make sense to compare Subject 4's "before" blood pressure to Subject 15's "after" blood pressure - we'd want to compare blood pressures within subjects since they have different baselines to begin with. When that is the case, our data are *paired*, and we'd compare means using a *paired* two-sample t-test. 

We have also been doing *two-sided* t-tests, by asking "is there a difference in means" but *not* implying directionality. If we instead ask "Is mean of A *greater* than mean of B", then we'd use a *one-sided* t-test to account for directionality.

In this take-home lab, you'll learn to do both by specifying new arguments in the `t.test()` function.

#### Background on Flint water data

To practice paired and one-sided t-tests, we'll use data collected from 271 residences in Flint, MI to explore the effect of collection methods on observed lead concentrations (ppb).  

**Background:** For a year and a half (summer 2014 - December 2015), city officials in Flint, MI directed residents to flush their faucets for 3 - 4 minutes before collecting samples for lead & copper corrosion testing. The guidelines were highly criticized by the EPA, as flushing could reduce Pb measured concentrations and imply lower exposure than residents were actually experiencing (more: [KJ Pieper et al. 2018](https://pubs.acs.org/doi/full/10.1021/acs.est.8b00791). 

For more Flint residential testing and blood lead levels data: [michigan.gov](https://www.michigan.gov/flintwater/0,6092,7-345-76292_76294_76297---,00.html) 

#### Read in and explore the Flint water data: 

```{r}
flint_pb <- read_csv(here::here("flint_water.csv")) %>% 
clean_names() %>% 
rename(pb_immediate = pb_bottle_1_ppb_first_draw, 
pb_2min = pb_bottle_2_ppb_2_mins_flushing) 
```

We are asked: is there a significant difference between lead concentrations immediately sampled and after 2 minutes of flushing? 

First, we'll look at the distributions & sample sizes:
  
  - With 271 observations each (note: there are 5 NA values in each sample), we have enough to say that by Central Limit Theorem we know the sampling distribution of means will be normal regardless of underlying population, so comparing means using a  t-test is OK if I think means is a useful metric of comparison.

- We should still always LOOK at the data, anyway: 
  
  ```{r}

# Histograms:
ggplot(flint_pb, aes(x = pb_immediate)) +
  geom_histogram()

ggplot(flint_pb, aes(x = pb_2min)) +
  geom_histogram()

# QQ plots:
ggplot(flint_pb, aes(sample = pb_immediate)) +
  geom_qq()

ggplot(flint_pb, aes(sample = pb_2min)) +
  geom_qq()

# They are NOT normally distributed; we'll still say we want to compare means, and can do that comfortably because of CLT (sampling distribution of the means will be normal).

```

We should also look at some statistics for comparison: 
  
  ```{r}

# Sample size (non-NA):
n_immediate <- sum(!is.na(flint_pb$pb_immediate))
n_flushed <- sum(!is.na(flint_pb$pb_2min))

# Means (immediate v. 2 min flushing):
mean_immediate <- mean(flint_pb$pb_immediate, na.rm = TRUE)
mean_flushed <- mean(flint_pb$pb_2min, na.rm = TRUE)

# Medians (immediate v. 2 min flushing):
median_immediate <- median(flint_pb$pb_immediate, na.rm = TRUE)
median_flushed <- median(flint_pb$pb_2min, na.rm = TRUE)

# Standard deviations (immediate v. 2 min flushing):
sd_immediate <- sd(flint_pb$pb_immediate, na.rm = TRUE)
sd_flushed <- sd(flint_pb$pb_2min, na.rm = TRUE)

# Lead concentrations measured in immediately collected samples are much higher than those in water collected after 2 min flushing. 
```

Return the values for the sample sizes and summary statistics in the code chunk above to see them in the Console. How do the sample means of lead concentration compare for immediate vs. 2-min flushed water samples?
  
  #### Are observations paired or unpaired? 
  
  These observations are reported for each *household*. Does it make sense to compare the "immediate" and "2 min flushing" observations across different households? 
  
  **No.** It makes sense to recognize that when we compare values, we should be comparing the immediate and post-flushing lead concentration differences at each house (e.g., each observation in the 'immediate' sample is associated with one and only one observation in the '2 min flushing' sample). 

When that is the case, data are called **paired**, and we will perform a **paired t-test** to answer: "Is there a significant difference in lead concentration in immediately collected tap, and in water after 2-min of flushing?"

**Null hypothesis:** The mean lead concentration is the same in water collected immediatetely, and water collected after 2-min flushing. 

**Alternative hypothesis:** The mean lead concentration is NOT the same in water collected immediatetely, and water collected after 2-min flushing. 

#### Two-sample, two-sided **paired** t-test:

To answer that question, we'll perform a two-sided, two-sample paired t-test. Breaking it down:

- **Two-sided** because we're not asking "Is A greater than B" or is "B less than A", we're just asking if they're different in either direction

- **Two-sample** because we're comparing means of two samples

- **Paired** because each observation in sample A is associated with one and only one observation in sample B

Perform the test by inputting the sample vectors, and adding argument `paired = TRUE`:

```{r}
my_flint_test <- t.test(flint_pb$pb_immediate, flint_pb$pb_2min, paired = TRUE)

```

Look at the results for `my_flint_test`. Think about the outcome, and decide based on the *p*-value whether you have enough evidence to reject the null hypothesis that the mean lead concentration in immediate and 2-min flushing samples is the same. 

**Example statement of test outcome:** 

"Mean lead concentration (ppb) measured in immediately collected water samples (`r round(mean_immediate, 2)` $\pm$ `r round(sd_immediate, 2)`, n = `r n_immediate`) differed significantly from lead in water collected after 2 minutes of flushing (`r round(mean_flushed, 2)` $\pm$ `r round(sd_flushed, 2)`, n = `r n_flushed`) by a paired two-sample t-test (t(`r round(my_flint_test$parameter, 2)`) = `r round(my_flint_test$statistic, 2)`, *p* < 0.001)."

**Note:** Usually when a p-value is really tiny, *p* < 0.001 is sufficient. But ask: Why is the way I added that in the statement above *not* best practice for reproducibility? 

#### Two-sample, ONE-sided **paired** t-test:

What if our question isn't "do the means differ," but instead "are mean lead concentrations in water after 2-min flushing *LESS* than in immediately sampled water?"

Then we are implying directionality, and would want to perform a one-sided test. We add directionality to `t.test()` by including the argument `alternative = "greater"` or `alternative = "less"`, depending on the order that we add our samples in the function. 

If I want to test: Is mean of A *greater* than mean of B? Then my code would be: 
  
  `t.test(A, B, alternative = "greater")`

If I want to test: Is the mean of B *less* than mean of A? (note that that is the same as the question above, just asked differently) Then my code would be:
  
  `t.test(B, A, alternative = "less")`

So be careful of the order of inputs when you're doing a one-tailed t-test! 

Our question is: "Are mean lead concentrations in water after 2-min flushing *LESS* than in immediately sampled water?"

**Null hypothesis:** The mean lead concentration in flushed samples *is not lower* than the mean for immediately sampled water.

**Alternative hypothesis:** The mean lead concentration in flushed samples *is* lower than the mean for immediately sampled water. 

Perform a one-sided, two-sample paired t-test:
```{r}
flushed_less_ttest <- t.test(flint_pb$pb_immediate, 
flint_pb$pb_2min, 
paired = TRUE, 
alternative = "less")
```

Check out the results for `flushed_less_ttest`.

On your own based on the results, **write a final statement using in-line referencing**.

### Cohen's *d* effect size

Remember, [the *p*-value is not enough](https://www.jgme.org/doi/full/10.4300/JGME-D-12-00156.1). Here, we'll use Cohen's *d* effect size to report a more meaningful metric of differences between group means. 

Recall, Cohen's *d* effecet size is calculated by: 

$$d = \frac{Mean_{Group A} - Mean_{GroupB}}{SD_{pooled}}$$

where the pooled standard deviation for both groups, SD~pooled~ is calculated by: 

$$SD_{pooled}=\sqrt{\frac{SD_{GroupA}^2 + SD_{GroupB}^2}{2}}$$

We could write our own function to calculate the effect size (see Casey's materials for Lab 6!) as follows: 
  
  ```{r}

# Creating a function called 'calc_d' to calculate Cohen's d effect size
# Here, a and b will be our sample vectors 

calc_d <- function(a, b) {
  sd_pooled <- sqrt((sd(a, na.rm = TRUE)^2 + sd(b, na.rm = TRUE)^2)/2) # SD pooled
  x <- (mean(a, na.rm = TRUE) - mean(b, na.rm = TRUE))/sd_pooled # Complete equation
  return(x)
}

# Then apply that function to our samples: 

flint_d_myfunction <- calc_d(flint_pb$pb_immediate, flint_pb$pb_2min)
# d = 0.41 (moderate effect size)
```

Or we could use the existing `effsize::cohen.d()` function instead of creating our own. Let's use it here to check that results match for our function and the `cohen.d` function: 

```{r}
flint_d <- effsize::cohen.d(flint_pb$pb_immediate, flint_pb$pb_2min, na.rm = TRUE)
# Same returned! Cohen's d = 0.41 (moderate effect size)
```

Then in our report, we would want to include the actual means of the samples, and the effect size, possibly the confidence interval for each, and then the *least interesting thing* shoud be the statement of significance associate with our t-test. 

### Making a table with kableExtra

**Note**: There are like a million ways to make a table when knitting to html from R Markdown (not all are compatible when knitting to Word or PDF, but you don't have to worry about that for now). Some packages that are helpful for producing customized tables are:

Here's an example using `kableextra` (see more examples  [here](https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html) and [here](https://rpubs.com/yutao/444395)).
                                                                                               
                                                                                               I'm going to use a subset from the built-in R datas 'trees' to create an example finalized table. 

First, check out the original data frame with `View(trees)`. 

**Note**: There are like a million ways to make a table when knitting to html from R Markdown (not all are compatible when knitting to Word or PDF, but you don't have to worry about that for now). Some packages that are helpful for producing customized tables are:
  - `DT`
- `kable`
- `kableExtra`
- `flextable`
- `gt`

Now, make a subset then create a table with `kableExtra` (to see what each line adds, run line-by-line and with & without different arguments):
```{r}

# Make the subset (keep only rows 1 - 5):
trees_sub <- trees %>% 
  dplyr::slice(1:5)

# Make the table of the subset:
trees_sub %>% 
  kable(col.names = c("Diameter (inches)", 
                     "Height (ft)", 
                     "Volume (cubic feet)")
        ) %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = F,
                position = "left"
                ) %>% 
  add_header_above(c("Black cherry tree metrics" = 3))
  

```

...and many other customization options!

### END LAB 7

                                                                                               
                                                                                           
                                                                                               
############################## ESM 206: Lab 8 #############################################


# add packages

library(tidyverse)
library(car)
library(janitor)
library(ggbeeswarm)
library(kableExtra)

# load data

penguins <- read_csv("penguins.csv") %>% 
  clean_names()

```

### Find counts of penguins by sex and species

```{r}
penguin_counts <- penguins %>% 
  count(species, sex)
```

### Compare flipper sizes for female penguins across 3 species

```{r}
# sub data: keep only female observations
penguin_f <- penguins %>% 
  filter (sex == "FEMALE")

# look at data spread!!!
ggplot (penguin_f, aes(x=flipper_length_mm)) +
  geom_histogram() +
  facet_wrap(~species)

ggplot (penguin_f, aes(sample=flipper_length_mm)) +  # change x= to sample=
  geom_qq() +
  facet_wrap(~species)
# qq plot: look at linearity of each species indivisually. 

#even if histogram and/or qqplot did not appear normal, then we can assume by the central limit theorm the sampling dist is normla (bc smaple size is large enough (>30))

```

### Find summary statistics

```{r}
penguin_f_summary <- penguin_f %>% 
  group_by(species) %>% 
  summarize(
    mean_flipper_length = mean(flipper_length_mm),
    sd_flipper_length = sd(flipper_length_mm),
    sample_size = n(),
    se_flipper_length = sd(flipper_length_mm)/sqrt(n()),
    var_flipper_length = var(flipper_length_mm)
  )

# visually determine if variances are equal (enough) to assume homogenaity of variance: if largest group vairance is less than 4x the other group variances then can assume normality? but should still conduct statstest to test variances 

leveneTest(flipper_length_mm ~ species, data=penguin_f)

# null: variances are equal (p > 0.5) --> for this example, retain null
# alt: variances are not equal (p < 0.5)


```

Ive done exploratory analysis, and all of these species combined are making me think a one way anova is an appropriate wat to compare means btw species. (meet assumptions to use anova)

QUESTION: if levenes test has p < 0.5, but the actual variances aren't 4x less than the highest variance, what do then?


### Make beeswarm plot with the mean and sd overlayed on top of it

3 most common error bars:

- SD
- SE
- CI

```{r}
ggplot () +
geom_beeswarm(data=penguin_f, aes(x=species, y=flipper_length_mm), 
size=1.2, 
color="pink") +
scale_x_discrete(labels=c("Adelie", 'Chinstrap', 'Gentoo')) +
labs(x="\nSpecies", y= "Flipper Length (mm)\n") +
geom_point(data=penguin_f_summary, aes(x=species, y=mean_flipper_length), 
size=1.2, 
color="blue") +
geom_errorbar(data=penguin_f_summary, aes(x=species, 
ymin = mean_flipper_length - sd_flipper_length,
ymax = mean_flipper_length + sd_flipper_length),
width=0.1,
color="blue") +
theme_bw()



# combine data from diff data frames into one graph, when do that check that the order of the groups is IN THE SAME ORDER!!
# geom error bar requires you to manually determine how far to extend the error bars (vs excel inputs them randomly with no true reasoning)
# geom error bar default width=1, which is ridiculous, so have to manually change
# NOTE: when showing summary stats, make sure you explain WHAT you're showing
```


** Figure 1.** Flipper length (mm) for three female penguin species (a,b,c) observaed at Palmer Station LTER, Antarctica. Pink points indicate indi flipper length, blue dots indicate group mean, error bars indicate $\pm$ 1 standard deviation. Data source: (enter name of data source and link to it)



NOTE: when doing tests and showing spread, make sure youre consistent with what youre testing and what your summary stats show (ex/ if looking at diff btw means, graphs should show spread around means, not medians!)


### one-way anova

Is there a sig diff in mean flipper length for female penguins btw the 3 species (a,b,c)

```{r}
#anova
penguin_aov <- aov(flipper_length_mm ~ species, data=penguin_f)

summary(penguin_aov)

# large F could mean large btw group variance compared to small within group var, so large F indicates its unlikely the groups are drawn from pops with same mean. (tihnking exercise, if within group spread increases (while btw group stays the same), then f value gets smaller and  value gets larger so more likely pops are drawn from groups wit the same mean)

# null: means btw groups are equal
# alt: all means are not equal, at least 2 means are diff

# post hoc test to determine which species flipper lengths are sig diff 
TukeyHSD(penguin_aov)

#lwr/upr= realted to CI for that group. 
```
Conclusion: one way anova with post-hoc Tukey's HSD reveals  sig diff in mean flipper length across all species (gentoo m=, chinstrap mean=, adelie mean=)(F(df) = ...., p < 0.001 for all pairwise comparisons). 

NOTE; this is suprising result considering how much overlap btw sd error bars in the beeswarm graph (for adile and chinstrp), but if graph se of flipper length instead of sd then VERY diff graph and we woundlt question the anova results cb it would confirm our assumptions. thats why reporting p value is not enough. should also report % diff btw groups, effect size, etc.


### make nicer tables with proportions
```{r}
#data, filter then count
party_relig <- gss_cat %>% 
filter (partyid %in% c("Strong republican", "Strong democrat")) %>% 
filter (relig %in% c ("None", "Catholic")) %>% 
count(partyid, relig)

# create contigency table

party_relig_table <- party_relig %>% 
pivot_wider(names_from=relig, values_from=n)



```

### creare proportions using janitor::adorn_*

```{r}
party_relig_prop <- party_relig_table %>% 
adorn_percentages(denominator = "row") %>% 
adorn_pct_formatting(digits=1)  %>%   # this code converts proportions to percentages AND limits num digits after decimal point 
adorn_ns(position = "front")    # this code includes the percentages/proportions AND the original values

kable(party_relig_prop) %>% 
kable_styling()
```

couples ways can ask a question looking at the relationship bte religous affiliation and political affillication: 

-Are political and religious affiliation ind from each other?
-is there a sig assoc btw political affiliation and religous affiliation?
-is there a sig effect of political assoc (strong republican s strong democrat) on religous affiliation (none, catholic)? --> be very careful with wording. don want to imply anything
-is there a sig diff in religious affil btw strong republicans and strong democrats?
```{r}
# make a contigency table that only contains the actual COUNTS

chi_counts <- party_relig_table %>% 
select (-partyid)   # remove qualitative column


# chi squre test 

# null: there is no diff btw groups --> there is no sig associ btw party affiliation and political affiliation, or there is no sig diff btw the 2 patry groups ( R, D), or party and religous affil are ind. These sound diff, but theyre all saying the same thing. 

my_party_chi <- chisq.test(chi_counts)

my_party_chi
```
there is a sig diff in religious affiliation (none, catholic) btw strong republicans and strong democrats ($\chi^2$ = ..., p < 0.001)....

there is no sig assoc btw party is and religious affil

party affil ...


########################################### ESM 206: Lab 9 ############################################

library(tidyverse)
library(janitor)
library(kableExtra)
library(ggpubr)
library(broom)
```

### 1. Objectives

- One more chi-square, with visualization
- Example of rank-based tests for rank / medians comparison (Mann-Whitney U)
- Simple linear regression by OLS in R
- Check assumptions (diagnostic plots)
- Visualize linear model, and summarize in text
- Calculate Pearson's *r* 
  - Make predictions for new values


### 2. Chi-square, revisited:

First, we'll make some mock data (suppose we ask residents of California and Colorado if they skiied at least once last season): 
```{r}
df <- data.frame(state = c("California", "Oregon", "Washington", "Colorado"), 
yes = c(54, 130, 67, 85), 
no = c(102, 115, 95, 71))
df

```

**We ask:** Does proportions of residents who went skiing last season differ significantly between Californians, Oregonians, Washingtonians, and Coloradans? 

Let's look at a table of proportions (remember `janitor::adorn_()`):
  
  ```{r}
df_prop <- df %>% 
  janitor::adorn_percentages(denominator = "row") %>% 
  janitor::adorn_pct_formatting(digits = 2) %>% 
  janitor::adorn_ns(position = "front")

df_prop
```

So yeah, it looks like we might expect to find a significant difference. Let's run chi-square to see: 

```{r}
# Remember, first get a contingency table of ONLY counts
# Last week we just removed the first column
# Here, we use column_to_rownames to just make the first column a rowname instead
df_ct <- df %>% 
column_to_rownames('state')

# Notice how it changes:
df_ct

# Then we run chi-square:
ski_chi <- chisq.test(df_ct)

# And to see the results: 
ski_chi
```

Since p < 0.05, we retain the alternative hypothesis that skiing and state are not independent. In other words: "There are significant differences in skiing habits (yes / no last season) based on state of residence ($\chi$^2^(`r ski_chi$parameter`) = `r round(ski_chi$statistic,2)`, *p* < 0.001)."

Our follow-up question should be: *What's driving the significant association?*
  
  ```{r}

# test to determine which groups have a sig diff --> so posthock version of anova for a chi-sq
ski_chi$stdres

```

Standardized residuals > |2| indicates strong divergence from null hypothesis scenario**
  
  --> so All but Washington are significantly different from expected if there were truly no significant differences in proportions. 

Then we can also follow-up by doing pairwise chi-square comparisons (just compare two states at a time):
  
  
  ### 3. A rank-based test example (Mann-Whitney U to compare unpaired ranks)
  
  In lecture we discussed non-parametric rank-based alternatives to some of the hypothesis tests we've been doing to compare means. Those were: 

- Mann-Whitney U to compare ranks (medians) between two unpaired samples
- Wilcoxon Signed-Rank to compare ranks (medians) between two paired samples
- Kruskall-Wallis to compare ranks (medians) between > 2 samples

As an example, let's make some mock "unpaired" data and perform Mann-Whitney U using the `wilcox.test()` function (you'd also use this for a paired Wilcoxon-Signed-Rank test, with an additional 'paired = TRUE' argument). 
                                                                                                                   
                                                                                                                   First, create two samples `gp_1` and `gp_2`:
                                                                                                                   
                                                                                                                   ```{r}
                                                                                                                   # Remember, use set.seed() to create a "pseudorandom" sample
                                                                                                                   # sample.int: creates random samples from 1 - n, of size = ?, with replacement: 
                                                                                                                   
                                                                                                                   set.seed(1414)
                                                                                                                   gp_1 <- sample.int(20, size = 15, replace = TRUE)
                                                                                                                   
                                                                                                                   set.seed(1424)
                                                                                                                   gp_2 <- sample.int(30, size = 15, replace = TRUE)
                                                                                                                   ```
                                                                                                                   
                                                                                                                   For example, imagine that those are rankings that people give a candidate on a scale from 1 - 30, and we want to know if there is a significant difference. 
                                                                                                                   
                                                                                                                   We ask: Is there a significant difference in ranks (medians) between the two groups?
                                                                                                                   
                                                                                                                   Here, we'll perform Mann-Whitney U: 
                                                                                                                     
                                                                                                                     ```{r}
                                                                                                                   # Is there a significant difference in ranks (medians)?
                                                                                                                   my_mwu <- wilcox.test(gp_1, gp_2)
                                                                                                                   
                                                                                                                   my_mwu
                                                                                                                   # No significant difference in rank (median) 
                                                                                                                   # If data are PAIRED, add argument 'paired = TRUE'
                                                                                                                   ```
                                                                                                                   
                                                                                                                   
                                                                                                                   Though not doing it today, see `kruskal.test` for more information about a rank-based test for comparing medians across > 2 groups. 
                                                                                                                   
                                                                                                                   ### 4. Simple linear regression
                                                                                                                   
                                                                                                                   We'll exploring the relationship between two continuous variables, using the `iris` dataset 
                                                                                                                   
                                                                                                                   Here, we'll explore petal length vs. petal width.
                                                                                                                   
                                                                                                                   #### A. Look at it: 
                                                                                                                   
                                                                                                                   Always. This should always be the first thing. 
                                                                                                                   ```{r}
                                                                                                                   # Exploratory (we'll make a final plot later)
                                                                                                                   ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) +
                                                                                                                     geom_point(aes(color = Species))
                                                                                                                   ```
                                                                                                                   
                                                                                                                   
                                                                                                                   And ask: 
                                                                                                                     
                                                                                                                     - Does it look like a linear relationship makes sense?
                                                                                                                     - Do we have any concerns about modeling as a linear relationship?
                                                                                                                     - Any outliers?
                                                                                                                     - Initial thoughts about homoscedasticity (explored more later)? 
                                                                                                                     
                                                                                                                     Here, it looks like overall a linear relationship between petal length and petal width makes sense (although a case could be made that the smaller group, which contains all obserations for setosa irises, should be modeled separately). 
                                                                                                                   
                                                                                                                   #### B. Model it
                                                                                                                   
                                                                                                                   Once we've decided that a linear relationship makes sense, we'll model it using `lm()`. 
                                                                                                                   
                                                                                                                   Note that we haven't checked all assumptions yet. That's because a lot of our assumptions for linear regression are based on model *residuals* (e.g. normality & homoscedasticity of residuals), which we can't calculate until after we find the predicted values from the model ($residual = y_{actual} - y_{predicted}$). 
                                                                                                                   
                                                                                                                   So make the model first: 
                                                                                                                   ```{r}
                                                                                                                   iris_lm <- lm(Petal.Width ~ Petal.Length, data = iris)
                                                                                                                   summary(iris_lm)
                                                                                                                   
                                                                                                                   # Look in lecture notes to see what the different pieces of the output are telling us. 
                                                                                                                   # Are the individual coefficients significantly different from zero?
                                                                                                                   # Is the model overall significant?
                                                                                                                   
                                                                                                                   # For comparison, forcing y-intercept to zero: 
                                                                                                                   iris_lm_2 <- lm(Petal.Width ~ Petal.Length + 0, data = iris)
                                                                                                                   summary(iris_lm_2)
                                                                                                                   ```
                                                                                                                   
                                                                                                                   Let's say we pick the first model (`lm_1`): 
                                                                                                                     
                                                                                                                     - The slope is `r iris_lm$coefficient[2]` 
                                                                                                                   - The y-intercept is `r iris_lm$coefficient[1]`
                                                                                                                   
                                                                                                                   But trying to get all of the statistical information from the `summary()` function would be kind of a mess. We can also use `broom::tidy()` to get the model outputs in nice data frame format: 
                                                                                                                     
                                                                                                                     ```{r}
                                                                                                                   iris_lm_tidy <- broom::tidy(iris_lm)
                                                                                                                   iris_lm_tidy
                                                                                                                   
                                                                                                                   # Get the intercept: 
                                                                                                                   petal_int <- iris_lm_tidy$estimate[1]
                                                                                                                   petal_int
                                                                                                                   
                                                                                                                   # Then to get the Petal.Length coefficient:
                                                                                                                   petal_coef <- iris_lm_tidy$estimate[2]
                                                                                                                   petal_coef
                                                                                                                   
                                                                                                                   # To get whatever pieces you want by indexing. 
                                                                                                                   ```
                                                                                                                   
                                                                                                                   
                                                                                                                   
                                                                                                                   What about getting some other model information (degrees of freedom, F-statistic, p-value, etc.)?
                                                                                                                     
                                                                                                                     Easier to use `broom::glance()` - but make sure to have the development version of `{broom}` package (so that DFs align with `summary.lm()`). 
                                                                                                                   
                                                                                                                   ```{r}
                                                                                                                   # Metrics at a glance: 
                                                                                                                   lm_out <- broom::glance(iris_lm)
                                                                                                                   lm_out
                                                                                                                   ```
                                                                                                                   
                                                                                                                   We can use the results of both to write a statement about the model: 
                                                                                                                     
                                                                                                                     Simple linear regression was used to explore the relationship between iris petal length (cm) and petal width (cm). A significant regression equation was found ($\beta$ = `r round(petal_coef,3)`, F(`r lm_out$df`,`r lm_out$df.residual`) = `r round(lm_out$statistic,1)`, p < 0.001) with an R^2^ of `r round(lm_out$r.squared,3)`. 
                                                                                                                   
                                                                                                                   #### C. Predictions & SEs at a glance (for existing values in df)
                                                                                                                   
                                                                                                                   To find the predicted values and residuals for iris petal width at each existing petal length in the data frame, we can use `broom::augment(iris_lm)`:
                                                                                                                     
                                                                                                                     ```{r}
                                                                                                                   lm_fitted <- broom::augment(iris_lm)
                                                                                                                   lm_fitted
                                                                                                                   ```
                                                                                                                   Notice that within `lm_fitted`, there are also a bunch of other things - like residuals, standardized residuals, etc. We'll explore those things a bit more next: 
                                                                                                                   
                                                                                                                   
                                                                                                                   #### D. Explore assumptions
                                                                                                                   
                                                                                                                   Let's use this information from `lm_fitted` to manually evaluate some assumptions. 
                                                                                                                   
                                                                                                                   - Linearly related variables (CHECK - already looked & thought hard)
                                                                                                                   - Normally distributed residuals
                                                                                                                   - Homoscedasticity (constant residuals variance)
                                                                                                                   - iid residuals (no serial correlation) - more often a concern in time series data
                                                                                                                   
                                                                                                                   We'll explore a few here.
                                                                                                                   
                                                                                                                   1. Residuals distribution. 
                                                                                                                   
                                                                                                                   A major assumption of linear regression is that the residuals are normally distributed. The residuals for our model (y~actual~ - y~predicted~) are stored in the `$.resid` column from the `broom::augment()` function. 
                                                                                                                   
                                                                                                                   Here, we create a histogram and QQ plot of the residuals: 
                                                                                                                   
                                                                                                                   ```{r}
                                                                                                                   ggplot(data = lm_fitted, aes(x = .resid)) +
                                                                                                                   geom_histogram()
                                                                                                                   
                                                                                                                   ggplot(data = lm_fitted, aes(sample = .resid)) +
                                                                                                                   geom_qq()
                                                                                                                   ```
                                                                                                                   
                                                                                                                   
                                                                                                                   Check! These overall look pretty normally distributed. 
                                                                                                                   
                                                                                                                   We could also do a formal test for normality (e.g. Shapiro-Wilke, Anderson-Darling, etc.), but remember - those formal hypothesis tests for normality are often more an indication of sample size than a meaningful divergence from normality, especially as sample size increases). 
                                                                                                                   
                                                                                                                   2. Homoscedasticity
                                                                                                                   
                                                                                                                   The assumption of homoscedasticity means that we assume relatively constant variance of residuals. Does it look like the variance (spread) of residuals changes over the span of the model? 
                                                                                                                   
                                                                                                                   Here, we'll look at the residuals (actual, and standardized) over the course of the fitted values to see if the spread of the residuals is changing notably:
                                                                                                                     
                                                                                                                     ```{r}
                                                                                                                   ggplot(data = lm_fitted, aes(x = .fitted, y = .resid)) +
                                                                                                                     geom_point()
                                                                                                                   
                                                                                                                   ggplot(data = lm_fitted, aes(x = .fitted, y = .std.resid)) +
                                                                                                                     geom_point()
                                                                                                                   ```
                                                                                                                   
                                                                                                                   
                                                                                                                   There, we might say "Well yeah...it looks like at larger values of the fitted model, we see greater spread of the residuals." But also try not to be distracted by a relatively small number of observations. Here, there are only about ~10 observations that look like they are leading to the larger residuals variance, and even those aren't much different from the spread that exists at lower values in the fitted model (e.g. |0.5| vs. |0.3|). So here, heteroscedasticity isn't a big concern. 
                                                                                                                   
                                                                                                                   Also remember, violations of homoscedasticity may lead to wrongly large or small *errors* associated with coefficients, but will not affect the model estimates (coefficients) themselves.  
                                                                                                                   
                                                                                                                   So the graphs we made manually above help us to explore the assumptions of linear regression (residuals normally distributed, ~ constant variance, and a linear model makes sense to describe the overall relationship between petal length and petal width). 
                                                                                                                   
                                                                                                                   An alternate approach to make those graphs (that you're more likely to use, and is fine): Just get the diagnostic plots using `plot(model_name)`:
                                                                                                                                                               
                                                                                                                                                               ```{r}
                                                                                                                                                               plot(iris_lm)
                                                                                                                                                               ```
                                                                                                                                                               
                                                                                                                                                               Notice that four plots show up. What do they show? 
                                                                                                                                                               
                                                                                                                                                               - **The first one**: fitted values vs. residuals - the same that we made manually to explore homoscedasticity
                                                                                                                                                               - **The second one**: QQ-plot for residuals - the same that we made manually to explore normality of residuals
                                                                                                                                                               - **The third one**: another way of looking at fitted vs. residuals (these are just standardized residuals, but you can interpret it the same way)
                                                                                                                                                               - **The fourth one**: Cook's distance, a measure of "influence" or "leverage" that individual points have on the model - often considered a way to explore outliers...
                                                                                                                                                               
                                                                                                                                                               So let's think a bit about Cook's distance (Cook's *d*): a measure of how "influential" a point is in the model output.
                                                                                                                                                                                                           
                                                                                                                                                                                                           #### E. Cook's Distance (observation influence / leverage)
                                                                                                                                                                                                           
                                                                                                                                                                                                           Some guidelines: 
                                                                                                                                                                                                             
                                                                                                                                                                                                             - If Cook's D is greater than $\frac{4}{n}$ for any observation, where *n* is the number of observations used to create the model, then that observation is strongly influential. This does NOT mean you should just remove that observation. In fact, you should plan on leaving all observations in unless you have really good reason not to. 
                                                                                                                                                                                                           
                                                                                                                                                                                                           - In our example (iris), *n* = 150, so the threshold for a second look is 4/150. Let's make & store that as a variable here:
                                                                                                                                                                                                             
                                                                                                                                                                                                             ```{r}
                                                                                                                                                                                                           cook_limit <- as.numeric(4 / count(iris)) 
                                                                                                                                                                                                           ```
                                                                                                                                                                                                           
                                                                                                                                                                                                           
                                                                                                                                                                                                           Let's take a look at the Cook's distance for each of our observations, which is also included in `lm_fitted` as `.cooksd`:
                                                                                                                                                                                                             
                                                                                                                                                                                                             ```{r}
                                                                                                                                                                                                           # Note: here, we convert the numbered row names to actual values to be plotted on the x-axis, then plot Cook's d for each observation as a column height
                                                                                                                                                                                                           
                                                                                                                                                                                                           ggplot(data = lm_fitted, aes(x = as.numeric(rownames(lm_fitted)), y = .cooksd)) +
                                                                                                                                                                                                             geom_col() +
                                                                                                                                                                                                             geom_hline(yintercept = cook_limit,
                                                                                                                                                                                                                        color = "red",
                                                                                                                                                                                                                        linetype = "dashed")
                                                                                                                                                                                                           ```
                                                                                                                                                                                                           
                                                                                                                                                                                                           We can see that there are a number (though relatively small, ~ 12 of 150 observations) of observations that we might want to take a second look at. But Cook's distance should **NOT** be a binary decision making tool to decide whether or not a value should be considered an outlier, or excluded from a dataset. 
                                                                                                                                                                                                           
                                                                                                                                                                                                           You should have a very compelling reason to believe that an observation is not representative of the population that you are trying to study (e.g. measurement error, mislabeled sample, etc.) before you even consider removing it. Your default should be to keep everything. How different a point is from the others is not a good enough reason to remove it from your analyses. 
                                                                                                                                                                                                           
                                                                                                                                                                                                           #### F. Visualize the model
                                                                                                                                                                                                           
                                                                                                                                                                                                           Now that we've explore the assumptions and have decided that linear regression is a valid tool to describe the relationship between petal length and petal width, let's look at the model.
                                                                                                                                                                                                           
                                                                                                                                                                                                           - Use `geom_smooth(method = "lm")` to add a linear model to an existing scatterplot. 
                                                                                                                                                                                                           - Use `stat_cor()` and `stat_regline_equation` to add equation information directly to the plot panel, at an x- and y-position that you specify
                                                                                                                                                                                                           
                                                                                                                                                                                                           ```{r}
                                                                                                                                                                                                           ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) +
                                                                                                                                                                                                           geom_point(size = 2) +
                                                                                                                                                                                                           geom_smooth(method = "lm",
                                                                                                                                                                                                           color = "red",
                                                                                                                                                                                                           size = 0.5,
                                                                                                                                                                                                           fill = "gray10",
                                                                                                                                                                                                           alpha = 0.5) +
                                                                                                                                                                                                           theme_light() +
                                                                                                                                                                                                           ggpubr::stat_cor(label.x = 5, label.y = 0.4) +
                                                                                                                                                                                                           ggpubr::stat_regline_equation(label.x = 5, label.y = 0.6)
                                                                                                                                                                                                           ```
                                                                                                                                                                                                           
                                                                                                                                                                                                           #### G. Make predictions at new points
                                                                                                                                                                                                           
                                                                                                                                                                                                           Often, we'll want to use a model to make predictions for **new** values of the predictor variable.
                                                                                                                                                                                                           
                                                                                                                                                                                                           Let's make a data frame of new petal lengths, then feed that into the `iris_lm` model to make predictions for petal width. 
                                                                                                                                                                                                           
                                                                                                                                                                                                           **Note**: Notice that here, I'm creating a variable called `Petal.Length` within new_df. That's important because the model `iris_lm` was built using a predictor variable called `Petal.Length`, so when we feed the model new data it is going to look for a variable *also called Petal.Length*. 
                                                                                                                                                                                                           
                                                                                                                                                                                                           ```{r}
                                                                                                                                                                                                           # Make a new data frame (in this case, containing only a single column called Petal.Length):
                                                                                                                                                                                                           new_df <- data.frame(Petal.Length = c(2.5, 3, 3.5))
                                                                                                                                                                                                           
                                                                                                                                                                                                           # Look at new_df to confirm:
                                                                                                                                                                                                           new_df
                                                                                                                                                                                                           ```
                                                                                                                                                                                                           
                                                                                                                                                                                                           Now, give that new data for Petal.Length (in new_df) to the model to make predictions, using the `predict()` function: 
                                                                                                                                                                                                           ```{r}
                                                                                                                                                                                                           
                                                                                                                                                                                                           predicted_values <- predict(iris_lm, newdata = new_df)
                                                                                                                                                                                                           
                                                                                                                                                                                                           # Put the predictions together with the inputs if you want: 
                                                                                                                                                                                                           new_predict <- data.frame(new_df, predicted_values)
                                                                                                                                                                                                           new_predict
                                                                                                                                                                                                           
                                                                                                                                                                                                           ```
                                                                                                                                                                                                           
                                                                                                                                                                                                           
                                                                                                                                                                                                           #### H. Find Pearson's *r* for correlation: 
                                                                                                                                                                                                             
                                                                                                                                                                                                             In lecture we talked about the coefficient of determination, R^2^, which tells us how much of the variance in the dependent variable is explained by the model. 
                                                                                                                                                                                                           
                                                                                                                                                                                                           We might also want to explore the strength of the correlation (degree of relationship) between two variables which, for two linearly related continuous variables, can be expressed using Pearson's *r*. 

Pearson's *r* ranges in value from -1 (perfectly negatively correlated - as one variable increases the other decreases) to 1 (perfectly positively correlated - as one variable increases the other increases). A correlation of 0 means that there is no degree of relationship between the two variables. 
                                                                                                                                                                                                           
                                                                                                                                                                                                           Typical guidelines look something like this (there's wiggle room in there): 

- *r* = 0: no correlation
- *r* < |0.3|: weak correlation
- *r* between |0.3| and |0.7|: moderate correlation
- *r* > |0.7|: strong correlation

We'll use the `cor.test()` function, adding the two vectors (Petal.Length and Petal.Width from iris) as the arguments. The function reports the Pearson's *r* value, and performs a hypothesis test with null hypothesis that the correlation = 0. 

```{r}
my_cor <- cor.test(iris$Petal.Length, iris$Petal.Width)
# r = 0.9629 (strong positive correlation)
# Correlation is significantly non-zero
```

Here, we see that there is a strong positive correlation between petal length and petal width (*r* = `r round(my_cor$estimate,2)`, t(`r my_cor$parameter`) = `r round(my_cor$statistic,2)`, p < 0.001). 

# END LAB



############################## ESM 244: Lab 1 ##############################################
```{r}
library(tidyverse)
library(here)  
library(janitor) 
library(kableExtra)
```

## Load Data
```{r}
us_landings <- read_csv(here("data", "noaa_fisheries.csv")) 
```
one columnn has dollar signs in each column/row, which R will view as a character. Any column with one symbol will be interpreted as a character 

## Make data tidy

```{r}
landings_tidy <- us_landings %>% 
  clean_names() %>% 
  mutate(state = str_to_lower(state),
         afs_name = str_to_lower(afs_name),
         dollars_num = parse_number(dollars_usd)) 

#first mutate code says: create new colum called state which is a string to lower version of the original state column (ie the original data values were state names in all caps, this code converts the letters to all lower case which is more code friendly)
# parse number: drops any non-numeric value in a cell before or after the first number. Good for cells with money/dollar signs (ex/ $1000 --> 1000)
```

lets get some info on salmon

```{r}
salmon_landings <- landings_tidy %>% 
  mutate(afs_clean = str_remove(afs_name, pattern = "aggregate")) # this means: in this column, remove this word/phase from all cells in this column. Doesnt remove observations, just removes a string from them

salmon_landings <- salmon_landings %>% 
  filter (str_detect(afs_clean, pattern = "salmon")) %>%  # usu we filter by finding an exact match to whatever is in " ". Now we want to filter for a partial match
  separate(afs_clean, into = c("group", "species"), sep = ",") # for columns with multiple pieces of data, this splits that data up into their own columns while keeping the original column. The into=c() is to give names to the new columns. sep is to tell R what to look for to split the data. In this case, im saying after a comma (,) split the data after that comma and before the comma into their own rows
```

Find some grouped summary data

Find annual total us alndings and dollar value (summing across all states) for each TYPE of salmon using group_by and summarize

```{r}
salmon_summary <- salmon_landings %>% 
  group_by(year, species) %>% 
  summarize(
    tot_landings = sum(landings_pounds),
    tot_value = sum(dollars_num)
  )
```


Graph!
  
  ```{r}
salmon_landings_graph <- ggplot(salmon_summary, aes(x= year, y=tot_landings)) +
  geom_line(aes(color=species))

ggsave(plot - salmon_landings_graph, 
       here('figures', "us_salmon_ah.png"),
       height=5,
       width=8) # defalt is 7x7, so can customize our saved iamge size. 
```



Make nice kable table:
  
  head() - saves the top n lines
tail () - saves last n lines

```{r}

salmno_first5 <- salmon_summary %>% 
  head(5)

kable(salmno_first5) #too lazy to finish. 

 ###################### ESM 244: Lab 2 #######################################

```{r}
library(tidyverse)
library(janitor)
library(naniar)
library(VIM)
library(skimr)
library(ggfortify)
library(here)
```

## Load Data
```{r}

ca_pb <- read_csv(here("data", "ca_pollution_burden.csv"))

ca_dem <- read_csv(here("data", "ca_census_demographics_2010.csv"))

```

## Clean and Wrangle Data
```{r}

#pollution data --------------------------------------------------

ca_pb_nopct <- ca_pb %>%
  clean_names() %>% 
  select (-contains("perc")) %>%    # code says: remove columns whose column header contains the string/phrase...
  select (-contains("pctl")) %>% 
  select (-latitude, -longitude)

# demographic data -----------------------------------------------

ca_dem_clean <- ca_dem %>% 
  clean_names()
```


## Pollution burden indicators only

```{r}
#subdata

ca_pb_subset <- ca_pb_nopct %>% 
  select(ozone:solid_waste, asthma:housing_burden)

```

## PCA of pollution burnden indicators

```{r, eval = FALSE}

pb_pca <- prcomp(ca_pb_subset, scale = T)
#get error bc missing values

```


## Exploring missingness in data

Couple ways to do this

```{r}

# method 1: 
summary(ca_pb_subset) # summary stats (min, max, quartiles, etc and also # NA values for each column)

#method 2:
gg_miss_var(ca_pb_subset) # pro graph to look at the numer of missing variables per column

#method 3:
matrixplot(ca_pb_subset, sortby="poverty")
#not an NA reprpesents the value of a variable where dark = higher valued numbers and whiter= lower valued variables and red represents missing data; sortby = sorts that column from high to low; witout sort by the values wil be ordered by order in the data frame (ex/ observation 1, 2, n); can use this to initially start looking for potential trends in the data and not just for looking at missing data

```


## Remove NAs from pollution data

```{r}
ca_pb_nona <- ca_pb_subset %>% 
  drop_na()  # removes any row with an NA

```
removed NAs in order to run the pca, however there are methods to run pca with NA values if removing too many rows is an issue

## Additional base data exploration

```{r}
skim(ca_pb_nona)
```


## PCA Here

```{r}
# PCA
ca_pca <- prcomp (ca_pb_nona, scale = T)

#PCA results
summary(ca_pca)
ca_pca

# plot pca
#biplot(ca_pca) # creates crazy mess

autoplot(ca_pca, 
         colour = NA, # HAVE to spell it color this way (colour) bc thats the only spelling form autoplot recognizes
         loadings.label = T,
         loadings.label.size = 3,
         loadings.label.colour = "black",
         loadings.label.repel = T) +
  scale_y_continuous(lim = c(-0.05, 0.05))




```


what does it mean that none of the lines are in the (+,+) quadrant?
  
  no cut off with the sum PC1 and PC2 variacne is "good" or "bad" or not publishaable, just indicates with what/how much confidence can interprete the results

## PCA for pollution burdens adn demographics

# Join the pollution and demographics datasets by census tract
```{r}
ca_nona <- ca_dem_clean %>% 
  inner_join (ca_pb_nopct, by = c("census_tract_number" = "census_tract")) %>% 
  drop_na()
#the data frames have a column with data to base the join, but theyre called different things, so use the c("_", "_") to show that and tell R to base the join on the values in these 2 differnetly named columns
# the by = c("_", "_") ORDER MATTERS! the first listed is tied to the frist data frame entered in the code here

```

# Make a subdata set that only includes:

- white_percent
- elderly_65_percent
- pm2_5 (air quality indicator)
- pesticides
- traffic
- asthma
- cardiovascular_disease
- poverty

```{r}
my_sub <- ca_nona %>% 
  select(white_percent, elderly_65_percent, pm2_5, pesticides, traffic, asthma, cardiovascular_disease, poverty)
```

# Run PCA

```{r}
dem_poll_pca <- prcomp(my_sub, scale = T)

#summary
summary(dem_poll_pca)
dem_poll_pca

#plot
autoplot(dem_poll_pca, 
         colour = NA, # HAVE to spell it color this way (colour) bc thats the only spelling form autoplot recognizes
         loadings.label = T,
         loadings.label.size = 3,
         loadings.label.colour = "black",
         loadings.label.repel = T) +
  scale_y_continuous(lim = c(-0.1, 0.1))

```

surprising that cardiovascular disease is more closely relalted with poverty adn asthma, than elderly. 

############################ ESM 244: Lab 2 #############################################
# Add packages ------------------------------------------------------------

library(tidyverse)
library(shiny)
library(shinythemes)
library(here)


# add data ----------------------------------------------------------------

spooky <- read_csv(here("data", "spooky_data.csv"))

# create initial user interface ---------------------------------------------------

#ui <- fluidPage()
#server <- function(input, output) {}

# r recognizes these as unconnected, blank pages/functions
# input=things coming into the ui, output = things going back to the ui

#shinyApp(ui = ui, server = server)

#save script as app.r or app.R --> a Run App button at the top will now appear. Congrats! Made an app! Even though its blank



# create more indepth user interface --------------------------------------

# notes: be careful with commas!!!!, in anctual code, wouldnt include 2 diff sections of ui and server, just doing it here to show examples
# notes: allison recommends getting the basic layout set up first doing little chunks at a time and constantly checking the run app to check progress and make sure everythin is working
# popular things to google for help: shiny widgets, shiny themes

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel ('My Cool App Title'),
  sidebarLayout(
    sidebarPanel("My Widgets are here",
                 selectInput(inputId = "state_select", # name of the funtion, doesnt matter what its called bc it wont be shown in the app. Just a name del function so you can reference it later
                             label = "Choose a State",
                             choices = unique(spooky$state)  # use this to list all options in a column but the choices only appear once bc of the unique function (w/o it all options would appear how every many times theyre in the data ex/ 43 californaias, 13 arizonas, etc)
                 )
                 
    ),
    mainPanel("My Outputs here!",
              tableOutput(outputId = "candy_table")
    )
  )
)

server <- function(input, output) {
  state_candy <- reactive({  # reactive function means what happens here will affect other things
    spooky %>%
      filter (state == input$state_select) %>%    #input refers back to the ui inut where the user choose a state, state_select is the name of the function in the ui code with the information the user choose from
      select (candy, pounds_candy_sold)
  })
  output$candy_table <- renderTable ({   #use render to create an output like a table or plot, so use right funciton (renderTable or renderPlot) based on the output you want
    state_candy()                          # output$candy_table--> candy_table does not exist yet, but this code is saying make this table and save it as candy_table in our outputs data set. now, can reference candy_table in the ui code
  })
  
  
}


shinyApp(ui = ui, server = server)


matrixplot(ca_pb_subset, sortby="poverty")

############################ ESM 244: Lab 3 ###########################################

# Load Packages

```{r}
library(tidyverse)
library(janitor)
library(here)
library(sf)
library(tmap)

```


## Binary Logistic Regression 

add data
```{r}

gentoo <-  read_csv("gentoo_lter.csv")
chinstrap <- read_csv("chinstrap_lter.csv")

```


join the data sets and tidy the data. They have the same info, but the species is different 
```{r}
penguins <- full_join(chinstrap, gentoo) %>% 
  clean_names() %>% 
  mutate (sex = str_to_lower(sex)) %>% 
  filter(sex %in% c("male", "female"))


# in the brackets for full join, just provide the name of the data sets
```


data exploration graph 
```{r}
ggplot(penguins, aes(x=sex, y=body_mass_g)) +
  geom_jitter(aes(color = sex, pch = sex)) +
  facet_wrap(~species)

```

manually assign the penguins as 1 or 0

chinstrap = 1
gentoo = 0

```{r}

penguin_bin <- penguins %>% 
  mutate (sp_binary = case_when(
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ 1,
    species == "Gentoo penguin (Pygoscelis papua)" ~ 0
  ))

# can also use str_detect or contains and only write part of the species (ex/ just chin or gentoo) 

```

when using case_when, if you want to assign remaining groups to one group (w/o having to type it out) can use this code at the end:
  
  penguin_bin <- penguins %>% 
  mutate (sp_binary = case_when(
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ 1,
    TRUE ~ 2
  ))

class has to be the same as the other observations, cannot have factor and numeric in same column

## binomial logistic regression
```{r}
penguin_blr <- glm(sp_binary ~ sex + body_mass_g, 
                   data=penguin_bin, 
                   family = "binomial")

summary(penguin_blr)


```

# lets make some predictions 

1. what is the probability that a penguin is a chinstrap, if it weighs 4500g and is male?
  
  ```{r}
# create data frame 
df_m4500 <- data.frame(sex = "male",
                       body_mass_g = 4500)

#inputs have to be exactly the same as the info in the original data

# find log odds of this 4500g male being a chinstrap penguin

m4500_logodds <- predict(penguin_blr, newdata = df_m4500, type = "link")
m4500_logodds # not that intuitive, so lets use type=repsonse

m4500_probs <- predict(penguin_blr, newdata=df_m4500, type = "response")
m4500_probs # easy to understand answer


```

2. whats the probability that a penguin is a chinstrap if it is a 4000g female 

```{r}

df_f4000 <- data.frame(sex = "female",
                       body_mass_g = 4000)

# find log odds of this 4000g female being a chinstrap penguin
f4000_probs <- predict(penguin_blr, newdata=df_f4000, type = "response")
f4000_probs

```

lets make an entire data frmae and use it to make then visualie logistic regression outcomes
```{r}
# create mock data 
penguins_mock <- data.frame(
  body_mass_g = rep(seq(3000, 6000, length = 200), 2),
  sex = c(rep("male", 200), rep("female", 200))
)

#determine predictions
full_predict <- predict(penguin_blr, newdata= penguins_mock, type="response", se.fit=T)

# combine mock data and prediction values

final_df <- data.frame(penguins_mock, 
                       full_predict$fit, 
                       full_predict$se.fit)

colnames(final_df) <- c("penguin_mass", "sex", "probability", "se")
final_df

# plot 
ggplot(data=final_df, aes(x=penguin_mass, y=probability)) +
  geom_line(aes(color=sex)) +
  geom_ribbon(aes(ymin = probability - se, 
                  ymax = probability + se,
                  fill = sex),
              alpha = 0.3) +
  labs (title = "Probability of being a chinstrap")

```

## using spatial data

```{r}
# add data
cougars <- read_sf(dsn = here("cougar_connections"), layer= "ds1014") %>% 
  clean_names()

#sub data
large_corridors <- cougars %>% 
  filter (area_ac > 20000) %>% 
  select(area_ac)

plot(large_corridors)

# sub data
cougars_sub <- cougars %>% 
  select(elev_mean)

ggplot(cougars_sub) +
  geom_sf(aes(fill = elev_mean), color=NA)

tmap_mode("view")  # set the mode: theres 2 options, view (which is interactive), and plot (not interactive)

tm_shape(cougars_sub) +
  tm_fill("elev_mean")

# click on the stacked squares in the map to select a basemap
```

OK maybe not the most inspiring thing, but it works...now let's change the view mode: 
```{r}
tmap_mode("view") # Set to interactive viewing
tm_shape(cougar_sub) +
  tm_fill("elev_mean") +
  tm_basemap("Stamen.Terrain")
```

See http://leaflet-extras.github.io/leaflet-providers/preview for a preview of basemaps you can add with `tm_basemap`!

Try out some different basemaps and test interactivity, and see that it's maintained when you knit! 
  
############################# ESM 244: Lab 4 Session 3 ##########################################


# Load Packages 
```{r, include = F}
library(tidyverse)
library(sf)
library(tmap)
library(janitor)
```
# Load data

```{r}

ca_eco <- read_sf(dsn = "session_3_materials", layer = "ca_eco") %>% 
  clean_names() %>% 
  select(us_l3name) %>% 
  rename(region = us_l3name) %>% 
  st_simplify(dTolerance = 100) # simplifies num of points used to create a polygon (makes the maps appear faster. Careful when simplify spatial data though. Use a dTolerance value of 100000 and get very incorrect looking map!). Can also use rmapshaper::ms_simplify() or geos::gSimplify() to simplify polygons of spatial data.

#dsn functions like here function. If the rproj and the spatial data are in the same folder use dsn = "."

ca_counties <- read_sf(dsn = "session_3_materials", layer = "california_county_shape_file") %>% 
  clean_names()

ca_dams <- read_sf(dsn = "session_3_materials", layer = "California_Jurisdictional_Dams")


```

```{r}
plot(ca_eco)
# check coord system del spatial data
st_crs(ca_eco) # theres no epsg code for this data, but can set it ourselves
st_crs(ca_counties)
st_crs(ca_dams)

# two ways to set the coord system
ca_eco <- ca_eco %>% 
  st_transform(crs = 4326)
st_crs(ca_counties) = 4326 

# make sure coord system of spatial layers are the same!!!
```


# make a map of dams in CA overlaid onto ecoregions with outlines of CA counties

```{r}

ggplot(data = ca_counties) + 
  geom_sf(color = "black", size = 0.1) +
  geom_sf(data = ca_eco, 
          aes(fill = region),
          alpha = 0.5,   # have two spatial data sets, but can only see one, so use alphs to make both visible. 
          color = "NA") + # color here is refering to the color of the line between polygons 
  geom_sf(data = ca_dams, size = 0.5, alpha = 0.5, size = 0.5) +
  theme_minimal()
```


# only want data in SB county. So use function to find where the data in these layers overlap

```{r}
#first, use filter to get sub data with just sb county

sb_county <- ca_counties %>% 
  filter (name == "Santa Barbara")

# clip eco-region to include only information within SB county

eco_clip <- st_intersection(ca_eco, sb_county)

# lets plot it!

ggplot() +
  geom_sf (data = ca_counties, 
           fill = "grey90", 
           color = "grey80", 
           size = 0.2) +
  geom_sf(data = eco_clip,
          aes(fill = region),
          color = 'white',
          size = 0.4) +
  coord_sf(xlim = c(-121, -119),
           ylim = c(33.5, 35.5)) +  # essentially crops the map to within these specified coords
  theme_minimal()

```



# interactive map %>% 

```{r}

sb_clip_map <- tm_basemap("Esri.WorldImagery") + # use ?tm_basemap in the console to get link to website that shows all the potential basemaps avail
  tm_shape(eco_clip) +
  tm_fill("region", palette = c("orange", "purple", "yellow"), alpha = 0.5)  # using "region" is anagolous to using aes(fill=region) in ggplot to tell R waht to base the color of the polygons on

tmap_mode("view") # defalt is static, view is interactive map so need this line of code before the name of the graph. the interactivness is maintained when the file is knitted. Need interactive mode for the basemap to show up
sb_clip_map

```

# convert latitude and longitude from excel into data that R will recognize as spatial data

```{r}
# make mock data using tribble() so can trouble shoot and get the code working on small data set first

my_example <- tribble (
  ~id, ~lon, ~lat,  #create column names using this format
  "tiger", -119.4, 34.35, # data for the first row, words need " ", while numbers dont
  "lion", -119.41, 34.39,
  "bear", -119.43, 34.38
)

# use class(my_example) to see if R recognizes this as spatial data or as a table. In this case, R recognizes it as a table

# convert to sf data
animals_sf <- st_as_sf(my_example, coords = c('lon', 'lat'), crs = 4326) # tell R which columns have the coords for longitude and latitude, IN THAT ORDER!!!

#use class(animals_sf) to maek sure r recognizes it as sf data#
```


# make a tmap

```{r}
animal_map <- tm_basemap("Stamen.Watercolor") +
  tm_shape(animals_sf) + # use tm_shape for polygon data and tm_dots for point data, but (need to?) include tm_shape here along with tm_dots, no tm dots code will get an error
  tm_dots(labels = "id", col = "purple", size = 0.5) 
# dont need tm_map(view) bc est that setting earlier
# when hover mouse over the fire, will get the info for that point that was specified in labels = ..

```

# chloropleth of dams

```{r}
intersection <- st_intersection(x = ca_dams, y = ca_counties)

dams_per_county <- intersection %>% 
  group_by(name) %>% 
  tally() # tally here performs the same fucntion as count()

ca_tot <- ca_counties %>% 
  st_join(dams_per_county) %>% 
  select(name.x, n) %>% 
  rename (name = name.x)

# convert NAs to zeros
ca_tot_zero <- ca_tot %>% 
  replace_na(list(n = 0)) # use list when want to perform this function for multiple columns; this says where ever in the column named n, replace any NA calues with 0  

# geometry is kept when perform all these join/filter/select functions


```
ggplot() +
  geom_sf(data=ca_tot_zero,
          aes(fill = n),
          size = 0.2,
          color = "white") +
  scale_fill_continuous(low = "yellow", high = "red") #manunally assign a color scheme. use high and low for continous data



```


places for more resources for sf data:
  
  - vignettes for sf (google this, sf package in github)
- "geocomputation with R" by Robin Lovelace (book)

########################### ESM 244: session4 ex1 #####################################

library(shiny)
library(tidyverse)
library(shinydashboard)

# add data ----------------------------------------------------------------

penguins <- read.csv("penguins.csv")

# create user interface (ui) ----------------------------------------------

ui <- fluidPage(
  titlePanel("Shiny App Title"),
  sidebarLayout(
    sidebarPanel("Enter text here: Widgets",
                 radioButtons(inputId = "species",  # radio buttons = all options visible with little bubble next to it to selct that option
                              label = "Choose Penguin Species:",
                              choices = c("Adelie", "Gentoo", "Awesome Chinstrap" = "Chinstrap")), # these choices = same as the options in the sp_short column. They HAVE TO match, ,otherwise will get issues, but to get around this can use the "" = "", within the same comma space. This changes what the user sees, but when filter the data can filter for chinstrap and wont have issues
                 selectInput(inputId = "pt_color",
                             label = "Select a Fun Color!",
                             choices = c("Rad Red" = "red", 
                                         "Pretty Purple" = "purple",
                                         "orange"))),  #select input = drop down menu
    mainPanel("Graph!", 
              plotOutput(outputId = "penguin_plot"),
              tableOutput(outputId = "penguin_table"))
  )
)

# create server -----------------------------------------------------------

server <- function(input, output) {
  
  penguin_select <- reactive({
    penguins %>% filter(sp_short == input$species) # this says, in this penguiins data set, filter to only include the observations from the species selected by the user (input) in the widget called species (--> input$name of widget)
  }) # have to tell R the output is reactive and the bracket format has to be: ({}) when creating reactive function (alsouse this bracket formaat for renderPlot, etc bc theyre reactive)
  
  output$penguin_plot <- renderPlot({
    ggplot(data = penguin_select(), aes(x = flipper_length_mm, y = body_mass_g)) + #when referencing a reactive data frame, have to include empty brackets at the end --> ()
      geom_point(color = input$pt_color) 
  })
  
  penguin_table <- reactive({
    penguins %>%
      filter(sp_short == input$species) %>%
      group_by(sex) %>%
      summarize(
        mean_flip = mean(flipper_length_mm),
        mean_mass = mean(body_mass_g)
      )
  })
  output$penguin_table <- renderTable({
    
    penguin_table()
    
  })
}

# combine ui and server into an app ---------------------------------------

shinyApp(ui = ui, server = server)
# must save r script as "app.r" before running the shinyApp code above. WHen do run it, a web page will show up 


############################ ESM 244 lab 4 session4 ex2 #####################################

library(shiny)
library(tidyverse)
library(shinythemes)

# User interface ----------------------------------------------------------

ui <- navbarPage("Navigation Bar!",
                 theme = shinytheme("cyborg"),
                 tabPanel("First Tab",
                          h1("Big Header"),  # like in r markdown where the number of #s corresponds to text size, in shiny we use h1, h2, h3, h4, h5, (etc?) to indicate text hize. h1= largest and the other h# get progressively smaller
                          p("Heres some regular text in paragraph..."),
                          plotOutput(outputId = 'diamond_plot')), # p stands for paragraph. Can add it decent amount of text. If have a lot of text, can import external text or r markdown files instead, but didnt go over that in class
                 tabPanel("Second Tab",
                          sidebarLayout(  # in sidebar layout, need to specify what is in the main panel and side panel
                            sidebarPanel("text here",
                                         checkboxGroupInput(inputId = "diamondclarity",
                                                            label = "Choose Some Options!",
                                                            choices = c(levels(diamonds$clarity)))),  # when there are several options for a value in a column, can use this method of inputing the options (works here bc this column is recognized by r as an ordered factor)
                            mainPanel("Main Panel text here",
                                      plotOutput(outputId = 'diamond_plot2'))
                          ))
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  output$diamond_plot <- renderPlot({
    ggplot(data = diamonds, aes(x = carat, y= price)) +  #dont need {} in renderplot bc not a reactive graph, dont need () after the data bc again, not reactive plot
      geom_point(aes(color = clarity))
  })
  
  diamond_clarity <- reactive({
    diamonds %>% 
      filter(clarity %in% input$diamondclarity)
  })
  output$diamond_plot2 <- renderPlot({
    
    ggplot(data = diamond_clarity(), aes(x = clarity, y = price)) +
      geom_violin(aes(fill = clarity))
    
  })
}

# run shiny app -----------------------------------------------------------

shinyApp(ui = ui, server = server)

############################ ESM 244: Lab 4 session 4 ex3 ###################################

ui <- dashboardPage(
  dashboardHeader(title = "Star Wars"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Homeworld", tabName = "Homes", icon = icon("jedi")),
      menuItem("Species", tabName = "species", icon = icon('pastafarianism'))# use ?icon in console to get link to the diff icon options in font awesome free
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Homes", # reference the tab (which was specified above) under which you want this input to be located
        fluidRow (
          box(title = "HomeWorld Graph", # give name you want this info to go under then 
              selectInput('sw_species',
                          label = "Choose Species:",
                          choices = c(unique(starwars$species)))),
          box(plotOutput(outputId = "sw_plot")))
        
        
      )
    )
  )
)


# server --------------------------------------------------------------

server <- function(input, output) {
  
  species_df <- reactive ({
    starwars %>% filter(species == input$sw_species)
  })
  output$sw_plot <- renderPlot({
    ggplot(species_df(), aes(x = homeworld)) +
      geom_bar() 
  })
}


# shiny app -----------------------------------------------------------

shinyApp(ui = ui, server = server)

########################### ESM 244: Lab 5 (aka gh-pages-test) #####################################

# load packages

library(tidyverse)
library(janitor)
library(lubridate)
library(here)
library(paletteer)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(forecast)
library(sf)
library(tmap)
library(mapview)

```

## Monthly US energy consumption (renewables)

```{r}

#load data

us_renew <- read_csv(here("data", "renewables_cons_prod.csv")) %>% 
  clean_names()

# sub-data: make descriptions lowercase, only keep observations including the phrase "consumption", remove observations in the description column with the phrase "total"

renew_clean <- us_renew %>% 
  mutate (description = str_to_lower(description)) %>% 
  filter(str_detect(description, pattern = "consumption")) %>% #only keeps observations in this column with the word consumption
  filter(!str_detect(description, patter = "total")) # gets rid of any obs where the word total is incldued in the observation. use the ! in front of str_detect to remove (instead of keep) observations

```


```{r}
# convert "yyyymm" column to a date

renew_date <- renew_clean %>% 
  mutate(yr_mo_day = lubridate::parse_date_time(yyyymm, "ym"))
# get NAs bc some observations in the original yyyymm column are not in the traditional yyyymm format (ex/ 191913, theres no 13th month). Gives all values the day value of 01

renew_date <- renew_date %>% 
  mutate(month_sep = yearmonth(yr_mo_day)) %>% # provides the yyyymm data in "yyyy first 3 letter of the Name of Month" format
  mutate(value = as.numeric(value)) %>% 
  drop_na(month_sep, value)

#make a data frame where the month and dyear are in separate columns

renew_parsed <- renew_date %>% 
  mutate(year = year(yr_mo_day)) %>% 
  mutate(month = month(yr_mo_day, label = T))# to keep the month stored as a labeled name (in 3 letter abrev. frmat) instead of a number (which is the default) use label = T



```

## Visually explore data

```{r}

renew_gg <- ggplot(data = renew_date, aes(x = month_sep, y = value)) +
  geom_line() #shows useless, random graph. Need to group the data by a specific column

renew_gg <- ggplot(data = renew_date, aes(x = month_sep, y = value, group = description)) +
  geom_line(aes(color = description)) #group = redundant when base color on an aesthetic, but can still keep it 

# use view(palettes_d_names) in the console to get a datasheet of the diff packages and the names and how many colors in the package (this is in the length column). Need to use palettes_d bc have aes based on discrete data. use palettes_c_names for data for continuous data

```

##add colors to graph with paletteer

```{r}
renew_gg +
  scale_color_paletteer_d("calecopal::figmtn") # format= (package::palette name). make sure to use ..._d bc the color is based on discrete data. Make sure palette you use has enough colors for the number of discrete classes you have. in this case, we need a package with 7+ colors
```


## coerce data renew_parse to a tsibble
```{r}

renew_ts <- as_tsibble(renew_parsed, key = description, index = month_sep)

#the yyyy Mon is a tibble freindly format. can also use other date formats

```


```{r}
renew_ts %>% autoplot(value)
renew_ts %>% gg_subseries(value) #diff consumption sources split up by month
# renew_ts %>% gg_season(value) --> problem with source package

ggplot(data = renew_parsed, aes(x = month, y = value, group =year)) +
  geom_line(aes(color = year)) +
  facet_wrap(~description,
             ncol = 1,
             scales = "free", 
             strip.position = "right") # this puts the name of the groups on the side instead of on the top, which is the default

```


##just look at hydroelectric energy consumption

```{r}
hydro_ts <- renew_ts %>% 
  filter(description == "hydroelectric power consumption")

hydro_ts %>% autoplot(value)
hydro_ts %>%  gg_subseries(value) #look at change over each month
ggplot(data = hydro_ts, aes(x = month, y = value, group =year)) +
  geom_line(aes(color = year))

```

## quarterly average consumption for hydro

```{r}
hydro_quarterly <- hydro_ts %>% 
  index_by(year_qu = ~(yearquarter(.))) %>%#why put a period here? how does r know which column to index by? also what are the quarters? jan-mar, apr-june, etc?
  summarise(avg_consumption = mean(value))

```

##Decompose the hydro_ts data frame

```{r}
#using moving average

#create the model
dcmp <- hydro_ts %>% 
  model(STL(value ~ season(window = 5))) # says decompose the values in the value column using a window of 5

#plot deconstructed model results 
components(dcmp) %>% autoplot() # can manually add equal scales like would in ggplot

```


```{r}
hydro_ts %>% 
  ACF(value) %>% 
  autoplot

```
confirms there is a seasonality trend 

#DANGER: dont copy this method of forecasting. its just a quick example!!!!

```{r}
hydro_model <- hydro_ts %>% 
  model(
    ARIMA(value)
  ) %>% 
  fabletools::forecast(h = "4 years") #the package recognizes the saying 4 years!

hydro_model %>% autoplot() # only shows the forecasted values/years 

hydro_model %>% autoplot(hydro_ts) #plots all data from hydro_ts including the predicted values

hydro_model %>% autoplot(filter(hydro_ts, year(month_sep) > 2010)) # in hydro_ts dataset, only plot just the years after 2010 plus the predicted values

```

## same as above but with 2 diff model typesforecasting the same values. typically papers will publish more than one modell bc we cant say for sure if that one prediction will happen. 
```{r}
hydro_model <- hydro_ts %>% 
  model(
    ARIMA(value),
    ETS(value)
  ) %>% 
  fabletools::forecast(h = "4 years") #the package recognizes the saying 4 years!

hydro_model %>% autoplot() # only shows the forecasted values/years 

hydro_model %>% autoplot(hydro_ts) #plots all data from hydro_ts including the predicted values

hydro_model %>% autoplot(filter(hydro_ts, year(month_sep) > 2010)) # in hydro_ts dataset, only plot just the years after 2010 plus the predicted values

```



##World map with SF

```{r}
world <- read_sf(dsn = here("data", "TM_WORLD_BORDERS_SIMPL-0.3-1"), 
                 layer = "TM_WORLD_BORDERS_SIMPL-0.3")
```

```{r}
mapview(world)
```

################################# ESM: Lab 5 Key #################################

## gh-pages set-up

Some of you have already been working in blogdown. But there are *other* ways that you can make your RMarkdown materials available for others to see. We'll create a repo that contains .Rmd and knitted .html files that we can share as web pages!

**Note**: The gh-pages section of this lab is inspired by materials from Dr. Julia Lowndes (https://jules32.github.io/). 

- On GitHub: @jules32 
- Twitter: @juliesquid

Julia has written a post on how to make website with RMarkdown and GitHub here: https://jules32.github.io/rmarkdown-website-tutorial/

Follow along with these steps to make your gh-pages branch:

- Create a new GitHub repo called 'my-webpage-test'
- Click the 'Branch: master' button 
- Type in (**exactly**): `gh-pages`, and create a new branch
- Go to Settings > Branches
- Update the 'Default' branch to `gh-pages`
- Click on `2 branches` and **delete** the master branch
- Update your ReadMe to say something unique, then commit
- Check that it can be viewed as a website with `your-user-name.github.io/repo-name/`
- Clone your repo to work locally in RStudio
- Copy and paste the `data` folder for this week into your project 
- Create a new .Rmd called `us-renewables.Rmd` in the project

In that .Rmd: 

### 0. Attach packages

```{r}
# For general stuff:
library(tidyverse)
library(janitor)
library(lubridate)
library(here)
library(paletteer)

# For ts stuff: 
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(forecast)

# For spatial stuff: 
library(sf)
library(tmap)
library(mapview)
```


## Monthly US energy consumption (renewables)

### 1. Get the data

We'll explore, then forecast, US energy consumption and production by renewables source. Get the data from `renewables_cons_prod.csv`:
  
  ```{r}
us_renew <- read_csv(here("data", "renewables_cons_prod.csv")) %>% 
  clean_names()
```

Explore the data frame:
  
  ```{r}
# View(us_renew)
# names(us_renew)
# unique(us_renew$description)
```

We'll focus on consumption data. 

### Clean up data

- Convert description to all lowercase
- Only keep observations for "consumption"
- Remove any "total" observations

```{r}
renew_clean <- us_renew %>% 
mutate(description = str_to_lower(description)) %>% 
filter(str_detect(description, pattern = "consumption")) %>% 
filter(!str_detect(description, pattern = "total"))
```

### Convert `yyyymm` column to date with `lubridate`

```{r}
renew_date <- renew_clean %>% 
mutate(yr_mo_day = lubridate::parse_date_time(yyyymm, "ym")) %>% 
mutate(month_sep = yearmonth(yr_mo_day)) %>% #coerce to tsibble `yearmonth` format
mutate(value = as.numeric(value)) %>% 
drop_na(month_sep, value)

# Want to parse the year and month? We may use this later...
renew_parsed <-renew_date %>% 
mutate(month = month(yr_mo_day, label = TRUE)) %>% 
mutate(year = year(yr_mo_day))
```

### Make a ggplot

Make, then save, the baseline plot as `renew_gg`:

```{r}
renew_gg <- ggplot(data = renew_date, aes(x = month_sep, y = value, group = description)) +
geom_line(aes(color = description)) +
theme_minimal() +
scale_y_continuous(limits = c(0, 350))

renew_gg
```

Now try updating your color palette using options from paletteer. Use `View(palettes_d_names)` to see all of the discrete scale options. We'll want a palette with at least 7 colors (length >= 7). Find a name of a palette that you like, then update your graph by adding `scale_color_paletteer_d("package::palette")`. Like, if I want to use the `calecopal::figmtn` palette, I'd add:

`renew_gg + scale_color_paletteer_d("calecopal::figmtn")`

Try some out!

```{r}
renew_gg +
scale_color_paletteer_d("calecopal::figmtn")
```

Have some fun trying out different color palettes.  

### Coerce to a tsibble:

```{r}
renew_ts <- as_tsibble(renew_parsed, key = description, index = month_sep)
```

### Look at the data in a few different ways:
```{r}
renew_ts %>% autoplot(value)
renew_ts %>% gg_subseries(value)
#renew_ts %>% gg_season(value)

# What if gg_season() didn't work? Well we can make this with ggplot anyway!
  # Remember our other parsed version (renew parsed):
  
  ggplot(data = renew_parsed, aes(x = month, y = value, group = year)) +
  geom_line(aes(color = year)) +
  facet_wrap(~ description, 
             ncol = 1, 
             scales = "free", 
             strip.position = "right")

```

### Get just the hydroelectric energy consumption data:
```{r}
hydro_ts <- renew_ts %>% 
  filter(description == "hydroelectric power consumption")

# Explore: 
hydro_ts %>% autoplot(value)
hydro_ts %>% gg_subseries(value)
#hydro_ts %>% gg_season(value)

# OK, what if gg_season() doesn't work?
# It's just a function that uses ggplot() to do things we already know how to do in ggplot()!
ggplot(hydro_ts, aes(x = month, y = value, group = year)) +
  geom_line(aes(color = year))

```

### Calculate summary data by time using `index_by()`

What if we want to calculate consumption by quarter? We'll use `index_by()` to tell R which "windows" to calculate a value with in. 

Quarterly:
```{r}
hydro_quarterly <- hydro_ts %>% 
index_by(year_qu = ~ yearquarter(.)) %>% # monthly aggregates
summarise(
avg_consumption = mean(value)
)

head(hydro_quarterly)
```

Or annually: 
```{r}
hydro_annual <- hydro_ts %>% 
index_by(annual = ~year(.)) %>% 
summarize(
avg_consumption = mean(value)
)

ggplot(data = hydro_annual, aes(x = annual, y = avg_consumption)) +
geom_line(color = "darkorange") +
geom_smooth(color = "purple",
size = 0.2,
linetype = "dashed",
fill = "purple",
alpha = 0.2) +
theme_minimal()
```

And if you have higher interval data (e.g. hourly), then you can calculate summaries by week, month, etc. using functions from `tsibble` like: 

- `yearweek`
- `yearmonth`

### Decompose the hydro consumption ts data

First, let's check the decomposition (STL):
  ```{r}
# Find STL decomposition
dcmp <- hydro_ts %>%
  model(STL(value ~ season(window = 5)))

# View the components
# components(dcmp)

# Visualize the decomposed components
components(dcmp) %>% autoplot() +
  theme_minimal()

# Let's check out the residuals:
hist(components(dcmp)$remainder)

```

### Explore the ACF

```{r}

hydro_ts %>% 
  ACF(value) %>% 
  autoplot()

```

We see peaks at 12 months: annual-difference similarities in consumption. 

### Forecast future hydro power consumption 

```{r}
hydro_model <- hydro_ts %>%
  model(
    arima = ARIMA(value)
  ) %>%
  fabletools::forecast(h = "2 years")

hydro_model %>% autoplot(filter(hydro_ts, year(month_sep) > 2010), level = NULL)

```

## Map-of-the-day

A world map with bubbles!
  ```{r}

# Get spatial data: 
world <- read_sf(dsn = here("data","TM_WORLD_BORDERS_SIMPL-0.3-1"), layer = "TM_WORLD_BORDERS_SIMPL-0.3") %>% clean_names()

# Quick & easy option to see those polygons (also for points, lines!)
mapview(world)

# ggplot (static)
world_base <- ggplot(data = world) +
  geom_sf(aes(fill = pop2005),
          color = NA) + 
  scale_fill_paletteer_c("viridis::viridis") +
  theme_minimal()

world_base

# Let's crop it: 
world_base +
  coord_sf(xlim = c(-20, 50), ylim = c(-40, 40), expand = FALSE)
```

## Making this a web page:

- Knit to create the html
- Push all updates back to GitHub

Since you are working in a gh-pages branch, this can be a webpage! 
  
  - Go to your-github-username.github.io/repo-name/file-name-prefix
- You should see the webpage containing your knitted document!
  
  - Troubleshooting:
  - Don't including a trailing slash (404 error)
- Don't include the .Rmd extension (will ask to download .Rmd)

# End Lab Week 5

############################ ESM 244: Lab 6 ######################################


```{r}
# load packages-------------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(raster)
library(tmap)
library(tmaptools)
library(gstat)
library(sf)

```

## Grand Canyon GeoTIFF

```{r}

#load data
gc_dem <- raster (here("data", "gc_dem.tif"))

#plot raster 
plot(gc_dem)

#check coord system
crs(gc_dem) # UTM 12, so measurements in meters instead of degrees

# check the extent (ie max and min boundaries of the layer)
extent(gc_dem)

#create a wgs84 with latitude and longitude metrics (meters --> degrees; to do this copy and paste the crs output (from above), remove ellps adn twgs from the copied crs output, change proj=utm to proj=long/lat, and put the output in " "). One way to do this. there are other ways. 

wgs84 <- "+proj=longlat +zone=12 +datum=WGS84 +units=m +no_defs"

# reproject the layer (ie change coord system)
gc_reproject <- projectRaster(gc_dem, crs = wgs84, method = "bilinear")

# check that the reproject changed 
extent(gc_reproject) # no units are in degrees

```


# Crop raster to smaller area (bounding box)

```{r}

bounds <- as(extent(-112.4, -112.0, 36.1, 36.3), "SpatialPolygons")
# choose coordinates within the extent ( which is determined by extent(layer_name)). not sure what spatialpolygons is but need it

```


# make the crs of our bounding box the same as for the gc_reproj

```{r}
crs(bounds) <- crs(gc_reproject)

```



# before created a bounding box, now lets actually crop our original data
```{r}
#crop
gc_crop <- crop(gc_reproject, bounds)

#visualize new layer
plot(gc_crop)

```

# resample layer using agregate function

if using another layer with diff resolution, setting the layers to the same reesolution will allow you to do raster math

```{r}
# resample: specify that using aggregation funciton from the raster package, specify the layer going to resample, fact = new cell/pixel size, and default= to use the mean value of the cells used to create the new, larger cell
gc_agg <- raster::aggregate(gc_crop, fact = 30)

#visualize new layer
plot(gc_agg)

```


# ggplot



```{r}
# first, convert data into a data frame
gc_df <- as.data.frame(gc_crop, xy= T)

# xy parameter tells r in inlcude the lat and long data of the cells in the data frame

# ggplot
ggplot(data = gc_df, aes(x = x, y = y)) +
  geom_raster(aes(fill = gc_dem)) +
  coord_quickmap() +
  theme_minimal() +
  scale_fill_gradientn(colors = c("purple",  "magenta", "orange",'yellow', 'white')) # gradientn = can choose as many colors as you want and they will appear as they are listed

```

# how to select cells that match a given criteria


```{r}
# copy gc_crop
gc_hab <- gc_crop

# set any cells outside of 1000-1500 to NA
gc_hab[gc_hab > 1500 | gc_hab < 1000] <- NA # this says: from this layer, look for cell values greater than 1500 and less than 1000 and assign those cells to a new value which is NA

#plot to visualize the new layer
plot(gc_hab)

```

# now lets make this interactive with tmap

```{r}
#set tmap to view
tmap_mode("view")

#plot

# PC users get an error runngin this code: tm_shape(gc_hab) + tm_raster(legend.show = F, palette = "plasma")



```


## Kriging: prediciting rain in Kansas

```{r}
#load data
ks_counties <- read_sf(here("data", "ks_counties", 'ks_counties_shapefile.shp'))

# use baseplot to visualize data
plot(ks_counties)

#check crs
crs(ks_counties) # no crs

# set crs for ks_counties
st_crs(ks_counties) <- 4326

#check crs again to make sure it worked
crs(ks_counties)

# plot again. assigned a coord system so the state now looks like it should (before it was a little stretched out)
plot(ks_counties)

# ggplot

ggplot(data = ks_counties) +
  geom_sf()

```

# read in rainfall data

```{r}
#load data. it has lat and long data/columns, but r does not recognize the csv data as spatial data
ks_rain <- read_csv(here("data", "ks_rain.csv")) %>% 
  clean_names

# have r recognize ks_rain as spatial data
ks_sf <- st_as_sf(ks_rain, coords = c("lon", "lat"), crs = 4326) # parameters = name of data, which columns (the column names) have the longitude and latitude data. HAVE TO ASSIGN LONGITUDE AND LATITUDE DATA IN THAT ORDER, then set the coord system


```


# plot

```{r}
ggplot() +
  geom_sf(data = ks_counties) +
  geom_sf(data = ks_sf,
          aes(color = amt, size = amt), 
          show.legend = F) +
  theme_minimal()
```


## kriging to predict rainfall across the entire state

```{r}
# not sure the purpose of this code
ks_sp <- as_Spatial(ks_sf)
class(ks_sp)

```

# make a spatial pixels grid that we'll make predictions over

use bbox(ks_sp) (in the console) to get the min and max coordinates of the data frame

```{r}
lat <- seq(37, 40, length.out = 200) # so this creates a grid from lat from 37 and 40 with points that are 200 points equidistant from the predceeding grid point. 
long <- seq(-94.6, -102, length.out = 200)

# now expand this into a spatial grid
grid <- expand.grid(lon = long, lat = lat)

# now have r recognize the grid as spatial data. make sure it has the same coord system as the real data
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)

grid_sp <-  as_Spatial(grid_sf)

#plot grid
plot(grid_sf)


```

## make variogram: want to know how closely related rainfall amounts are based on distance btw points

```{r}

# variogram
ks_vgm <- gstat::variogram(amt ~ 1, data = ks_sp)

#plot variogram output
plot(ks_vgm)

# estimates for variogram parameters:
## nugget = 0.1
# sill = 0.8
#range = 200

# fit variogram model
ks_vgm_fit <- fit.variogram(ks_vgm, model = vgm(nugget = 0.2, psill = 0.8, range = 200, model = "Sph")) # --> input best guesses forr nugget, psill and range; other model types = Sph (Sphere?), Exp (exponential), Gau (gausian). 

# see model fit outputs. Can see how your guesses of nugget, psill, and range match up to the model estiamtes
ks_vgm_fit

#plot model and fitted model to get line of best fit
plot(ks_vgm, ks_vgm_fit)

```

plot shows...how much rainfall differes between points based on distance btw the points


# now, Krige!
```{r}

ks_krige <- krige(amt ~ 1, ks_sp, grid_sp, model = ks_vgm_fit)

```

to view the krige output--> in console view(ks_krige@data)


# plot krige predictions

```{r}
spplot(ks_krige, "var1.pred")
```


# make data frame of krige predictions

```{r}

#data frame
ks_df <- data.frame(ks_krige@data["var1.pred"],
                    ks_krige@data["var1.var"],
                    ks_krige@coords) %>% 
  rename(longitude = coords.x1,
         latitude = coords.x2)

# convert df to spatial data
rain_sf <- st_as_sf(ks_df, coords = c("longitude", "latitude"), crs = 4326)

# plot predictions. predictions are based on a grid and not the outline of kansas, so next step is to crop the predicitons to the state of kansas
ggplot (rain_sf)+
  geom_sf(aes(color = var1.pred))


```

# crop predcitions to boundary of kansas

```{r}
# load data 
ks <- read_sf(dsn = here("data", "states"), layer = "cb_2017_us_state_20m") %>% 
  dplyr::select(NAME) %>% 
  filter (NAME == "Kansas") %>% 
  st_transform(crs = 4326) # use st_transform instead of st_crs bc theres already a coord system associated with this data. 

# crop
rain_sf_ks <- st_intersection (rain_sf, ks)

# plot
ggplot(rain_sf_ks) +
  geom_sf(aes(color = var1.pred))

# plot
ggplot(rain_sf_ks) +
  geom_sf(aes(color = var1.var))
#low uncertainty with points that have many close by points and high uncertainty with points that have few nearby points

```



#################################### Shiny App Assignment #######################################

# load packages ------------------------------------------------------------

# General packages
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(dplyr)
library(scales)

# Shiny packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)

# Map and graph packages
library(sf) 
library(gganimate)
library(transformr) #need?
library(magick) #need?
library(tmap)
library(ggthemes)
library(ggridges)

# add data ----------------------------------------------------------------

ca_border <-  read_sf(here::here("Arc_data", "ca_state_border"), layer = "CA_State_TIGER2016") %>% 
  st_transform(crs = 4326) 

fire_raw <- read_sf(here::here("Arc_data", "fire_perimeter_shpfile"), layer = "fire_perimeters" ) # still need to remove fires outisde of CA, even though those fires are listed as being in CA but visually are outside of the state boundaries

# sub data ------------------------------------------------------------------------------------------

#test data (limit data size to stop r from freezing)-------------
fire <- fire_raw %>% 
  clean_names() %>% 
  dplyr::filter (acres > 200) %>% 
  mutate (fire_name = str_to_title(fire_name)) %>%
  st_transform(crs = 4326) %>% 
  mutate (decade = case_when(
    year < 1900 ~ "1890s",
    year < 1909 ~ "1900s",
    year < 1919 ~ "1910s",
    year < 1929 ~ "1920s",
    year < 1939 ~ "1930s",
    year < 1949 ~ "1940s",
    year < 1959 ~ "1950s",
    year < 1969 ~ "1960s",
    year < 1979 ~ "1970s",
    year < 1989 ~ "1980s",
    year < 1999 ~ "1990s",
    year < 2009 ~ "2000s",
    year < 2019 ~ "2010s"
  )) %>% 
  st_as_sf() %>% 
  #st_simplify(dTolerance = 10) %>% error. converts geometry to empty values :(
  mutate(year = as.numeric(year))

# decades fire map for intro data-----------------------------------------------------------

fire_facet <- fire %>% filter(!is.na(decade)) %>% filter(decade != "1890s")
fire_decade_count <- fire_facet %>% group_by(decade) %>% count()

# sub data for fire length and season -------------------------------------

fire_date <- fire %>% 
  drop_na(alarm_date) %>% 
  drop_na(cont_date) %>% 
  mutate (alarm_year = lubridate::year(alarm_date), 
          alarm_month = lubridate::month(alarm_date),
          alarm_day = lubridate::day(alarm_date),
          alarm_day_of_year = lubridate::yday(alarm_date), #** convert mm/dd/yyyy to day of the year(0-365)
          cont_year = lubridate::year(cont_date), 
          cont_month = lubridate::month(cont_date),
          cont_day = lubridate::day(cont_date), 
          cont_day_of_year = lubridate::yday(cont_date)) %>% 
  mutate (length_of_fires = (cont_day_of_year - alarm_day_of_year)) %>%  # doesnt account for fires that stated in one year, but ended in another
  mutate (decade = case_when(
    year < 1939 ~ "1920s-1930s", # combine 1920s adn 1930s data
    year < 1949 ~ "1940s",
    year < 1959 ~ "1950s",
    year < 1969 ~ "1960s",
    year < 1979 ~ "1970s",
    year < 1989 ~ "1980s",
    year < 1999 ~ "1990s",
    year < 2009 ~ "2000s",
    year < 2019 ~ "2010s"
  )) # no alarm date for fires before 1920s

fire_season_post1970 <- fire_date %>% 
  filter(alarm_year %in% c(1970:2019)) %>% 
  mutate(year = as.factor(year)) %>% 
  group_by(alarm_year) %>% 
  summarize(min_alarm = min(alarm_month),
            mean_alarm = mean(alarm_month),
            max_alarm = max(alarm_month),
            sample_size = n()) 

mean_season <- ggplot(fire_season_post1970, aes(x = alarm_year, y = mean_alarm)) +
  geom_line() +# try the stacked facet wrap
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     lim = c(5, 9),
                     breaks = seq(5, 9, by = 1),
                     labels = c("May", "Jun", "Jul", "Aug", "Sep")) +
  labs(x = "", y = "Average Fire Start Month\n")

min_season <-  ggplot(fire_season_post1970, aes(x = alarm_year, y = min_alarm)) +
  geom_line() +# try the stacked facet wrap
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     lim = c(1, 8),
                     breaks = seq(1, 8, by = 1),
                     labels = c("Jan", "Feb","Mar","Apr", "May", "Jun", "Jul", "Aug")) +
  labs(x = "", y = "Earliest Fire Start Month\n")

max_season <- ggplot(fire_season_post1970, aes(x = alarm_year, y = max_alarm)) +
  geom_line() +# try the stacked facet wrap
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     lim = c(6, 12),
                     breaks = seq(6, 12, by = 1),
                     labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  labs(x = "", y = "Latest Fire Start Month\n")

season_ggridges <- ggplot(fire_date, aes(x = alarm_month, y = decade)) +
  geom_density_ridges(aes(fill = decade), show.legend = F) +
  labs(x = "Fire Start Month", y = "") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0),
                     lim = c(1, 12),
                     breaks = seq(1,12, by = 1), 
                     labels = c("Jan", "Feb","Mar","Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_discrete(expand = c(0,0)) 

# fire length -------------------------------------------------------------

fire_length_sub_pos<- fire_date %>% 
  filter(length_of_fires >= 0) 
fire_length_sub_neg <- fire_date %>% 
  filter(length_of_fires < -100) %>% 
  mutate(length_of_fires = ((365 - alarm_day_of_year) + cont_day_of_year))
fire_length <- rbind(fire_length_sub_pos, fire_length_sub_neg) %>% 
  mutate(alarm_year = as.numeric(alarm_year))

fire_length_no_outliers <- fire_length %>% 
  filter(length_of_fires < 75)

length_no_outliers <- ggplot(fire_length_no_outliers, aes(x = length_of_fires, y = decade)) +
  geom_density_ridges(aes(fill = decade), show.legend = F) +
  labs(x = 'Length of Fires', y= "") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))

length_all <- ggplot(fire_length, aes(x = length_of_fires, y = decade)) +
  geom_density_ridges(aes(fill = decade), show.legend = F) +
  labs(x = 'Length of Fires', y= "") +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))

length_season <- ggplot(fire_length, aes(x = alarm_month, y = length_of_fires)) +
  geom_point() +
  theme_minimal() +
  labs(x = "\nFire Start Month", y = "Length of Fires\n") +
  scale_x_continuous(expand = c(0,0),
                     lim = c(1, 12),
                     breaks = seq(1,12, by = 1),
                     labels = c("Jan", "Feb","Mar","Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))  +
  scale_y_continuous(expand = c(0,0)) 

# data for fire sizes ---------------------------------------------------------------------------
fire_size <- fire %>% 
  mutate(area_categorical = case_when(
    acres < 1000 ~ "200-1,000",
    acres < 5000 ~ "1,000-5,000",
    acres < 10000 ~ "5,000-10,000",
    acres < 20000 ~ "10,000-20,000",
    acres < 40000 ~ "20,000-40,000",
    acres < 100000 ~ "40,000-100,000",
    acres < 500000 ~ "100,000-450,000"
  )) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(decade)) %>% 
  st_as_sf() %>% 
  mutate(area_categorical = as.factor(area_categorical))  %>% 
  mutate(area_categorical = fct_relevel(area_categorical, levels = c("200-1,000", "1,000-5,000", "5,000-10,000",
                                                                     "10,000-20,000", "20,000-40,000", "40,000-100,000",
                                                                     "100,000-450,000"))) 
# data for fire causes --------------------------------------------------------------------------
fire_causes <- fire %>%
  mutate(fire_cause = case_when(
    cause == 0 ~ "Unknown",
    cause == 1 ~ "Lightning",
    cause == 2 ~ "Equipment Use",
    cause == 3 ~ "Smoking",
    cause %in% c(4,19) ~ "Campfire",
    cause == 5 ~ "Debris",
    cause == 6 ~ "Railroad",
    #cause == 7 ~ "Arson",
    cause == 8 ~ "Playing with Fire",
    cause %in% c(9, 16, 7, 12, 13) ~ "Miscellaneous",
    cause == 10 ~ "Vehicle",
    cause == 11 ~ "Powerline",
    #cause == 12 ~ "Firefighting Training",
    #cause == 13 ~ "Non-firefighting Training",
    cause == 14 ~ "Unknown",
    cause == 15 ~ "Structure",
    #cause == 16 ~ "Aircraft",
    cause == 17 ~ "Volcanic",
    cause == 18 ~ "Escaped Prescribed Burn"
    #cause == 19 ~ "Illegal Campfire"
  )) %>% 
  arrange(fire_cause) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(year)) 

fire_un <- fire_causes %>% filter(fire_cause == "Unknown")

#data for fire causes to compare natural vs human casued fires ------------------------------
fire_cause_simplified <- fire_causes %>% 
  mutate (fire_cause_simplified = case_when(
    fire_cause %in% c("Lightning", "Volcanic") ~ "Natural Cause",
    fire_cause %in% c('Equipment Use','Smoking','Campfire','Debris','Railroad','Arson','Playing with Fire',
                      'Miscellaneous','Vehicle','Powerline','Firefighting Training','Non-firefighting Training',
                      'Structure', 'Aircraft', 'Escaped Prescribed Burn', 'Illegal Campfire') ~ "Human Cause",
    fire_cause == "Unknown" ~ "Unknown"
  )) %>% 
  filter (fire_cause_simplified != "Unknown") 

# sum number of yearly fires
fire_causes_simplified_count <- fire_cause_simplified %>% 
  group_by(year, fire_cause_simplified) %>% 
  count() 

#sum yearly acres burned 
fire_causes_simplified_acres <- fire_cause_simplified %>% 
  group_by(year, fire_cause_simplified) %>% 
  summarise(yearly_acres_burned = sum(acres)) %>% 
  st_drop_geometry() 

#join data frames
fire_causes_simplified_count_acres <- inner_join(fire_causes_simplified_count, 
                                                 fire_causes_simplified_acres, by = c("year", "fire_cause_simplified"))

# graphs for server
count_plot <- ggplot(data = fire_causes_simplified_count_acres, aes(x = year, y = n)) +
  #geom_point(aes(color = fire_cause_simplified)) +
  geom_line(aes(color = fire_cause_simplified), show.legend = F) +
  geom_area(aes(fill= fire_cause_simplified), position ="identity") + # position="idendity" = critical componenet. otherwise the colored area is VERY off
  scale_fill_discrete(name = "Cause") +
  theme_minimal() +
  scale_y_continuous(expand = c(0,0),
                     lim = c(0, 125)) + 
  scale_x_continuous(expand = c(0,0),
                     lim = c(1910, 2020)) +
  labs(x = "\nYear", y = "Number of Occurances\n")

acres_plot <- ggplot(data = fire_causes_simplified_count_acres, aes(x = year, y = yearly_acres_burned)) +
  # geom_point(aes(color = fire_cause_simplified)) +
  geom_line(aes(color = fire_cause_simplified), show.legend = F) +
  geom_area(aes(fill= fire_cause_simplified), position ="identity") +
  scale_fill_discrete(name = "Cause") +
  theme_minimal() +
  scale_y_continuous(expand = c(0,0),
                     label = comma,
                     lim = c(0, 1250000)) + 
  scale_x_continuous(expand = c(0,0),
                     lim = c(1910, 2020)) +
  labs(x = "\nYear", y = "Acres Burned\n") 


# user interface ---------------------------------------------------------

ui <- navbarPage(
  "California Fire",
  theme = shinytheme("united"),
  tabPanel("Fire History",
           h1("Background"),
           p("In some places fire is a natural and essential component of the ecosystem, however fear of wildfire due to its uncontrollability and the threat it posed to human lives and essential industries, such as timber 
             industries, resulted in the passage of fire suppression policies in the early 1900s. While these policies were successful in reducing the number of wildfires, they had unintended consequences that have 
             changed the structure and composition of fire prone ecosystems, such as forests and chaparral. These consequences have created unnaturally dense vegetation which has resulted in larger, more frequent, 
             and more severe wildfires than historic norms."),
           p("This App explores trends in fire history for California. There have been over 21,000 recorded fires from mid 1880s to 2018 that have burned over 38 million acres; this represents 37.6% of land in the state."),
           h1("Data"),
           p("Fire perimeter polygons were obtained from the Fire and Resource Assessment Program (FRAP) from CalFire (https://frap.fire.ca.gov/frap-projects/fire-perimeters/).  Data is available from 1880s to 2018, 
             but there are relatively very few recorded observations from 1880s to the early 1900s. Thus, data from these years do not represent a complete record and comparisons from these years to later years should 
             be interpreted with caution.   To help with this issue, in some cases data from 1880s-early 1900s was omitted. While there were over 21,000 recorded fires, due to technical difficulties only fires over 200 acres, 
             which represents 9,584 fires, were included for analysis."),
           p("Recorded data included fire cause, fire start date, fire containment date, fire name, and fire size in acres. However, not every fire had records of all of these data points. Thus, some analyses include less fires 
             than are in the final dataset. The total number of fires used in an analysis is noted on each page."),
           mainPanel(plotOutput(outputId = "fire_facet_graph", height=1000, width=1000))),
  tabPanel("Fire Season and Fire Length",
           h3("Background"),
           p("This page explores trends in fire season and duration that a fire burned, aka fire length. Research has shown that fire season has changed compared to historic norms; fires are starting \
             earlier and ending later. It has been estimated that the fire season has increased by 75 days. Several factors are driving this change, of which climate change is considered the key driver. 
             Hotter temperatures, reduced snowpack, and earlier snowmelt create longer dry seasons with more drought stressed vegetation. These factors in addition to increased vegetation density due to fire supresssion
             also lengthen the duration of fires as more densely packed, drought stressed vegetation is more flamable and thus easier to spread."),
           h3("Data and Methods"),
           p("Alarm date refers to the day a fire started. This data was used to determine trends in fire season. Data on alarm date was available for
             2,480 fires (from the 9,584). There was no data on alarm date available pre-1920 and there were Very few fires from the 1920s-mid 1900s that contained this information. To avoid misinterpreting any results, fires from before 1970 were exlcuded from this 
             analysis."),
           p("Fire length was calculated by converting date data (mm/dd/yyyy) to day of the year (0-365) format, then subtracting containment 
             date (day the fire was declared contained) from alarm date. A few fires started in one year and ended in another. In this case, the following equation was used: (365 - alarm_date) + containment_date).
             A few fires had the containment day recorded as before the alarm day, so these were removed as it was assumed these data points were the result of human error. In total 2,477 fires had accurate data for both
             fire alarm date and containment date."),
           sidebarLayout(
             sidebarPanel(pickerInput(inputId = "select_summary_stat",
                                      label = "Select Summary Statistic to Explore",
                                      choices = c("Earliest Alarm Date " = "min_alarm", 
                                                  "Average Alarm Date" = "mean_alarm", 
                                                  "Last Alarm Date" = "max_alarm", 
                                                  "Distribution of All Alarm Dates" = "all_decade",
                                                  "Relationship Between Fire Length and Fire Season" = "length_and_season",
                                                  "Distribution of All Fire Lengths" = "length_graph_all",
                                                  "Distribution of All Fire Lengths With no Outliers" = "length_graph_no_outliers"),
                                      options(list(style = "btn-danger")))),
             mainPanel("",
                       plotOutput(outputId = "season_summary_graph")))),
  tabPanel("Fire Size",
           h3("Background"),
           p("This page explores how fire size has changed over the decades. Since fire suppression policies were enacted, vegetation density was allowed to accumulate for decades. Because of this
             fire size and severity has increased. Of California's largest wildfires (> 100,000 acres) 40% occurred after 2000 and 89% occurred after 1970. "),
           h3("Data and Methods"),
           p("Size was calculted for each polygon in ArcGIS. Units are in acres. There are few data points available from 1880s-early 1900s, so those should be 
             interpreted with caution."),
           sidebarLayout(
             sidebarPanel(radioButtons(inputId = "select_area",
                                       label = "Select Fire Size (Acres)",
                                       choices = c("200-1,000", 
                                                   "1,000-5,000", 
                                                   "5,000-10,000", 
                                                   "10,000-20,000", 
                                                   "20,000-40,000", 
                                                   "40,000-100,000", 
                                                   "100,000-450,000"))), #unique(fire_size$area_categorical)
             mainPanel(" ",
                       plotOutput(outputId = "area_graph"),
                       tableOutput(outputId = "area_sum_table"),
                       plotOutput(outputId = 'size_decades_map')))),
  navbarMenu("Fire Causes",
             tabPanel("All",
                      h3("Background"),
                      p("Recent large and deadly wildfires caused by powerlines typically dominate the news cycle, but the occurances of these large wildfires are realtively 
                        rare compared to the hundreds of small wildfires (< 1,000 acres) that occur every year but do not make the news. Therefore, less is known about the causes of smaller fires
                        and overall causes of historic fires and how those have changed over the years."),
                      h3("Data and Methods"),
                      p("There are 14 categories for causes of wildfires; exlcuding unknown category. There are 4,420 fires with fire causes recorded; 5,116 fires had an unknown cause."),
                      sidebarLayout(
                        sidebarPanel(" ",
                                     multiInput(inputId = "check_fire_causes",
                                                label = "Select Fire Cause(s) to explore:",
                                                choices = c(unique(fire_causes$fire_cause))),
                                     #checkboxGroupInput(inputId = "check_fire_causes",
                                     #                  label = "Select Fire Cause(s) to explore:",
                                     #                 choices = c(unique(fire_causes$fire_cause))),
                                     sliderInput(inputId = "date_fire_causes1",
                                                 label = "Select Range of Year(s)",
                                                 min = 1880, max = 2019, value = c(1880,2019),
                                                 sep = "")),
                        mainPanel("",
                                  plotOutput(outputId = "fire_causes_graph"),
                                  tableOutput(outputId = 'fire_causes_table'),
                                  plotOutput('fire_causes_map')))),
             tabPanel("Natural vs Human Caused",
                      h3("Background"),
                      p("This page compares fire occurances and acres burned between human caused and natural fires. Natural fires and fires started by lightening or volcanic 
                        activity. Fires with unknown causes were excluded from this analysis."),
                      sidebarLayout(
                        sidebarPanel(" ",
                                     radioButtons(inputId = "select_count_area",
                                                  label = "Pick:",
                                                  choices = c("Total Annual Fires" = "n",
                                                              "Annual Acres Burned" = "yearly_acres_burned"))),
                        mainPanel("",
                                  plotOutput(outputId = "fire_causes_simplified_graph"),
                                  tableOutput(outputId = "fire_simplified_decades_summary")))))
             )

#server --------------------------------------------------------------------

server <- function(input, output) {
  
  #intro facet wrap graph
  output$fire_facet_graph <- renderPlot({
    ggplot() +
      geom_sf(data = ca_border, color = "grey80") +
      geom_sf(data = fire_facet, fill = "red4", color = "red4") +
      theme_classic() +
      theme_map () +
      facet_wrap(~decade)
  })
  
  # output for summary stats of fire season
  output$season_summary_graph <- renderPlot({
    if (input$select_summary_stat == "min_alarm") {print(min_season)}
    if (input$select_summary_stat == "mean_alarm") {print(mean_season)}
    if (input$select_summary_stat == "max_alarm") {print(max_season)}
    if (input$select_summary_stat == "all_decade") {print(season_ggridges)}
    if(input$select_summary_stat == "length_graph_all") {print(length_all)}
    if(input$select_summary_stat == "length_graph_no_outliers") {print(length_no_outliers)}
    if(input$select_summary_stat == "length_and_season") {print(length_season)}
  })
  
  #data frame for the number of fires that occurred per decade grouped by fire size
  area_decades_count <- reactive ({
    fire_size %>% 
      filter(area_categorical == input$select_area) %>% 
      group_by(decade, area_categorical) %>% 
      count() %>% 
      ungroup(decade, area_categorical)  
    # complete(decade, area_categorical, fill = list(n = 0))
  })
  
  #graph output for fire size per decade 
  output$area_graph <- renderPlot({
    ggplot(data = area_decades_count(), aes(x = decade, y = n)) + 
      geom_col(fill = "red4", alpha= 0.7) +
      scale_x_discrete(expand = c(0,0),
                       drop = F) +
      scale_fill_discrete(drop = F) +
      scale_y_continuous(expand = c(0,0)) +
      labs (x = "\nTime", y = "Number of Fires\n") +
      theme_classic() +
      theme(plot.margin = unit(c(5,5,5,5), "lines"))
  })
  
  #data frame to total ALL the fire sizes that occurred throughout the fire history
  area_decades_sum <- fire_size %>% 
    group_by(area_categorical) %>% 
    count() %>% 
    st_drop_geometry() %>% 
    rename("Total Number of Fires" = n) %>% 
    rename ("Fire Size" = area_categorical)
  
  #table for the total fire sizes
  output$area_sum_table <- renderTable(
    area_decades_sum, 
    striped =T, 
    bordered = T,
    align = 'c'
  )
  
  # map for size data   
  output$size_decades_map <- renderPlot({
    ggplot() +
      geom_sf(data = ca_border, color = "grey80") +
      geom_sf(data = area_decades_count(), fill = "red4", alpha = 0.8, color = NA) +
      theme_map()
  })
  
  # data frame for all fire causes
  fire_causes_count <- reactive({
    fire_causes %>% 
      filter(fire_cause %in% input$check_fire_causes) %>% 
      filter(year >= input$date_fire_causes1[1]) %>%
      filter(year <= input$date_fire_causes1[2]) %>% 
      group_by(fire_cause, year) %>% 
      count()
  })
  
  # graph for fire causes
  output$fire_causes_graph <- renderPlot({
    ggplot(data = fire_causes_count(), aes(x = year, y = n)) +
      geom_point(aes(color = fire_cause)) +
      geom_line(aes(color = fire_cause)) +
      labs(x = "\nYear", y = "Number of Occurances\n") +
      theme_minimal() +
      scale_color_discrete(name = "Fire Cause") +
      expand_limits(y = 0) +
      scale_y_continuous(expand = c(0,0)) + #limits = c(0,max(variable)+10); input$date_fire_causes1[2], max(fire_causes_count$year)
      scale_x_continuous(expand = c(0,0)) 
  })
  
  # map for fire causes
  output$fire_causes_map <- renderPlot({
    ggplot() +
      geom_sf(data = ca_border) +
      geom_sf(data = fire_causes_count(), aes(fill = fire_cause), color = NA) +
      theme_map() +
      scale_fill_discrete(name="Cause") 
  }) 
  
  # sum fire occurances and acres burned
  fire_causes_count_sum <- reactive ({
    fire_causes %>% 
      filter(fire_cause %in% input$check_fire_causes) %>% 
      filter(year >= input$date_fire_causes1[1]) %>%
      filter(year <= input$date_fire_causes1[2]) %>% 
      group_by(fire_cause) %>% 
      summarise(sum_fires = n(),
                sum_acres = sum(acres)) %>% 
      mutate(sum_acres = format(round(sum_acres), big.mark=","),
             sum_fires = format(sum_fires, big.mark=",")) %>% 
      st_drop_geometry() %>% 
      rename("Total Fire Occurances" = sum_fires,
             "Total Acres Burned" = sum_acres,
             "Fire Cause" = fire_cause)
  })
  
  # table for total fire causes
  output$fire_causes_table <- renderTable({
    fire_causes_count_sum()
  })
  
  output$fire_causes_simplified_graph <- renderPlot({
    if(input$select_count_area == "n") {print(count_plot)}
    else {print(acres_plot)}
  })
  
}


#run shiny app --------------------------------------------------------------

shinyApp(ui = ui, server = server)


####################################### Shiny gganimate ###############################################

library(tidyverse)
library(janitor)
library(sf) 
library(gganimate)
library(transformr) #need?
library(magick) #need?
library(here)
library(raster) #need?
library(tmap)
library(lubridate)
library(dplyr)
library(lwgeom)
library(ggthemes)


ca_border <-  read_sf(here::here("Arc_data", "ca_state_border"), layer = "CA_State_TIGER2016")
fire <- read_sf(here::here("Arc_data", "fire_perimeter_shpfile"), layer = "fire_perimeters" ) 


fire_animate <- fire %>% 
  clean_names() %>%  
  filter(!is.na(year)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year >= 1936) %>% 
  filter(year <= 1940) %>% 
  st_as_sf() %>% 
  mutate (fire_name = str_to_title(fire_name))  %>% 
  dplyr::filter (acres > 6000)  %>% 
  group_by(year) %>% 
  mutate(cumulative_acres_burned = cumsum(acres)) %>% 
  ungroup(year)
# st_simplify(dTolerance = 100)  


ggplot(data = fire_animate) +
  geom_sf(data = ca_border, color = "grey80") +
  geom_sf(data = fire_animate, fill = "red", alpha = 0.8, color = "red") +
  theme_classic() +
  theme_map() +
  labs(title = "Year: {round(frame_time,0)}")  +
  transition_time(year) +
  shadow_mark(alpha = 1)

class(fire)
class(fire$year)
st_crs(fire) #none

```
error for map on first page: 'x' and 'units' must have length > 0 ; 
Error in points[[1]] : subscript out of bounds


#trial 2: use transition states. try to Use year as a factor instead of numeric for this
```{r}
fire_animate <- fire %>% 
  clean_names() %>%  
  filter(!is.na(year)) %>% 
  filter(!is.na(decade)) %>% 
  mutate(year = as.numeric(year)) %>% 
  #filter(year >= 1990) %>% 
  #filter(year <= 1997) %>% 
  filter(year != 1930, 1931, 1985, 1985, 1986, 1998, 1999) %>% 
  st_as_sf() %>% 
  mutate (fire_name = str_to_title(fire_name))  %>% 
  dplyr::filter (acres > 200)  %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate (decade = case_when(
    year < 1900 ~ "1890",
    year < 1909 ~ "1900",
    year < 1919 ~ "1910",
    year < 1929 ~ "1920",
    year < 1939 ~ "1930",
    year < 1949 ~ "1940",
    year < 1959 ~ "1950",
    year < 1969 ~ "1960",
    year < 1979 ~ "1970",
    year < 1989 ~ "1980",
    year < 1999 ~ "1990",
    year < 2009 ~ "2000",
    year < 2019 ~ "2010"
  )) %>% 
  mutate(decade = as.numeric(decade))

#year_glue <- str_glue_data(fire_animate2, "Year: {year}")

ggplot(data = fire_animate) +
  geom_sf(data = ca_border, color = "grey80") +
  geom_sf(data = fire_animate, aes(color = decade, fill = decade), alpha = 0.8) +
  theme_classic() +
  theme_map() +
  labs(title = "Year: {round(frame_time,0)}") +
  #labs(subtitle = "Acres Burned: {} ")  +
  transition_time(decade) +
  # transition_states(year) +
  #labs(title = str_glue_data(fire_animate2, "Year: {year}")) +
  ease_aes("linear") +
  shadow_mark(alpha = 0.3)

animate(anim_plot, fps = 10, end_pause = 30)


####################################### ESM 244: Assigment 2 Task 3 ####################################

### Explore Landcover and Watersheds of the Hawaii Islands

```{r}
# load packages ---------------------------------------------------------

library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(raster)
library(here)
library(dplyr)
library(paletteer)
library(leaflet)
library(tmaptools)

# laod data -------------------------------------------------------------

watershed <- read_sf(dsn = here("Watersheds"), layer = "Watersheds")
landcover <- read_sf(dsn = here('Land_Use_Land_Cover_LULC'), layer = "Land_Use_Land_Cover_LULC")

```

<br> 
  
  ## Landcover of the Hawaiian Islands
  ```{r}
# subdata: just landcover column and condense the landcover types
landcover_sub <- landcover %>% 
  dplyr::select (landcover) %>%  
  filter(landcover !="0") %>% 
  mutate(condensed_landcover = case_when(
    landcover %in% c("Industrial", "Industrial and Commercial Complexes", "Commercial and Services") ~ 'Industrial and Commerical',
    landcover %in% c ("Cropland and Pasture", "Other Agricultural Land", "Confined Feeding Operations", 
                      "Orchards, Groves, Vineyards, Nurseries and Ornamental Horticultural Areas") ~ 'Agriculture and Pastureland',
    landcover %in% c("Herbaceous Rangeland", "Mixed Rangeland", "Shrub and Brush Rangeland") ~ "Rangeland",
    landcover %in% c("Lakes", "Reservoirs", "Streams and Canals") ~ "Water", 
    landcover %in% c("Other Urban or Built-up Land", "Mixed Urban or Built-up Land") ~ "Urban",
    landcover %in% c("Forested Wetland", "Nonforested Wetland", "Bays and Estuaries") ~"Wetland, Bays, and Estuaries",
    landcover == "Residential" ~ "Residential",
    landcover == "Evergreen Forest Land" ~ "Forest",
    landcover == "Transportation, Communications and Utilities" ~ "Transportation, Communications and Utilities",
    landcover %in% c("Sandy Areas Other than Beaches", "Beaches") ~"Beach",
    landcover == "Transitional Areas" ~"Transitional Areas", 
    landcover %in% c("Bare Exposed Rock", "Mixed Barren Land") ~ "Bare Rock",
    landcover == "Strip Mines, Quarries, and Gravel Pits" ~ "Strip Mines, Quarries, and Gravel Pits"
  ))


# set tmap mode to interactive
tmap_mode("view")

# create map
tm_shape(landcover_sub) +
  tm_polygons('condensed_landcover', palette = c("wheat4", "grey60", "khaki1", "springgreen4", "darkmagenta", "yellow3", "plum", "tan", "red4", "darkgoldenrod", "purple", "blue", "turquoise2"),title = "Landcover") +
  tm_basemap(leaflet::providers$OpenStreetMap)


```

<br>
  
  ##Hawaii Watersheds
  ```{r}

# create tmap
tm_shape(watershed) +
  tm_borders(col = "purple") +
  tm_fill("hucarea", title = "Area (sqm)", style="fisher") +
  tm_basemap(leaflet::providers$OpenStreetMap)

```

############################### ESM 244: Assignment 2 task 2 ########################################

### Introduction

This report explores time series data of endangered Steelhead salmon (Figure 1) passage across the Bonneville Dam, which is located on the Columbia River along the Oregon and Washington Border.. Salmon passage is recorded daily from 1939-2019.Data was obtained from Columbia Research Basin^1^; more information on the project and additional data can be found [here](http://www.cbr.washington.edu/dart/query/adult_graph_text). 

![](sthl_trout.jpg)

***Figure 1:** Two adult Steelhead salmon. Photo Credit: [NOAA](https://www.fisheries.noaa.gov/species/steelhead-trout)*
  
  <br>
  
  ```{r}
# load packages ------------------------------------

library(tidyverse)
library(dplyr)
library(lubridate)
library(tsibble)

# load data-----------------------------------------

steelhead <- read_csv("cbr_fish_passage_bonneville_allyrs_steelhead.csv") %>% 
  dplyr::select(year, 'mm-dd', value) %>% 
  rename(ddmm = "mm-dd") %>% 
  separate(ddmm, remove=F, sep="-", into=c("date", "month")) %>% 
  unite(ddmm, c("date", "month"), sep="", remove=F) %>% 
  unite(yyyyddmm, c("year", "ddmm"), sep="", remove=F)  %>% 
  mutate(yyyyddmm = lubridate::ydm(yyyyddmm)) %>% 
  mutate(ddmm_new = as.Date(ddmm, format = "%d%b")) %>% #use b for month, lowercase b for month in word format and capital B for month in number format
  mutate(month = fct_relevel(month, levels=month.abb)) 

```

<br>
  
  ```{r, include = F}
# other trials for lubridating the date


#lubridate::parse_date_time
#mutate(yyyddmm = str_c(data$ddmm, data$year, sep="-"))
#separate, unite, then lubridate by ymd, then aggreagte by month

# note from jessica: 
#I did some digging and it looks like the way we were converting the dates (using lubridate::parse_date_time(date)) creates a "POSIXct" class vector rather than a straight "Date" class vector, which is what seems to be the issue when setting the index for as_tsibble(). If you instead convert straight to a "Date" (via the base as.Date() or the lubridate::dmy() functions) you should be able to coerce the data to a tsibble using that new date vector as the index. 

#(In lab 5 we converted from the "POSIXct" to "Date" using the yearmonth() function when we didn't have daily data to worry about)
```

<br>
  
  
  
  ### Time Series Plot of the Daily Steelhead Passages
  
  ```{r}
#plot 

ggplot(steelhead, aes(x = yyyyddmm, y = value)) +
  geom_line(color = "turquoise2") + 
  scale_y_continuous(lim =c(0, 40000), 
                     expand = c(0,0),
                     labels=scales::comma) +
  scale_x_date(expand = c(0,0)) +
  labs(x = "\nDate", 
       y = "Daily Steehead Salmon Passage\n", 
       title = "Daily Salmon Passage",
       subtitle = "1939-2019") +
  theme_classic() 

```
***Figure 2:** Daily number of Steelhead  passage at Bonneville Dam from January 1, 1939 to December 31, 2019. Daily Steelhead passage is very low for most days out of the year, but a several consecutive days have very high Steelhead passage.*
  
  <br>
  
  ### Time Series Plot of the Monthly Steelhead Passages
  
  ```{r}

# subdata: average the monthly values for each year

steelhead_month_year <- steelhead %>% 
  mutate(month = fct_relevel(month, levels=month.abb)) %>% 
  unite(month_year, c("year", "month"), sep = " ", remove = F) %>% 
  group_by(month, year) %>% 
  summarize(monthly_count = mean(value, na.rm =T))

# plot

ggplot(steelhead_month_year, aes(x = month, y = monthly_count, group = year)) +
  geom_line(aes(color = year)) +
  labs(x = "\nYear", 
       y = "Monthly Average\n", 
       title = "Average Monthly Passage of Steelhead Salmon",
       subtitle = "1939-2019") +
  theme_bw() +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(lim =c(0, 9000), 
                     expand = c(0,0),
                     labels=scales::comma,
                     breaks = seq(0, 9000, by = 3000)) +
  scale_color_continuous(name = "Year")


```
***Figure 3:** Time series analysis of the average number of Steelhead passing per month the Bonneville Dam from 1939 to 2019. Steelhead passage is relatively low from November to beginning of June. During the summer months salmon passage drastically increases and typically peaks in July or August before sharply dropping by October.*
  
  <br>
  
  ### Annual Steelhead Passage 
  
  ```{r, fig.align="center"}

# subdata: sum values for each year

steelhead_year <- steelhead %>% 
  group_by(year) %>% 
  summarize(yearly_count = sum(value, na.rm=T))

#plot  

ggplot(steelhead_year, aes(x = year, y = yearly_count)) +
  geom_point(color = "purple") +
  geom_line(color = "purple") +
  theme_bw() +
  labs (x = "\nYear", 
        y = "Annual Sum\n", 
        title = "Annual Passage of Steelhead Salmon",
        subtitle = "1939-2019") +
  scale_x_continuous(lim = c(1939, 2020),
                     expand = c(0,0)) +
  scale_y_continuous(lim =c(50000, 650000), 
                     expand = c(0,0),
                     labels=scales::comma) +
  coord_cartesian(clip="off", xlim=c(1939, 2020)) +
  theme(plot.margin=unit(c(1,5,1,1), "lines"))

```
***Figure 4:** Annual sum of Steelhead that passed the Bonneville Dam in STATE. Annual passage remained realitvely constant until the mid-1980s.Average salmon passage increased from the 1980s until the mid 2010s. Salmon passage continously declined and reached the lowest annual salmon passage in the study's history in 2019. *

<br>

### Citations
Columbia River DART, Columbia Basin Research, University of Washington. (2019). Adult Passage Graphics & Text. Available from http://www.cbr.washington.edu/dart/query/adult_graph_text


######################################### ESM 244: Lab 10 #####################################


```{r}
library(tidyverse)
library(janitor)
library(corrplot)
library(beepr)
library(praise)
library(stargazer)
library(sf)
library(gganimate)
library(transformr)
library(magick)
```
### Objectives

- multiple linear regression
- first map with sf and ggplot


### 1. learn new fun r functions

```{r}
beep(3) # plays sound when a code is done running can choose number from 1-12 to pick diff sounds
praise() # gives random phrases of praise'
praise("you are totally ${adjective}! Super ${EXCLAMATION}") # use ${ } with function of praise inside to create custom phrases with cycled adjectives, etc. 

```


### 2. multiple linear regression: SLO home prices

```{r}
#load data
homes <- read_csv("slo_homes.csv") %>% 
  clean_names()

# sub data: only include data from 3 cities
homes_sub <- homes %>% 
  filter(city %in% c("San Luis Obispo", "Atascadero", "Arroyo Grande"))

```

are there correlations btw variables that we'd consider ewhile trying to model home price?

```{r}
homes_cor <- cor(homes_sub [2:5]) # brackets= means only consider these columns
homes_cor

corrplot(homes_cor,
method = "ellipse",
type = "upper")
#=visualize correlation btw variables. have to create the correltaion matrix then use that data in corrplot(), cant put original data in corrplot()
# corrplot is redudant so add in type parameter to limit the graph so only shows the relationship btw variables once instead of twice (which is confusing)


```

also use scatterplots to examine correlations
```{r}
#ggplot ()+ geom_point()

```

lets start with a complete model (city, bedrooms, bathrooms, sq ft, and sale status)

```{r}
homes_lm <- lm(formula = price ~ city + bathrooms+ bedrooms + sq_ft + status, data=homes_sub)

summary(homes_lm)

# can reorder categorical variables so the one you want to be the reference to befirst in the list, bc r has the firt variable in the categorical var to be listed as the reference 
```

intercept= 184,130

equation: price = 184,130 + 167396(atascadero) + 31018(slo) - 161645(bed) + 48692(bath) + 389(sq_ft) + 303964(regular) - 19828(short)

concerning variable= bedrooms. we expect home price to decrease by $160,000 for every additional room, which, bc weve done lit review adn understand our variables and how they influence home rpice outside of this model, we know is unrealistic. 

bathrooms, baedrooms, and sq ft is essentially getting at the same thing: home size, so this colliniearity is likely causing the negative bedroom assoc. so best to choose just one var

p= 0, so out model predicts home price sig bettr than random chance

```{r}
homes_lm2 <- lm(price ~ sq_ft + status + city, data=homes_sub)

summary(homes_lm2)
```

AIC: compare models (should do this after picking varibales that make the most conceptual sense)

```{r}
AIC(homes_lm)
AIC(homes_lm2)
```

model  1 has lower AIC--> good example why shouldnt base decision on which model to choose, just on AIC! conceptually the first model does not make sense bc of the negative bedroom coefficient!!! 

okay to use aic to pick models when don't know which model is better. 

### check assumptions for normality and homoscedasticity 

for linear regression, we want to look at the distribution of the RESIDUALS!!! NOT the dist of the actual data points


```{r}
plot(homes_lm) 
# looks like assumption of constant variances of residuals (ie homoscedasicity) is met (dont let a couple outliers sway your opinion )
# assumption of residual normality is also met

plot(homes_lm2)

# the numbers by the outlier plots refers to the row number in the data, so can look at the outliers
```

cooks dist = measure of leverage a single point has on model fit . vaue >0.5 means that value has unusually high impact on model fit. Doesnt mean you can just remove that point, should consider other factors before you do that. 

make regression table using stargazer (doing it by hand is a pain)
```{r, results="asis"}

# use stargazer to report results of linear regression
stargazer(homes_lm2, type='html')  


```

ressults='asis' tells r to ...put table in knitted html file?
  
  lets make some predicitons for home price based on a new data frame. make sure the variables created for the new data frame match the vairables that the model will be looking for (ie some capitalization, spelling, special characters, etc)
    
    ```{r}
new_df <- data.frame(
  city = rep(c("San Luis Obispo", "Arroyo Grande", "Atascadero"), each=10),
  sq_ft = rep(seq(1000, 5000, length=10)),
  status = "Regular"
)
```
for categorical variables: rep=reapeat the observations 10 times
word before = sign is the column header
for numeric values: rep=repeat, seq=sequence, #1, #2 = means i want values between 1000 - 5000sqft, length = means the spacing desired between values 


Now make preditions

```{r}
predict_df <-  predict(homes_lm2, newdata=new_df)
predict_df

#bind together the new df with the prediciotns so its easier to see
full_data <- data.frame(new_df, predict_df)
full_data
```

now plot in ggplot 

```{r}
ggplot() +
  geom_point(data=homes_sub, 
             aes(x=sq_ft, y=price, color=city, pch=city)) +
  geom_line(data = full_data,
            aes(x = sq_ft, 
                y = predict_df,
                color = city)) +
  scale_color_manual(values = c("orange", "magenta", "black")) +
  theme_light()
```

### our first map (thanks sf package)

great bc has sticky geometries. so when get bunch of spatial data with lots of attributes. So when wrangle data (ex/ select, filter, etc), all the original attributes will stick with the data point even if it doesnt show in the subdata

sf = useful for gis data and layers

```{r}
dams <- read_csv("ca_dams.csv") %>% 
  clean_names() %>% 
  drop_na(longitude) %>% 
  drop_na(latitude) %>% 
  drop_na(year_completed)
```


r doesnt recognize lat and longitude as spatial data. --> so convert our data frame to an sf object using st_as_sf

```{r}
dams_sf <- st_as_sf(dams, coords=c("longitude", "latitude"))

st_crs(dams_sf) <- 4326  # = computes/assigns coordinate system of the spatial data (lat, long)

class(dams_sf)
```
make sure to input the actual column names for lat and long. some times theyre called other things like lat and long



```{r}
#plot lat and long
plot(dams_sf)

# in and of its self, the points are not as intersting/meaningful, but will be when we add a spatial map to overlay the points on (ie map of CA)

#add map (CA boundary)
ca_border <-  read_sf(here::here("ca_state_border"), layer="CA_State_TIGER2016") 
#shape files tend to have several files, so this code lets you read in all files with the same suffix so dont have to load the shapefiles supporting files one by one

#view(ca_border)# --> = data frame for the shapefile appears as a single row. spatial data is in last column. likely to have more rows is have more states. but depends on the shapefile

plot(ca_border)
```



```{r}
ggplot() +
  geom_sf(data=dams_sf, color="orange", size=1, alpha=0.4)

ggplot()+
  geom_sf(data=ca_border, fill="purple", color="green") 

ggplot()+
  geom_sf(data=ca_border, fill="purple", color="green") +
  theme_void()

ggplot() +
  geom_sf(data=ca_border) +
  geom_sf(data=dams_sf) +
  theme_bw()
```

### gganimate

animated maps dont show up in rmarkdown, so have to knit afterwards
```{r}

ggplot() +
  geom_sf(data=ca_border) +
  geom_sf(data=dams_sf, 
          color="blue", 
          alpha=0.5,
          size=1) +
  theme_void() +
  labs(title='Year: {round(frame_time,0)}') + # shows title so audience knows what the points represent. frame time is not a value we created but is an sf function.
  transition_time(year_completed) +
  shadow_mark()  # default is to have points dissapear after they appear, this code makes sure they stay visable



```

################################## ESM 206: HW 2 task 1 ##################################


# Load Packages
```{r}
##load packages --------------------------------------------------------------------------------------------

library(hexbin)
library(tidyverse)
library(janitor)
library(dplyr)
library(tidyr)

## load data and clean names --------------------------------------------------------------------------------
pollution <- read_csv("ca_pollution_burden.csv") %>% 
  clean_names()

pollution_demographics <- read_csv("ca_census_demographics_2010.csv") %>% 
  clean_names()

## Join the two data sheets together by census tract number--------------------------------------------------

# step 1: make sure both data sets have same colum name for column based on the join
pollution_demo_new <- pollution_demographics %>% 
  rename(census_tract=census_tract_number)

# step 2: join data based on census tract number
pollution_join <- pollution %>% inner_join(pollution_demo_new)

#step 3: check waht values weren't kept
pollution_antijoin <- pollution %>% anti_join(pollution_demo_new)
```


# Mean CES 3.0 Score for 10 Worst Counties 

```{r}

#calculate mean cse 3.0 score by CA county

# find mean by county
mean_ces3.0 <- pollution_join %>% group_by(california_county) %>% 
  summarize(mean_ces3.0_county = mean(ces_3_0_score, na.rm=T)) %>% 
  arrange (-mean_ces3.0_county)


# select top 10 counties with worst (highest) ces 
top_mean_ces <- mean_ces3.0 %>% top_n(10)

# all ces scores for top 10 counties 

all_ces_top <- pollution_join %>% 
  select (ces_3_0_score, california_county) 

# graph to visualize the top 10 worst counties

ggplot(top_mean_ces, aes(x=fct_reorder(california_county, mean_ces3.0_county), y=mean_ces3.0_county )) +
  geom_col(fill="blue", color="orange") +
  labs(y= "Mean CES 3.0 Score", x="California County", title="Mean CES 3.0 Scores for 10 Worst Counties in California" ) +
  theme_minimal() +
  coord_flip()


```

# Asthma Incidents Compared to Percent White in a Population
```{r}

# step 1: select columns
white_asthma <- pollution_join %>%  select(asthma, white_percent)

#step 2: graph 
ggplot(white_asthma, aes(x=white_percent, y=asthma)) +
  geom_hex() +
  labs(x="% White in Population", y="# Asthma Incidents", title="Number of Asthma Incidents Compared to Percent White in a Population" )


```
Graph description: While there is a lot of variation in the distrubtion, overall there is a negative relationship between the percent of white people in the population and the number of asthma incidents; as the percentage of white people in the population increase, the number of asthma incidents decreases. The graph shows that a high proportion of observations occured where there was a high percentage of white people in a community (around 75%) and low number of asthma incidents (approx. 45 incidents). 




### The following graph explores the relationship between poverty levels and CES 3.0 Scores.In this study, poverty is defined as living two times below the federal poverty level.  

```{r}

ggplot(pollution_join, aes(x=poverty, y=ces_3_0_score))+
  geom_hex() +
  labs(x="% of the Population Living in Poverty", y="CES 3.0 Score", title="Relationship Between Poverty and CES 3.0 Score") +
  theme_minimal()


```

##################################### ESM 206: HW2 Task 2 #######################################


```{r, echo=F, message=F, warning=F}

# load packages -------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tidyverse)
library(janitor)

# load data and clean names---------------------------------------------------------------------------------

plastic <- read_csv("plastic_rivers.csv") %>% 
  clean_names() %>% 
  rename (mp_model2=microplastic_load_model_2_tons_y_1)



```

## Part 1: Calculate the annual contribution of rivers to receiving seas, then determine the 15 seas with the most micoplastic inputs.

```{r, echo=F, message=F, warning=F}

# step 1: determine annual contribution of microplastics from contributing rivers to receiving seas and then only include top 15 seas most impacted by microplastics
plastic_sea_top <- plastic %>% 
  group_by(receiving_sea) %>% 
  summarize (
    total_mp = sum(mp_model2, na.rm=T)
  ) %>% 
  top_n(15)

#step 2: plot 15 seas most impacted by microplastics
ggplot(plastic_sea_top, aes(x=fct_reorder(receiving_sea,total_mp), y=total_mp)) +
  geom_col(fill="blue")  +
  coord_flip()+
  labs(x="Receiving Sea", y="Microplastic Load (tons/year)", subtitle="15 Seas with Highest Annual Microplastic Load From Contributing Rivers")+
  theme_minimal()



```

## Part 2: Determine which rivers are contributing the most microplastics and macroplastics to the Gulf of Mexico.

```{r,  echo=F, message=F, warning=F}

#step 1: determine which 5 river catchments are contributing the greatest total plastic loads to the Gulf of Mexico
plastic_gulf <- plastic %>% 
  filter(receiving_sea=="Gulf of Mexico") %>% 
  mutate(total_plastic=(mp_model2 + macroplastic_load_tons_y_1)) %>% 
  select(river, total_plastic) %>% 
  top_n(5)

# step 2: make graph
ggplot(plastic_gulf, aes(x=fct_reorder(river, -total_plastic), y=total_plastic)) +
  geom_col(fill="violet") +
  labs(x="River", y="Total Plastic Load (tons/year)", title="5 Biggest Contributors of Plastic (Macro + Mircoplastic) to the Gulf of Mexico") +
  theme_minimal()


```
#################################### ESM 206: HW 3 task 1 #####################################


library(here)
library(dplyr)
library(tidyverse)
library(janitor)
library(directlabels) 
library(plotrix) 

# load data and clean names----------------------------------------------------------------------------------------

mono_lake <- read_csv("Mono Lake Annual Levels.csv", skip= 5)  %>% 
  clean_names()

# create Label columns and names for ine graphs -----------------------------------
# had to do its this way since most tutorials only showed how to create line names at the end of lines when there were multiple lines (ie groups) in the same column.

mono_lake$label = "Mono Lake Level"
mono_lake$abline="Stable Level"


# Beautiful Graph of Mono lake levels from 1850-2017 --------------------------------------------------

ggplot(mono_lake ,aes(x=year, y=lake_level_feet_above_sea_level))+
  labs(x="\nYear", 
       y="Mono Lake Levels (ft above sea level)\n", 
       title="Historical Mono Lake Levels", subtitle="1850-2017")+    # axis names and title
  geom_line(color="light blue", size=1.25) +                          # lake levels line
  geom_line(aes(y=6377), linetype="longdash") +                       # abline for land bridges line
  geom_point(aes(x=1941, y=6417), 
             size=4.5,
             shape=18) +                                              # 1941 point on line
  annotate("text", 
           label="1941: Los Angeles begins\n diverting water", 
           x=1950, y=6418,
           size=3.5, 
           fontface=1, 
           vjust=0, hjust=0.25) +                                     # 1941 point text 
  geom_point(aes(x=1982, y=6372.8), 
             size=4.5, 
             shape=18) +                                              # 1982 point on line
  annotate("text", 
           label="1982: Mono Lake Tufa\n State Reserve created",
           x=1982, y=6370,
           vjust=0.8,size=3.5) +                                      # 1982 point text
  scale_x_continuous(lim=c(1850, 2017), 
                     expand=c(0,0)) +                                 # x axis limits
  scale_y_continuous(lim=c(6360,6430), 
                     expand=c(0,0), 
                     breaks=c(6370, 6390, 6410, 6430)) +        # y axis limits and breaks
  annotate("text",
           label="Level at which land\n bridges form", 
           x=Inf, y=6374, 
           hjust=-0.06, 
           size=3.5,
           fontface=2) +                                              # text for stable line outside graph
  annotate ("text",
            label="Mono Lake Level",
            x=Inf, y=6382.47, 
            hjust=-0.08,
            size=3.5,
            fontface=2) +                                             # text for lake levels outside of graph
  theme_classic() +
  coord_cartesian(clip="off", xlim=c(1850, 2017)) +                   # allows text to be written outside the graph
  theme(plot.margin = unit(c(1,10,1,1), "lines"))                      # graph margins. allows you to add text outside the main graphing area# background


# save graph ----------------------------------------------------------------------------------------

ggsave("Best graph_Mono Lake Levels.png")


####################################### ESM 206: HW 3 task 2 ##########################################


space <- read_csv("space_launches.csv")

#make subset data: number of successful launches per agency type----------------------------

agency_success <- space %>% 
  select(agency_type, category) %>% 
  filter(agency_type %in% c("private", "state")) %>% 
  dplyr::group_by(agency_type, category) %>% 
  count()

# worst graph --------------------------------------------------------------------------------------

ggplot(agency_success, aes(x=agency_type, y=n, width=1)) + 
  geom_col(aes(fill=fct_rev(category)), alpha=0.9) +
  labs(x="agency type (excluding start ups)",
       y="Frequency",
       title="The number of successful space launches by private and state 
      agencies, exlcuding start ups, from\n the years of 1957-2018") +
  theme_dark() +
  coord_flip() +
  scale_y_continuous(breaks=seq(0,5000, by=150)) +
  geom_text(aes(label=n), size=5, color="purple", angle=300) +
  theme(axis.text.x = element_text(angle = 15), 
        axis.text.y = element_text(angle = 45),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "yellow"),
        legend.position="bottom")+
  scale_fill_discrete(name="Successful?",
                      labels=c("Yes", "unsuccessful"))

#save graph -------------------------------------------------------------------

ggsave("worst graph_space data.png")



###################################### ESM 206: HW 4 ########################################

<center>
  
  ![](figures/wikipedia_lobster_Ed_Bierman.jpg)
</center>
  ***Figure 1:** California spiny lobsters (*Panulirus interruptus*) emerging from under a rock. Photo credit: Ed Bierman, [Wikipedia](https://en.wikipedia.org/wiki/California_spiny_lobster).* 
  
  
  MPAs are sections of the ocean reserved for conservation of ecosystems, habitats, and species (WildAid 2018). While MPAs allow some activities within their borders, one of their main goals is to increase wildife populations by restrcting or eliminating removal of species (WildAid 2018). This study report examines how lobster size and abundance has changed in two MPAs compared to three non-MPAs along the Santa Barbara Coast.  

<center>
  
  ![](figures/lobster_survey_sites.jpg)

library(tidyverse)
library(janitor)
library(here)
library(tidyr)
library(kableExtra)
library(effsize)
library(dplyr)

# Read in data and clean/wrangle
# Assign site name to 4 letter site code
# Create new column for MPA status

lobster_abundance_sbc <- read_csv(here::here("data", "lobster_abundance_sbc_lter.csv"),
                                  na = "-99999") %>%
  janitor::clean_names() %>%  
  mutate(site_name_long = case_when(
    site %in% c("CARP") ~ "Carpinteria" ,
    site %in% c("IVEE") ~ "Isla Vista",
    site %in% c("AQUE") ~ "Arroyo Quemado",
    site %in% c("MOHK") ~ "Mohawk", 
    site %in% c("NAPL") ~ "Naples"))  %>% 
  mutate(MPA = case_when( 
    site %in% c("IVEE", "NAPL") ~ "MPA",
    site %in% c("AQUE", "CARP", "MOHK") ~ "Non MPA"))

```


```{r fig.align='center'}
# annual lobster sub data set

annual_lobsters <- lobster_abundance_sbc %>% 
  group_by(site_name_long, year, MPA) %>% 
  summarise(yearly_lobsters=sum(count)) 

# graph of annual lobster counts from 2012 - 2018

ggplot(annual_lobsters, aes(x=year, y=yearly_lobsters)) +
  geom_line(aes(color=site_name_long), show.legend = FALSE) +
  geom_point(aes(color = site_name_long), show.legend = FALSE) + 
  theme_minimal() +
  labs(x = "\nYear", y = "Annual Number of Observed Lobsters\n", 
       title="Annual Lobster Abundance from 2012-2018\n") +
  coord_cartesian(clip="off", 
                  xlim=c(2012,2018)) +
  theme(plot.margin=unit(c(1,10,1,1), "lines"),
        plot.title = element_text(hjust=0.5),
        legend.position = "right",
        legend.title=element_blank()) +
  scale_fill_discrete(name= "MPA Status") +
  scale_color_manual(values = c("black", "black", "maroon", "black", "maroon")) +
  scale_x_continuous(lim=c(2012, 2018), 
                     expand=c(0,0)) +
  scale_y_continuous(lim=c(0, 1000), 
                     expand=c(0,0),
                     breaks=seq(0,1000, by=250)) +
  annotate("text", label= "Isla Vista (MPA)", 
           x = Inf, 
           y = 940, 
           size = 3,
           hjust = -0.1,
           vjust = 0,
           color = "maroon") +
  annotate("text", label = "Naples (MPA)",
           x = Inf,
           y = 270,
           size = 3,
           hjust = -0.2,
           vjust = 0,
           color = "maroon") +
  annotate("text", label = "Arroyo Quemado",
           x = Inf,
           y = 33,
           size = 3,
           hjust = -0.1,
           vjust = 0) +
  annotate("text", label = "Mohawk",
           x = Inf,
           y = 145,
           size = 3,
           hjust = -0.2,
           vjust = 0) +
  annotate("text", label = "Carpinteria",
           x = Inf,
           y = 330,
           size = 3,
           hjust = -0.18,
           vjust = 0)

```

***Figure 3.** Annual lobster counts in MPAs (red) and non-MPAs (black) from 2012 to 2018. Both MPAs and non-MPAs lobster observations increased between 2012 and 2018, however MPAs had a much greater increase in lobster observations ( mean 3,700% increase) than non-MPAs (mean 182% increase). Data: SBC LTER.*
  
  
  #### Result B: Lobster size distribution shifts, 2012 vs 2018
  
  The lobster size distribution in the five study sites in 2012 and 2018 is shown in Figure 4. Lobster size distribution has stayed relatively constant for two non-MPAs in 2012 and 2018: Carpinteria (n~2012~= 78, n~2018~=343) and Mohawk (n~2012~= 83, n~2018~=164). The lobster sizes followed a normal distribution at both sites, however only observed lobster sizes at Carpinteria exerienced no change in means (2012: 74.4mm $\pm$ 1.65mm, 2018: 74.5mm $\pm$ 0.6mm) (mean $\pm$ SE); Mohawk experienced a decline in means (2012: 77.3mm $\pm$ 1.2mm, 2018: 72.4mm $\pm$ 0.7mm). The other MPA, Arroyo Quemado (n~2012~=38, mean~2012~=71mm, SE~2012~=1.7mm), had a slightly right-skewed distribition in 2012. However, in 2018 lobster sizes in Arroyo Quemado followed a normal distribtion like the other non-MPAs, but experienced no change in mean lobster size. 

In 2012, lobster sizes at both MPAs, Isla Vista (n~2012~=26) and Naples (n~2012~=6), were right skewed. Isla Vista lobsters had an average size of 66.1mm $\pm$ 2.4mm and lobsters at Naples had an average size of 73mm $\pm$ 4.8mm. However, in 2018 the distribution of lobster sizes (Isla Vista n~2018~=946, Naples n~2018~=298) followed a normal distribition that is similar to that of the other non-MPA sites in 2018 and both experienced an increase in mean lobster sizes (Isla Vista: 76.6mm $\pm$ 0.4mm, Naples: 80.5mm $\pm$ 0.5mm)

In 2018, all sites followed a normal distribition. Naples had the largest mean lobster size in 2018 and Isla Vista had the second largest mean lobster size. Naples, Isla Vista, and Arroyo Quemado had the three smallest smaple sizes in 2012, which could account for the skewed distribution. Likely, the larger number of observations 2018 displayed the true population distribution for all the sites.


```{r fig.align='center'}
# Uncount lobster observations
lobster_abundance_tidy <- lobster_abundance_sbc %>%
  tidyr::uncount(count)

# sub-data: keep only years 2012 and 2018
lobster_size <- lobster_abundance_tidy %>%
  filter(year %in% c("2012", "2018"))%>% 
  mutate(year=as.character(year)) ### this code added becuase ggplot wouldnt recognize our year column correctly (would display color based on year aesthetic). Changing the values to characters helped over come this error

# determine sample size for each class/year
num_observations <- lobster_size %>% 
  group_by(site) %>% 
  count(year)

# determine mean, SD, SE for each site in 2012 and 2018
site_stats <- lobster_size %>% 
  group_by(year, site) %>% 
  summarize(means=mean(size_mm),
            n= n(),
            sd=sd(size_mm),
            se=sd/sqrt(n)) 


# graph for distrmibution of lobster sizes in 2012 and 2018 
ggplot(lobster_size, 
       aes(x=size_mm)) +
  geom_density(aes(fill=year), 
               position='identity', 
               alpha=0.5) +
  theme_minimal() +
  labs(x="Size (mm)", y="Density\n", 
       title="Change in lobster size distribution between 2012 and 2018") +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_x_continuous(lim=c(30, 130),
                     expand=c(0,0),
                     breaks=seq(30,120, by=20)) +
  scale_y_continuous(lim=c(0, 0.06),
                     expand=c(0,0)) +
  facet_wrap(~site_name_long, scales="free_x") +
  scale_fill_discrete(name="Year")

```

***Figure 4:** The change in lobster size (mm) distributions at five sites on the California coast in 2012 (pink) and 2018 (blue). All sites except Mohawk and Carpinteria experienced a mean increase in size of observed lobsters between 2012 and 2018. Data: SBC LTER.*
  
  
  #### Result C: Is there a differnce between mean lobster sizes at MPA vs non-MPA sites in 2012 and 2018?
  
  ```{r, include=FALSE}

# Q-Q plots to explore data and check for normal distribution/linear regressions

# QQ plot for sample size for 2012 lobsters - distribution is normal.
lobster_size_2012 <- lobster_size %>%
  filter(year == 2012)

ggplot(lobster_size_2012, aes(sample = size_mm)) +
  geom_qq()

# QQ plot for sample size for 2018 lobsters - distribution is normal.
lobster_size_2018 <- lobster_size %>%
  filter(year == 2018)

ggplot(lobster_size_2018, aes(sample = size_mm)) +
  geom_qq()

# QQ plot for MPA only site for distribution of sample lobster size - distribution is normal. 
lobster_size_MPA <- lobster_size %>%
  filter(MPA == "MPA")

ggplot(lobster_size_MPA, aes(sample = size_mm)) +
  geom_qq()

# QQ plot for Non-MPA only site for distribution of sample lobster size - distribution is normal. 
lobster_size_Non_MPA <- lobster_size %>%
  filter(MPA == "Non MPA")

ggplot(lobster_size_Non_MPA, aes(sample = size_mm)) +
  geom_qq()

# Question 1:For 2012 observations, is there a significant difference in lobster size between MPA and non-MPA sites? 

# H null = there is no difference in MPA vs non-MPA lobster size in 2012
# H alternative = there is a difference in MPA vs non-MPA lobster size in 2012
# significance value: p = 0.05
# stat test= 2 sample t test

# sub-data: only include observations from 2012
lobster_size_2012 <- lobster_size %>% 
  filter(year==2012) 

#  welch's 2 sample t test

size_ttest_2012 <- t.test(size_mm ~ MPA, data=lobster_size_2012)

size_ttest_2012

# effect size

Q1_effectsize <- cohen.d(lobster_size_2012$size_mm, lobster_size_2012$MPA)


# Question 2: For 2018 observations, is there a significant difference in lobster size between MPA and non-MPA sites? 

# H null = there is no difference in MPA vs non-MPA lobster size in 2018
# H alternative = there is a difference in MPA vs non-MPA lobster size in 2012
# significance value: p = 0.05
# stat test= 2 sample t test

# sub-data: only include observations from 2018
lobster_size_2018 <- lobster_size %>% 
  filter(year==2018) 

#  welch's 2 sample t test

size_ttest_2018 <- t.test(size_mm ~ MPA, data=lobster_size_2018)

size_ttest_2018

# effect size (p< 0.05)

Q2_effectsize <- cohen.d(lobster_size_2018$size_mm, lobster_size_2018$MPA) 


# Question 3:	For MPA sites only, is there a significant difference in lobster size means observed in 2012 vs. 2018?

# H null = there is no difference in lobster observations in MPAs in 2012 vs MPAs in 2018
# H alternative = there is a difference in MPA 2012 vs MPA 2018 lobster size
# significance value: p = 0.05
# stat test= 2 sample t test

# sub-data: only include MPA observations from 2012 and 2018
lobster_size_MPA <- lobster_size %>% 
  filter(MPA == "MPA") 

#  welch's 2 sample t test

MPA_size_ttest <- t.test(size_mm ~ year, data=lobster_size_MPA)

MPA_size_ttest

#effect size (p < 0.05)
Q3_effectsize <- cohen.d(lobster_size_MPA$size_mm, lobster_size_MPA$year) 

Q3_effectsize

# Question 4: For non-MPA sites only, is there a significant mean size difference in lobsters observed in 2012 vs. 2018?

# H null = there is no difference in MPA 2012 vs MPA 2018 lobster size
# H alternative = there is a difference in MPA 2012 vs MPA 2018 lobster size
# significance value: p = 0.05
# stat test= 2 sample t test

# sub-data: only include Non MPA observations from 2012 and 2018
lobster_size_Non_MPA <- lobster_size %>% 
  filter(MPA == "Non MPA") 

#  welch's 2 sample t test
Non_MPA_size_ttest <- t.test(size_mm ~ year, data=lobster_size_Non_MPA)

Non_MPA_size_ttest

# effect size

Q4_effectsize <- cohen.d(lobster_size_Non_MPA$size_mm, lobster_size_Non_MPA$year)

Q4_effectsize

```


***Table 1.** Results from two sample t-tests comparing mean lobster sizes at MPA and non-MPA sites in 2012 and 2018. Data: SBC LTER.*
  ```{r fig.align='center'}
# data from 1: mean and standard deviation for MPA and non MPAs in 2012 and 2018
lobster_size_stats <- lobster_size %>% 
  group_by(year, MPA) %>% 
  summarize(Mean=mean(size_mm), 
            Standard_Deviation=sd(size_mm))


# data frame 2: sample size for MPA and non MPAs in 2012 and 2018
num_observations <- annual_lobsters %>%
  group_by(MPA, year) %>% 
  summarize(sample_size=sum(yearly_lobsters)) %>% 
  ungroup(year) %>% 
  mutate(year=as.character(year)) %>% 
  dplyr::filter(year %in% c("2018", "2012"))  # fixed it!!

# merge data frames 
lobster_size_table_merged <- inner_join(lobster_size_stats, num_observations, by=c("MPA", "year"))

# create table with mean, standard deviation, and sample size for MPA and non MPAs in 2012 and 2018
lobster_table <- lobster_size_table_merged %>%
  kable(col.names = c("Year", 
                      "MPA status",
                      "Mean lobster size (mm)",
                      "Standard deviation",
                      "Sample population"),
        digits=2) %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F, 
                position="center") %>%
  add_header_above(c("Mean lobster sizes at MPA vs Non-MPA sites in 2012 and 2018" = 5)) 

lobster_table


```



2012 observations for mean lobster sizes between MPA (`r round(size_ttest_2012$estimate[1],2)`mm) and non-MPA sites (`r round(size_ttest_2012$estimate[2],2)`mm) differed significantly (t(`r round(size_ttest_2012$parameter,2)`) = `r round(size_ttest_2012$statistic,2)`, *p* = `r round(size_ttest_2012$p.value,2)`). The medium effect size (`r round(Q1_effectsize$estimate,1)`) indicates it is likely the there was a significant difference between mean lobster sizes at MPA and non-MPA sites in 2012, despite the large difference in smaple sizes. 

For observations in 2018, mean lobster sizes between MPA (`r round(size_ttest_2018$estimate[1],2)`mm) and non-MPA sites (`r round(size_ttest_2018$estimate[2],2)`mm) differed significantly (t(`r round(size_ttest_2018$parameter,2)`) = `r round(size_ttest_2018$statistic,2)`, *p* = `r round(size_ttest_2018$p.value,2)`). To measure the magnitude of the difference between MPA and non-MPA sites in 2018, we calcualted the effect size. A moderate effect size (`r round(Q2_effectsize$estimate,1)`) indicates that though the difference in means was more difficult to observe, there was indeed a difference between population means. 

In MPA sites, the difference in mean lobster size observations in 2012 (`r round(MPA_size_ttest$estimate[1],2)`mm) and 2018 (`r round(MPA_size_ttest$estimate[2],2)`mm) differed significantly (t(`r round(MPA_size_ttest$parameter,2)`) = `r round(MPA_size_ttest$statistic,2)`, *p* = `r round(MPA_size_ttest$p.value,2)`). The large effect size (`r round(Q3_effectsize$estimate,1)`) calculated for MPA sites indicates that there was a substantial difference in lobster means in 2012 and 2018.

The difference in mean lobster sizes in non-MPA sites in 2012 (`r round(Non_MPA_size_ttest$estimate[1],2)`mm) and 2018 (`r round(Non_MPA_size_ttest$estimate[2],2)`mm) did not differ (t(`r round(Non_MPA_size_ttest$parameter,2)`) = `r round(Non_MPA_size_ttest$statistic,2)`, *p* = `r round(Non_MPA_size_ttest$p.value,2)`). The actual difference between means was 1.3 mm. A small effect size (`r round(Q4_effectsize$estimate,1)`) confirms that there is not enough evidence to determine that these two populations have different means.    

### Summary

It is worth noting that all size observations recorded by divers were visually estimated. So, these types of measurements are likely more erroneous and should be subject to more scrutiny during data analysis than measurements conducted with a device. 

- Lobster abundance increased dramatically in newly designated Marine Protected Areas and increased slightly in non-Marine Protected Areas between 2012 and 2018 (Figure 3). 
- Mean lobster sizes in Marine Protected Areas were larger in 2018 compared to 2012, and larger than mean lobster sizes in non-Marine Protected Areas (Figure 4). 
- Population means varied significantly between Marine Protected Areas and non-Marine Protected Areas (Table 1). 
- Population means between 2012 and 2018 in non-Marine Protected Areas did not vary significantly, but did vary significantly in Marine Protected Areas (Table 1).


### Citations 

**Bierman, Ed (2016).** *California spiny lobster.* Wikipedia. https://en.wikipedia.org/wiki/California_spiny_lobster.

**Reed D. (2019).** *SBC LTER: Reef: Abundance, size and fishing effort for California Spiny Lobster (Panulirus interruptus), ongoing since 2012.* Environmental Data Initiative. https://doi.org/10.6073/pasta/a593a675d644fdefb736750b291579a0. Date accessed 11/13/2019.

**SBC LTER.** *Santa Barbara Coastal LTER: About SBC LTER.* https://sbclter.msi.ucsb.edu/. Date accessed 11/15/2019.

**WildAid (2018).** *Marine Protected Areas 101.* https://wildaid.org/marine-protected-areas-101/. Date accessed 11/15/2019. 


################################### ESM 206: HW 5 ###########################################

library(tidyverse)
library(janitor)
library(dplyr)
library(kableExtra)
library(ggbeeswarm)
library(car)
library(effsize)

# add data -------------------------------------------------------------------------

mack_creek_verts <- read_csv("mack_creek_vertebrates.csv") %>% 
  clean_names()


#### Results A: Annual salamander counts in old growth and clear cut sections of Mack Creek

Salamander counts in old growth and clear cut sections were compared between 1993 and 2017. Overall, salamander counts in both sections generally followed similar patterns; when counts in old growth sections increased, counts in clear cut sections also increased. Similarly, when counts of salamanders in old growth sections decreased, salamander counts in clear cut sections also decreased. Likely, environmental factors affect annual variability in salamander counts. 

Generally, salamander observations were higher in old growth forest. However, in 1994 and 2015-2017, salamander counts were higher in clear cut sections. Between 1993 and 2017, salamander counts in the clear cut and old growth forests increased by 129% and 154%, respectively. Throughout the study period, salamander counts in the clear cut section reached a high of 368 in 2017 and a low of 137 in 2014. Comparatively, salamander counts in the old growth section reached a high of 380 in 2002 and a low of 129 in 1993. In general, salamader counts have been increasing since the study began in 1993. 


```{r}
# subdata: remove unneccessary columns and keep only salamander observations
salamander_counts_data <- mack_creek_verts %>% 
  filter (species == "DITE") %>% 
  select (year, section, species)

# count number of salamanders in each section (CC vs OG) for every year
salamander_counts <- salamander_counts_data %>% 
  group_by(year, section) %>% 
  count(species) %>%
  mutate(section_long = case_when(
    section %in% "CC" ~ "Clear Cut",
    section %in% "OG" ~ "Old Growth"))

# totalling counts of salamanders in CC and OG sections for result descriptions
salamander_counts_totals <- salamander_counts %>%
  group_by(section) %>%
  summarize(total_pop = sum(n))

# Graph of salamander counts
ggplot(salamander_counts, aes(x = year, y = n)) +
  geom_line(aes(color = section_long), size = 1) +
  geom_point(aes(color = section_long), size = 2) +
  scale_x_continuous(lim = c(1993, 2017),
                     expand = c(0, 0),
                     breaks = seq(1993, 2017, by = 2)) +
  scale_y_continuous(lim=c(0, 400),
                     expand=c(0,0),
                     breaks=seq(100, 400, by=100)) +
  scale_color_manual(name="Creek Section", values = c("orange", "seagreen")) +
  theme_bw() +
  labs(x = "\nYear",
       y = "Annual Salamander Counts\n",
       title = "Salamander Count in Clear Cut and Old Growth Sections of Mack Creek\n")+
  theme(plot.title = element_text(hjust=0.5))


# I can't figure out how to change the title of the legend!?!? ah ha! got it! I was trying to figure this same thing out earlier and couldnt get it

```

***Figure 3:** Annual salamander counts in clear cut (orange) and old growth (green) sections of Mack Creek from 1993-2017. Old growth sections are responsible for higher counts of salamanders for 80% of the study period. The populations in both sections display similar patterns of highs and lows. Data: Stanley Gregory, Andrews Forest LTER Sites.* 
  
  #### Results B: Table of 2017 salamander counts by channel classification 
  
  ***Table 1:** Salamander counts in different channel types (cascade, pool, and side channel) in clear cut and old growth forests in Mack Creek (1993-2017). Data: Stanley Gregory, Andrews Forest LTER Sites.*
  ```{r}
# Table of 2017 salamander counts by channel classification (pool, cascades and side-channel) in old growth and clear cut sections of Mack Creek.

#subdata: only include 2017 and salamader observations, exclude isolated pools from data
salamander_channel_2017 <- mack_creek_verts %>% 
  filter (year == "2017") %>% 
  filter (species == "DITE") %>% 
  filter (unittype != "IP") %>% 
  mutate(channel_type= case_when(
    unittype %in% "C" ~ "Cascade",
    unittype %in% "P" ~"Pool",
    unittype %in% "SC" ~ "Side Channel"
  )) %>% 
  select (species, channel_type, section)

# counts
salamander_channel_counts_2017 <- salamander_channel_2017 %>% 
  group_by(channel_type, section) %>% 
  count(species) %>% 
  select (channel_type, section, n)

# create contigency table 
salamander_table_2017 <- salamander_channel_counts_2017 %>% 
  pivot_wider(names_from = section, values_from = n) 


# add proportions to contigency table
salamander_table_counts_proportions_2017 <- salamander_table_2017 %>% 
  adorn_percentages(denominator = "row") %>% 
  adorn_pct_formatting(digits=1) %>% 
  adorn_ns(position = "front")

#  markdown ready table
salamander_table_counts_proportions_2017 %>% 
  kable (col.names = c("Channel Type", "Clear Cut Forest", "Old Growth Forest")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


```

2017 salamander counts in clear cut sections were higher in cascades (n~cascade~ = 247 (55.1%)) and side channels (n~side~ ~channel~ = 90 (54.9%)). In old growth sections, pools had higher counts of salamanders (n~pool~ = 45 (59.2%)). 

#### Results C: Is there a significant difference in salamander counts in channel locations in old growth and clear cut forests?

```{r, include = FALSE}
# Chi-Square test to determine if there is a significant difference in what channel Pacific giant salamanders are located (pool, cascade or side channel) between the two old growth and clear cut forests

# Null Hypothesis: There is no siginificant difference in what channel Pacific giant salamander are located between old growth and clear cut forests

# Alternative Hypothesis: There is a significant difference in what channel Pacific giant salamander are located between old growth and clear cut forests


# first, get a contingency table of ONLY counts
salamander_ct <- salamander_table_2017 %>%
  column_to_rownames('channel_type')

# chi-square test
salamander_chi <- chisq.test(salamander_ct)

salamander_chi 

# p-value of 0.06262, retain the null hypothesis


```

We performed a Pearson's chi-squared test to examine whether or not there was a significant difference in where in the channel Pacific giant salamanders are located between the two sections of forest (old growth and clear cut). The results from our test found that there was no siginificant difference in what channel Pacific giant salamander are located in forest section ($\chi$^2^(`r salamander_chi$parameter`) = `r round(salamander_chi$statistic,2)`, *p* = `r round(salamander_chi$p.value,2)`). 



#### Results D: Is there a significant difference in mean weights for salamanders observed in clear cut vs  old growth forests in 2017?

```{r, include = FALSE}
# Is there a significant difference in mean weights for Pacific giant salamanders observed in clear cut vs  old growth forests in 2017?

# sub data: only include salamander observations in 2017 and remove unneccessary columns
salamader_weights <- mack_creek_verts %>% 
filter (species == "DITE") %>% 
filter (year == "2017") %>% 
select (section, species, weight)

# look at data spread ------------------------------------------------------------

#histogram --> observations are very skewed towards smaller weights
ggplot (salamader_weights, aes (x=weight)) +
geom_histogram() +
facet_wrap(~section)

# qqplot
ggplot (salamader_weights, aes (sample=weight)) +
geom_qq() +
facet_wrap(~section)

#conclusion: most of the points in both qq plots follow a straight line, even though there is some deviation in the qqplot and the histograms are skewed, we can assume the means follow a normal sampling distribution because of the central limit theorem so we can use a parametrric test to compare means -- Can you explain how those are normally distributed? They don't look like it at all... (HGW)

# statistical significance test ----------------------------------------------------

weigths_ttest <- t.test(weight~section, data=salamader_weights)

weigths_ttest

# effect size ---------------------------------------------------------------

weights_effectsize <- cohen.d(weight~section, data=salamader_weights)

weights_effectsize
```

A two-sample t-test was used to determine if mean salamander weights in 2017 were significantly different between old growth and clear cut forests. 2017 mean salamander weights in clear cut forests (`r round(weigths_ttest$estimate[1],2)` g) did not differ significantly from 2017 salamander weights in old growth forests (`r round(weigths_ttest$estimate[2],2)` g) (t(`r round(weigths_ttest$parameter,2)`) = `r round(weigths_ttest$statistic,2)`, *p* = `r round(weigths_ttest$p.value,2)`). A negligable effect size (`r round(weights_effectsize$estimate,1)`) confirms that there is no difference in mean 2017 weights by section along Mack Creek.


#### Results E: Is there a significant difference between mean salamander weights in the 3 channel types of Mack Creek in 2017?


```{r, include = FALSE}
# Is there a significant difference between mean Pacific giant salamander weights in the 3 channel types: pools, cascades and side-channels of Mack Creek in 2017?

# Visually compare the salamander weights between the three channel classifications: SC (side channel), P (pool), C (cascade). Use beeswarm or jitter. 

# filter species, years, and channels for just Cascade, Pool, and Side Channel
channel_weights <- mack_creek_verts %>%
  filter(species == "DITE") %>%
  filter(year == 2017) %>%
  filter(unittype %in% c("C", "P", "SC")) %>%
  mutate(channel_type = case_when(
    unittype %in% "C" ~ "Cascade",
    unittype %in% "P" ~"Pool",
    unittype %in% "SC" ~ "Side Channel"))

# Calculate the mean, standard deviation, standard error, and variance
channel_weights_mean <- channel_weights %>%
  select(unittype, weight, channel_type) %>%
  group_by(channel_type) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE),
            sd_weight = sd(weight, na.rm = TRUE),
            sample_size = n(),
            se_weight = sd((weight) / sqrt(n()), na.rm = TRUE),
            var_weight = var(weight, na.rm = TRUE))

# Levene's test to assess equal variance 

leveneTest(weight~channel_type, data=channel_weights) 
#p=0.09, accept null that the variances are not different

```
Salamanders found in 2017 in side channels of Mack Creek had the lowest mean weight (mean~2017~ = `r round(channel_weights_mean$mean_weight[3], 2)` g) compared to salamanders found in pools (mean~2017~ = `r round(channel_weights_mean$mean_weight[2], 2)` g) and cascades (mean~2017~ = `r round(channel_weights_mean$mean_weight[1], 2)` g) (Figure 4.). 

```{r}
# Graph the salamander weights for all channels for 2017
ggplot() +
  ggbeeswarm::geom_beeswarm(data = channel_weights,
                            aes(x = channel_type,
                                y = weight,
                                color = unittype),
                            na.rm = TRUE,
                            size = 2,
                            alpha = 0.6,
                            pch = 16,
                            show.legend = FALSE) +
  geom_errorbar(data = channel_weights_mean,
                aes(x = channel_type,
                    ymin = mean_weight - sd_weight,  
                    ymax = mean_weight + sd_weight),
                color = "black",
                width = 0.1) +
  geom_point(data = channel_weights_mean,
             aes(x = channel_type, y = mean_weight),
             color = "black",
             size = 4) +
  labs(x = "\nChannel Type",
       y = "Weight (grams)\n") +
  ggtitle("2017 Salamander Weight in Different Channels\n") +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_y_continuous(expand=c(0,0),
                     lim=c(-5, 100))     
```

***Figure 4:** Distribution of 2017 weights of salamanders in cascade, pool, and side channels of Mack Creek. Mean weights for each channel type are indicated by the black center dot. Standard deviation of weights are indicated by the upper and lower bars. Data: Stanley Gregory, Andrews Forest LTER Sites.*
  
  ```{r, include = FALSE}
# Is there a significant difference in mean weight for pacific giant salamanders observed in pools vs cascades vs side channels in 2017? 

## Null Hypothesis: the mean weight for different channel types are the same/not significantly different.
## Alternative Hypothesis: the mean weight for differnt channel types are not the same/are significantly different. 

salamander_aov <- aov(weight ~ channel_type, data = channel_weights)
summary(salamander_aov)

# Result: Reject the Null. 
# F(`r salamander_aov[1]`, `r salamander_aov[2]`) = `r round(salamander_aov[7],2)`, *p* = `r round(salamander_aov[9],3)`) <-- In-line referencing that I cannot get to work.. UGH

# Post-hoc testing to determine whethere there is a difference in means between all three channels or just two - Using Tukey's HSD

salamander_tukey <- TukeyHSD(salamander_aov)
salamander_tukey

# Results of Tukey's HSD :
# No difference in mean between Pool and Cascade
# No significant difference between Side Channel and Cascade  
# Significant Difference between Side Channel and Pool

# To report your one-way ANOVA, need to unlist the outcomes

salamander_outputs <-unlist(summary(salamander_aov))
salamander_outputs

# Create a data frame of Tukey to reference p-values through in-line referencing

df <- data.frame(salamander_tukey$channel_type)

```
To determine whether or not there is a difference in 2017 mean weight for salamanders found in different channel types (pool, cascade, and side channel), we performed a one-way ANOVA test. The results  F(`r salamander_outputs[1]`, `r salamander_outputs[2]`) = `r round(salamander_outputs[7],2)`, *p* = `r round(salamander_outputs[9],3)`) indicate that there is a significant difference between at least two of the mean weights for salamanders in different channels. A Tukey's HSD post-hoc test was performed to determine which channels had significant differences between 2017 mean weights. The results show that there is a significant difference in only one pairwise comparison: mean 2017 weights between salamanders in side channels (`r round(channel_weights_mean$mean_weight[3], 2)`g $\pm$ `r round(channel_weights_mean$sd_weight[3], 2)`g)(mean $\pm$ SD) and pools (`r round(channel_weights_mean$mean_weight[2], 2)`g $\pm$ `r round(channel_weights_mean$sd_weight[2], 2)`g)(mean $\pm$ SD) (*p* = `r round(df$p.adj[3],3)`). There was no significant difference in mean weights of salamanders between cascades (`r round(channel_weights_mean$mean_weight[1], 2)`g$\pm$ `r round(channel_weights_mean$sd_weight[1], 2)`g)(mean $\pm$ SD) and pools (*p* = `r round(df$p.adj[1],3)`), as well as no significant difference between side channel and cascades (*p* = `r round(df$p.adj[2],3)`).


The actual differences in mean weights between channel types was 1.78 grams between pools and cascades and 1.84 grams between side channels and cascades. These differences were significantly lower than the difference between pools and side channles, which was 3.62 grams (about 50% higher than the other mean differences). 

```{r}
# Describe any concerns you have about comparing means across the groups 
```

Our above tests are parametric tests that include assumptions of random samples, normal distribution of the salamander population's weights, and homogeneity of variances.  A levene's test for homogeneity of variances revealed that the variances are equal between groups, however an examination of the sampling distribution through histograms revealed that the salamander population's weights are skewed to the right. In cases of skewed samples, an unpaired non-parametric test may also be appropriate to determine a signficant difference in salamander weights by channel type. Non-parametric tests can be used for non-normal distributions, as we saw in the salamander weights above. In that case, we would perform a Mann-Whitney U test to rank the weights of the salamanders instead of comparing the means. 


### Summary

- Salamander counts have generally increased in both sections of Mack Creek (clear cut and old growth forests) since surveys of salamander populations began in 1993 (Figure 3). It is evident that salamanders can adapt to the clear-cutting of forests.
- There is no significant difference in 2017 salamander counts and weights in clear cut forests compared to old growth forests (Results C & D). 
- Mean salamander weights in 2017 were only significantly different in side channels and pools (Figure 4); the mean difference in salamander weights was about half as much as the difference in means between the other two pairwise comparisons. 


### References 

**HJ Andrews Experimental Forest Long Term-Ecological Research.** https://andrewsforest.oregonstate.edu/sites/default/files/lter/data/map/2011base.jpg

**Gregory S. V. 2016**. Aquatic Vertebrate Population Study in Mack Creek, Andrews Experimental Forest, 1987 to present. Environmental Data Initiative. https://doi.org/10.6073/pasta/5de64af9c11579266ef20da2ff32f702. Dataset accessed 11/25/2019.

**Nafis, Gary.** "Great Pacific Salamander." CaliforniaHerps. http://www.californiaherps.com/salamanders/pages/d.tenebrosus.html





############################################# ESM 206: Final ##################################################


library(tidyverse)
library(dplyr)
library(janitor)
library(kableExtra)
library(grid)
library(stargazer)
library (car)

```

<br>
  
  ### Task 1: Predicting Penguin Mass
  
  <br>
  ```{r}
# load data and remove NA's
penguin <- read_csv("lter_penguin_species.csv") %>% 
  drop_na(body_mass_g) %>% 
  drop_na(flipper_length_mm) %>% 
  drop_na(sex) %>% 
  filter (sex != ".")

```

```{r}

# visualize data
penguin_sub <- penguin %>% 
  select(body_mass_g, species, flipper_length_mm, sex)

ggplot(penguin_sub, aes(x=flipper_length_mm, y=body_mass_g)) +
  geom_point(aes(color=fct_rev(species))) +
  facet_wrap(~sex) +
  labs(x = "\nFlipper Length (mm)", 
       y = "Body Mass (g)\n", 
       title = "Weight and Flipper Length\n of Three Antarctic Penguin Species") +
  theme_bw() +
  scale_color_discrete(name="Species", 
                       labels=c("Gentoo Penguin (Pygoscelis papua)",
                                'Chinstrap Penguin (Pygoscelis antarctica)',
                                "Adelie Penguin (Pygoscelis adeliae)")) +
  theme(legend.title.align=0.5, 
        plot.title = element_text(hjust=0.5),
        panel.spacing=unit(2, "lines")) +
  scale_x_continuous(lim=c(150, 250), 
                     expand=c(0,0),
                     breaks=seq(150, 250, by=50)) +
  scale_y_continuous(lim=c(2500, 6500), 
                     expand=c(0,0),
                     breaks=seq(3000, 6500, by=1000))

```

***Figure 1:** Flipper lengths (mm) and body mass (g) by sex of three penguin species in Antarctica - [Gentoo](https://doi.org/10.6073/pasta/409c808f8fc9899d02401bdb04580af7), [Chinstrap](https://doi.org/10.6073/pasta/2b1cff60f81640f182433d23e68541ce), and [Adelie](https://doi.org/10.6073/pasta/abc50eed9138b75f54eaada0841b9b86) penguins. Data: Palmer Station LTER.*
  
  <br>
  General trends from Figure 1:
  
  - Male penguins appear to have higher body mass and slightly longer flipper lengths than females across all three species. 
- Gentoo penguins have the highest body mass and longest flipper length than Chinstrap and Adelie penguins. 

<br>
  
  ```{r, include = F}
#Multiple linear regression model 

# model trials --------------------------------------------------

mass_model1 <- lm(body_mass_g ~ species + flipper_length_mm + sex, data=penguin)
summary(mass_model1)

mass_model2 <- lm(body_mass_g ~ flipper_length_mm + sex, data=penguin) 
summary(mass_model2)

mass_model3 <- lm(body_mass_g ~ species + flipper_length_mm, data=penguin)
summary(mass_model3)

# compare model AICs

AIC (mass_model1) #4740 --> lowest AIC
AIC (mass_model2) #4862
AIC (mass_model3) #4895

# check assumptions (residual normality and homoscedasticity)

plot(mass_model1)

# statement about how the model passes the assumptions here #########


```

***Table 1:** Regression table displaying the relationship between Antarctic penguins species, sex, and flipper length to predict body mass. body mass(sex, flipper length, species) = -365.8 + 530.38(MALE) + 20.03(flipper length) + 836.26(Gentoo Penguin) - 87.63(Chinstrap Penguin)*
  ```{r, results='asis'}
#table to report model results (using stargazer)

stargazer(mass_model1, type="html")
```

<br>
  
  The assumptions for using multiple linear regression were met by plotting the residuals of the multiple linear regression model. The QQ plot showed that the residuals follow a normal distribution so the assumption of residual normality is met, while the residuals vs. fitted plot showed that the residuals appear randomly distributed with no change in range so the assumption of homoscedasticity is also met. 

<br>
  
  The adjusted R^2^ (R^2^ = 0.865) evaluates how much variance in body mass is explained by the explanatory variables (species, flipper length, sex), so in this case 86.5% of the variance in penguin body mass can be explained by species, flipper length, and sex.

<br>
  
  Interpretation of the model coefficients:
  
- If everything else about the penguin is the same (species, flipper length), then I expect male penguins to weigh 530.38 grams more than a female penguin on average
- If everything else is held constant, then I expect boby mass to increase 20.03 grams for every 1 mm increase in flipper length.
-  If everything else about the penguin is the same (sex, flipper length), then I expect Gentoo penguins weigh 836.26 grams more than Adelie penguins. 
-  If everything else about the penguin is the same (sex, flipper length), then I expect Chinstrap penguins weigh 87.63 grams less than Adelie penguins. 

# Use the model predict the masses 

# create data frame
new_df <- data.frame(
  flipper_length_mm = rep(c(195, 205, 220)),
  sex = rep(c("FEMALE", "MALE", "MALE")),
  species = rep(c("Chinstrap penguin (Pygoscelis antarctica)", "Adelie Penguin (Pygoscelis adeliae)", "Gentoo penguin (Pygoscelis papua)"))
)

# predict values in data frame
predict_df <- predict(mass_model1, newdata = new_df)

# bind predictions with new_df
new_df_predictions <- data.frame(new_df, predict_df)

# make new_df_preiditions into a nice table

new_df_predictions %>% 
  kable (col.names = c("Flipper Length (mm)", "Sex", "Species", "Body Mass (g) Predictions"), align="c") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
    full_width=T,
    position="center") 

```


<br>
  <br>
  
  ### Task 2: Smoking effects on baby birth weight
  
  ```{r}
# load data and only keep tpounds, lowbw, and smoke columns

birthweights <- read_csv('nc_birthweights.csv') %>% 
  select (tpounds, lowbw, smoke) %>% 
  drop_na(smoke)

```

<br>
  
  ***Table 3:** Data was collected on babies birthweight (categorically classified as low or not low) and whther or not the mother smoked during pregnancy. 1,000 babies from North Carolina were randomly selected for the study.*
  ```{r}
# table: Does the proportion of babies born at “low birth weight” differ depending on whether the mother smoked or did not smoke during pregnancy

# step 1: create sub data

birthweights_table_data <- birthweights %>% 
  group_by(smoke) %>% 
  count(lowbw) %>% 
  mutate(low_birth_weight = case_when( 
    lowbw == 1 ~ "Yes",
    lowbw == 0 ~ "No")) %>% 
  mutate("Mother Smoke?" = case_when(
    smoke == 1 ~"Yes",
    smoke == 0 ~"No"
  )) %>% 
  ungroup(smoke) %>% 
  select(-lowbw, -smoke)

# step 2 create contigency  table

birthweights_table_counts <- birthweights_table_data %>% 
  pivot_wider(names_from = low_birth_weight, values_from = n) 

birthweights_table_proportions <- birthweights_table_counts %>% 
  adorn_percentages(denominator = "row") %>% 
  adorn_pct_formatting(digits=1) %>% 
  adorn_ns(position = "front")

# step 3: R markdown friendly contigency table with counts and proportions

birthweights_table_proportions %>% 
  kable (col.names = c("Mother Smoke?", "No", "Yes")) %>%
  add_header_above(c(" "= 1, "Low Birthweight?"=2)) %>% 
  #add_header_above(c("North Carolina babies: Does mother smoking affect baby's birthweight?"=3)) %>% 
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
    full_width=F,
    position="center") %>% 
  column_spec(1, width='4cm') %>%  #control column 1's row width
  column_spec(2, width="8em") %>% 
  column_spec(3, width="8em")


```

```{r, include = F}
# statistically significant difference between the proportion of babies born at low birth weights for smoking vs. non-smoking mothers? 

# H null: there is no difference in birthweights of babies from mothers who did and did not smoke
# H alt : there is a difference in birthweights of babies from mothers who did and did not smoke

# step 1: get cocntigency table of just counts
birthweight_counts <- birthweights_table_counts %>% 
  column_to_rownames('Mother Smoke?')           

# step 2: run chi-square test
birthweights_chi <- chisq.test(birthweight_counts)
birthweights_chi

# p = 0.2696
```

<br>
  
  A Pearson's chi-squared test was performed to examine whether or not babies with low birthweights are independent of having mothers who smoked during pregnancy. The results from our test found that babies with low bithweights are independent of being born from mothers who smoked during pregnancy ($\chi$^2^(`r birthweights_chi$parameter`) = `r round(birthweights_chi$statistic,2)`, *p* = `r round(birthweights_chi$p.value,2)`). There was a 3.8% difference between babies who did not have low birthrates from mothers who did smoke and mother who did not smoke. There was a 4.2% difference between babies with low birthrates from mothers who did smoke and mother who did not smoke.

To improve future surveys, researchers might consider changing the "smoking during pregnancy" category from binomial (yes or no) to an ordinal response with more than two categories (e.g. frequently, sometimes, rarely, never). Some women might have smoked once or twice throughout their pregnancy, while others might have smoked every day, or some other frequency. Mothers who smoked once or twice may not have had as much of an effect on their babies birthweight than mothers who smoked frequently, so researchers would want to distinguish between these differences if there are any.

<br>

### Task 3: Visualizing UCSB campus profiles

<br>
```{r, include = F}
ucsb <- read_csv('ucsb_campus_profiles.csv') %>% 
pivot_longer('2018 - 2019' : '2008 - 2009',
names_to = 'year',
values_to = 'value') %>% 
clean_names()
```

In this section, I will explore the undergraduate enrollment rates of minorities at University of California, Santa barbara across all years of available data (2008-2019). Enrollment rates of whites, others, and unknowns were excluded from the analysis and some ethnicities were combined into similar groups (Filipinos were combined with Asian and Pacific Islanders, Chicanos and Latinos were combined into a new group called Hispanics). 

```{r}
# for now, explore what relationship to explore
# subdata: combine groups and remove other/unknown/white

ucsb_ethnicity <- ucsb %>% 
filter (student_level == "Undergraduate (individuals)") %>% 
group_by(ethnicity_domestic_enrollment,year) %>% 
summarise(enrollment = sum(value, na.rm=T)) %>% 
ungroup(ethnicity_domestic_enrollment)

ucsb_ethnicity_combined <- ucsb_ethnicity %>% 
filter (ethnicity_domestic_enrollment != "Other") %>%
filter (ethnicity_domestic_enrollment != "Unknown") %>% 
filter (ethnicity_domestic_enrollment != "White") %>% 
mutate (ethnicity_domestic_enrollment = case_when(
ethnicity_domestic_enrollment %in% 'American Indian / Alaskan'  ~ 'American Indian/Alaskan', 
ethnicity_domestic_enrollment %in% c("Asian / Pacific Islander", "Filipino" ) ~ "Asian/Pacific Islander", 
ethnicity_domestic_enrollment %in% "Black / African American" ~ "African American",
ethnicity_domestic_enrollment %in% c("Chicano", "Latino") ~ "Hispanic",
ethnicity_domestic_enrollment %in% "E. Indian / Pakistani" ~ "E.Indian/Pakistani"
)) %>% 
group_by(year, ethnicity_domestic_enrollment) %>% 
summarise(enrollment = sum(enrollment)) 



#plot
ggplot (ucsb_ethnicity_combined, aes(x= year, y=enrollment)) +
geom_line (aes(group = ethnicity_domestic_enrollment, 
color=ethnicity_domestic_enrollment), 
show.legend=F) +
geom_point(aes(color=ethnicity_domestic_enrollment), show.legend=F) +
theme_bw() +
labs(x= "\nYear", 
y= "Enrollment\n", 
title=" Undergraduate Enrollment of Minorities at UC Santa Barbara", 
subtitle="2008-2019\n") +
coord_cartesian(clip="off") +
theme(plot.title = element_text(hjust=0.5),
plot.subtitle = element_text(hjust=0.5),
plot.margin = unit(c(1,7.3,1,1),"lines")) +
scale_x_discrete (expand=c(0,0),
labels = c('08-09', '09-10', '10-11', 
'11-12', '12-13', '13-14', 
'14-15', '15-16', '16-17', 
'17-18', '18-19')) +
scale_y_continuous(lim=c(0, 7000), 
expand=c(0,0),
breaks=seq(0,7000, by=1000)) +
annotate("text", label= "American Indian/Alaskan", 
x = Inf, 
y = 150, 
size = 3,
hjust = -0.12,
vjust = 0) +
annotate("text", label = "Asian/Pacific Islander",
x = Inf,
y = 5000,
size = 3,
hjust = -0.1,
vjust = 0) +
annotate("text", label = "African American",
x = Inf,
y = 1010,
size = 3,
hjust = -0.175,
vjust = 0) +
annotate("text", label = "Hispanic",
x = Inf,
y = 6020,
size = 3,
hjust = -0.25,
vjust = 0) +
annotate("text", label = "E.Indian/Pakistani",
x = Inf,
y = 650,
size = 3,
hjust = -0.18,
vjust = 0) 

```

***Figure 2:** Annual undergraduate enrollment rates of minorities and underrepresented groups at University of California, Santa Barbara. Years are based on a school calendar year which starts in September of one year and ends in June of the following year. Data: [UCSB](http://bap.ucsb.edu/institutional.research/campus.profiles/)*

<br>

General takeways from Figure 2:

- Hispanic and Asian/Pacific Islanders are the two most abundant minorities at UCSB, which have increased by 54% and 70%,respectively, since 2008.
- African American, E.Indian/Pakistani, and American Indian/Alaskan are the least represented minorities ar UCSB. Their combined enrollment is a fraction of Hispanic or Asian/Pacific Islander students. However, two groups, African Americans and  E.Indian/Pakistani, have had a raster growth rate in enrollment (75% and 226% respectively) than Hispanic and Asian/Pacific Islanders.

<br>

### Task 4: Purple urchins on the Santa Barbara coast

```{r, include = F}
# load data and modify so only have purple urchin observations in control treatments 

urchin <- read_csv('sbc_lter_urchins.csv') 
urchin <-  urchin %>% 
clean_names() %>% 
filter (common_name == "Purple Urchin") %>% 
filter (treatment == "CONTROL") %>% 
uncount(count) %>% 
select (year, site, size) %>% 
mutate(site = case_when(
site %in% c("CARP") ~ "Carpinteria" ,
site %in% c("IVEE") ~ "Isla Vista",
site %in% c("AQUE") ~ "Arroyo Quemado",
site %in% c("MOHK") ~ "Mohawk", 
site %in% c("NAPL") ~ "Naples")) %>% 
mutate(year=as.character(year))

```

<br> 

***Table 4** Summary statistics for purple urchin (*Strongylocentrotus purpuratus*) population sizes in control plots at five sites along the Santa Barbara coast from 2008-2018. Data: [SBC LTER](https://doi.org/10.6073/pasta/846569acc8745c133a44eac40245e6f0). *

```{r}

# summary statistics for urchin sizes at the 5 different sites

urchin_stats <- urchin %>% 
group_by(site) %>% 
summarize (mean = mean(size, na.rm=T),
sd = sd(size, na.rm=T),
se = sd((size) / sqrt(n()), na.rm=T),
var =var(size),
sample_size = n())
# kable of urchin stats

urchin_stats %>% 
kable (col.names = c("Site", "Mean", "Standard Deviation", "Standard Error", "Variance", "Sample Size"),
digits=2) %>%
kable_styling(
bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
full_width=T,
position="center") 


```
<br>
<br>
```{r}
# exploratory graphs of purple urchin size distributions at each site

ggplot (urchin, aes(x=size)) +
geom_density(aes(fill=site), show.legend=F) +
labs ( x= '\nSize (cm)', 
y = 'Density\n', 
title="Sampling distribution of purple urchins",
subtitle = "2008-2018\n") +
scale_x_continuous(expand = c(0,0),
lim = c(0,10),
breaks = seq(0,10, by=2)) +
scale_y_continuous(expand = c(0,0)) +
theme_bw() +
facet_wrap(~site, scales="free_x") +
theme(plot.title = element_text(hjust=0.5),
panel.spacing=unit(2, "lines"),
plot.subtitle = element_text(hjust=0.5))



```

***Figure 3:** Sampling distributions of purple urchin (*Strongylocentrotus purpuratus*) population sizes in control plots at five sites along the Santa Barbara coast. Sample size was combined with all samples collected from 2008 - 2018. Data: [SBC LTER](https://doi.org/10.6073/pasta/846569acc8745c133a44eac40245e6f0). *

<br>

Observed trends in sampe distribution graphs:

- Urchin sizes at Arroyo Quemado, Isla Vista, and Mohawk appear normally distributed.
- Urchin sizes at carpenteria and Naples are slightly right skewed. 



```{r, include = F}
# is there a significant difference in mean purple urchin size in control treatments at the 5 sites

# H null: there is no significant difference in mean purple urchin size in control treatments at the 5 sites
# H alt: there is a significant difference in mean purple urchin size in control treatments at the 5 sites

# explore assumptions ------------------------------------------------------

#levene's test for homogeneity
leveneTest(size ~ site, data=urchin) # p <2.2e-16, so variances are not equal, but the largest variance is less than 4x greater than the smallest variance so can still use parametric anova

# looked at histograms, can also look at qqplots to check nomrality

ggplot(data = urchin, aes(sample = size)) +
  geom_qq(aes(color = site),
          alpha = 0.5,
          show.legend = FALSE
  ) +
  facet_wrap(~site, scales = "free")
# they appear normal enough (same with the histograms) to use an anova

# run tests -------------------------------------------------------------

# anova
urchin_aov <- aov(size ~ site, data=urchin)
summary(urchin_aov)
# p<2e-16

# tukey
urchin_tukey <- TukeyHSD(urchin_aov)
urchin_tukey

# results: all sites have a significant difference except for 

# To report your one-way ANOVA, need to unlist the outcomes isla vista and carpenteria

urchin_outputs <- unlist(summary(urchin_aov))
urchin_outputs

# Create a data frame of Tukey to reference p-values through in-line referencing

df <- data.frame(urchin_tukey$site)
df
```

<br>
  
  To determine whether or not there is a difference in total mean sizes for purple urchins (*Strongylocentrotus purpuratus*) found in five different sites along the Santa Barbara coast, a one-way ANOVA test was performed. The results  F(`r urchin_outputs[1]`, `r urchin_outputs[2]`) = `r round(urchin_outputs[7],2)`, *p* = `r round(urchin_outputs[9],4)`) indicate that there is a significant difference between at least two of the mean sizes for purple urchins in different sites. A Tukey's HSD post-hoc test was performed to determine which sites had purple urhcin populations with significant differences between total mean sizes. The results show that there is a significant difference in all but one pairwise comparison: total mean sizes between purple urchins in Isla Vista (`r round(urchin_stats$mean[3], 2)`cm $\pm$ `r round(urchin_stats$sd[3], 2)`cm)(mean $\pm$ SD) and Carpinteria (`r round(urchin_stats$mean[2], 2)`cm $\pm$ `r round(urchin_stats$sd[2], 2)`cm)(*p* = `r round(df$p.adj[5],3)`). The actual difference between the population means was 0.08cm.

All other pairwaise comparisons had significant differences in purple urchin mean sizes (*p* = `r round(df$p.adj[1],3)`). Mohawk had the largest mean population size (`r round(urchin_stats$mean[4], 2)`cm $\pm$ `r round(urchin_stats$sd[4], 2)`cm)(mean $\pm$ SD), which was 0.71cm, 1.11cm, 1.03cm, and 1.23cm larger than mean population sizes ar Arroyo Quemado (`r round(urchin_stats$mean[1], 2)`cm $\pm$ `r round(urchin_stats$sd[1], 2)`cm) (mean $\pm$ SD), Carpinteria (`r round(urchin_stats$mean[2], 2)`cm $\pm$ `r round(urchin_stats$sd[2], 2)`cm), Isla Vista (`r round(urchin_stats$mean[3], 2)`cm $\pm$ `r round(urchin_stats$sd[3], 2)`cm)(mean $\pm$ SD), and Naples (`r round(urchin_stats$mean[5], 2)`cm $\pm$ `r round(urchin_stats$sd[5], 2)`cm) (mean $\pm$ SD), respectively. Difference in mean urchin sizes at the other sites, Carpinteria, Isla Vista, and Naples, was 0.4cm, 0.32cm, and 0.52cm. The difference in mean sizes for urchins in Naples from Isla Vista and Carpinteria was 0.2cm and 0.12cm.



<br>
<br>
<br>


################################### ESM 262 HW 1 ###########################################


## Assignment 1 - Hannah Garcia-Wickstrum & Anne-Marie Parkinson 

# This function determines global per capita growth rate based on average years of education completed, average daily kilocalorie intake per capita, HIV percent prevalence, and the gross domestic product.

## function variables-----------------------------------------------------------------------------

# 'intercept' was derived from a multiple linear regression model (see below)
# 'education' coefficient was derived from a multple linear regression model, units = years (see below)
# 'kcal' coefficient was derived from a multple linear regression model, units = KCal (see below)
# 'hiv' coefficient was derived from a multiple linear regression model, units = % prevalence between ages 15 - 49
# 'gdp' (gross domestic product) coefficient was derived from a multiple linear regression model, units = USD ($)
# 'education_variable' is the global mean years of education completed, units = years
# 'kcal_variable' is the global average daily kilocalorie intake per capita, units = KCal
# 'hiv_variable' is the percent prevalence of the disease in people ages 15 - 49, units = %
# 'gdp_variable' is the global dollar amount of per capita gdp, units = USD ($)

## load packages ---------------------------------------------------------------------------------

library(tidyverse)
library(dplyr)

## linear regression model used to get coefficients ---------------------------------------------

#lm(global_dnndt ~ education + kcal + hiv + gdp, data = population_data)

## function to estimate global dN/Ndt------------------------------------------------------------

global_dNNdt = function(education_variable, kcal_variable, hiv_variable, gdp_variable, 
intercept=0.014, education=-0.001104, kcal=-0.000000004785, hiv=0.0005353, gdp =0.0000001285) {

# set parameters for education and kilocalories
if (education_variable < 0) return("education_variable cannot be less than zero") 
if (education_variable > 30) return("education_variable cannot be > 30")

if (kcal_variable < 0) return("kcal_variable cannot be < 0")
if (kcal_variable > 25000) return("kcal_variable cannot be > 25000")

if (hiv_variable < 0) return("hiv_variable cannot be < 0")
if (hiv_variable > 100) return("hiv_variable cannot be > 100")

if (gdp_variable < 0) return("gdp_variable cannot be < 0 ")

# equation
result = intercept + (education*education_variable) + (kcal*kcal_variable) + (gdp*gdp_variable) + (hiv*hiv_variable)

return(result)
} 

## test function ------------------------------------------------------------------------------------
#global_dNNdt(-1, 11404.616, 10.3004753, 570.7973)


```{r}
library(dplyr)
library(tidyverse)
```


```{r}
# generate test data (1/2)
set.seed(123456)
nsample = 500
education_data1 = rnorm(mean=6.2, sd=2.8, n=nsample)
kcal_data1 = rnorm(mean=5050, sd=3151, n=nsample)
hiv_data1 = rnorm(mean=0.49, sd=1.86, n=nsample)
gdp_data1 = rnorm(mean=5900, sd=5800, n=nsample)


# create data frame 
test_dataframe1 <- data.frame(education_data1 = education_data1, 
kcal_data1 = kcal_data1, 
hiv_data1 = hiv_data1, 
gdp_data1 = gdp_data1,
global_dNNdt_predicted1 = NA) 

# predict global dNNdt using function and test data
test_dataframe1$global_dNNdt_predicted1 <- global_dNNdt(education_variable = education_data1, 
kcal_variable = kcal_data1, 
hiv_variable = hiv_data1, 
gdp_variable = gdp_data1)


# Create a graph 

ggplot(test_dataframe1, aes(x = education_data1, y = global_dNNdt_predicted1)) +
geom_point() +
labs(title = "Effect of education levels on global per capita growth rate (dN/Ndt)", x = "Global Average Years of Education", y = "Predicted Global dN/Ndt")
```
Figure 1: Effect of education level on global per capita growth rate (dN/Ndt)
The above graph shows the relationship between the years of education completed and the global per capita growth rate. The lower the amount of education completed, the higher the per capita growth rate. When more years of education are completed, the lower the per capita growth rate. 


```{r}

# generate test data (2/2)
nsample = 500

education_data2 = runif(min=0.5, max = 15, n=nsample)
kcal_data2 = runif(min=1000, max=15000, n=nsample)
hiv_data2 = runif(min=0, max=11, n=nsample)
gdp_data2 = runif(min=500, max=27000, n=nsample)

#create data frame
test_dataframe2 <- data.frame(education_data2 = education_data2, 
kcal_data2 = kcal_data2, 
hiv_data2 = hiv_data2, 
gdp_data2 = gdp_data2,
global_dNNdt_predicted2 = NA)

# predict global dNNdt using function and test data
test_dataframe2$global_dNNdt_predicted2 <- global_dNNdt(education_variable = education_data2, 
kcal_variable = kcal_data2, 
hiv_variable = hiv_data2, 
gdp_variable = gdp_data2)

# Create a graph

ggplot(test_dataframe2, aes(x = education_data2, y= global_dNNdt_predicted2)) +
geom_point() +
labs(title = "Effect of education levels on global per capita growth rate (dN/Ndt)", x = "Global Average Years of Education", y = "Predicted Global dN/Ndt")

ggplot(test_dataframe2, aes(x = education_data2, y= global_dNNdt_predicted2)) +
geom_boxplot() + 
labs(title = "Boxplot of education levels on global per capita growth rate (dN/Ndt)", x = "Global Average Years of Education", y = "Predicted Global dN/Ndt")

```

Figure 2: Effect of education level on global per capita growth rate (dN/Ndt)
The above graph shows the relationship between the years of education completed and the global per capita growth rate. The lower the amount of education completed, the higher the per capita growth rate. When more years of education are completed, the lower the per capita growth rate.

Figure 3: Boxplot of education levels on global per capita growth rate (dN/Ndt)
The above graph shows a median of approximately 0.011 for dN/Ndt globally.


####################################### ESM 262: Practice function ###############################


library(tidyverse)

# function---------------------------------

temperature_converter <- function(temp_C) {

# set parameters for the temperature
if (temp_C < -100) return("Tempmerature cannot be < -100")
if (temp_C > 100) return("Tempmerature cannot be > 100")

fahrenheit <- temp_C*(9/5) + 32

return(fahrenheit)
}

# test function ----------------------------
temperature_converter(temp_C = 120)

```

####################### ESM 263: class practice diversity function #################################

### try generating springs

#one input
dailytemp = 10
sprintf("the temperature today is %d", dailytemp)

# 2 inputs
tmrtemp=12
sprintf("the temperature today is %d and tomorrow the temperature will be %d", dailytemp, tmrtemp)

## species diversity function

function (species) {

species = as.factor(species)
tmp = (summary(species)/sum(summary(species)))^2
diversity = 1 - sum(tmp)
nspecies = length(summary(species))
tmp = which.max(summary(species))
dominant = names(summary(species)[tmp])
return(list(num = nspecies, simpson = diversity, dominant = dominant))


}

######################### ESM 262: march2 function practice ###############################

### try generating springs

#one input
dailytemp = 10
sprintf("the temperature today is %d", dailytemp)

# 2 inputs
tmrtemp=12
sprintf("the temperature today is %d and tomorrow the temperature will be %d", dailytemp, tmrtemp)

## species diversity function --------------------------------------------------
library(tidyverse)
library(here)

flowers <- read.table(here::here("datafiles", "datafiles", "flowers.txt" ))

diversity_index = function (species) {

species = as.factor(species)
tmp = (summary(species)/sum(summary(species)))^2
diversity = 1 - sum(tmp)
nspecies = length(summary(species))
max_species = which.max(summary(species))
dominant = names(summary(species)[max_species])
min_species = which.min(summary(species))
rare = names(summary(species)[min_species])

return(list(num = nspecies, simpson = diversity, dominant = dominant, rare = rare))

}

diversity_index(flowers$V1)

#least frequent flower=rose
#mos frfequent=daisy

####################################### ESM 262: Feb24and26_original #####################################



# Random Numbers as Inputs

* sample from distributions (normal, uniform, poisson), which distribution depends on the model
* R has many tools for generating samples from distributions with known parameters (such as mean, standard deviation, or min/max)
*  generating rainfall for a hydrologic model given know mean and variance of rainfall

* R also has tools for picking samples from collections 
* generating fish catches based on populations for an economic model


## Steps for running your model over multiple inputs

1. design a data structure to store results: sometimes this is automatic but not always
2. generate the input data
3. apply to the model




# Code

```{r powerexample}

#' Power Required by Speed
#'
#' This function determines the power required to keep a vehicle moving at 
#' a given speed
#' @param cdrag coefficient due to drag default=0.3 
#' @param crolling coefficient due to rolling/friction default=0.015
#' @param v vehicle speed (m/s)
#' @param m vehicle mass (kg)
#' @param A area of front of vehicle (m2)
#' @param g acceleration due to gravity (m/s) default=9.8
#' @param pair (kg/m3) default =1.2
#' @return power (W)

autopower = function(V, m, A, cdrag=0.3, crolling=0.015,pair=1.2,g=9.8) {
  P = crolling*m*g*V + 1/2*A*pair*cdrag*V**3
  return(P)
}

```



# Sampling from Collections to Generate Data

Lets **scale** up from a single car to a group of cars on a highway
and use our **autopower** function to estimate a distribution of power 

What might vary?
  
  # Our highway
  
  3 car types 

Imagine with have 3 different car types - and we know how often each occurs:
  
  * car A  mass 31000 kg, area 25 m^2^
  
  * car B mass 45000 kg, area 30 m^2^
  
  * car C mass 38000 kg area 22 m^2^
  
  Mean highway speed is 100 km/hr 

40% of cars are A, 40% of cars are B and 20% are C

```{r sampling, eval=TRUE, echo=TRUE}


# generate a structure to store info on our possible cars
possible_cars = data.frame(name = c("A","B","C"),mass=c(31000,45000,38000), area = c(25,30,22))

# first look at how results vary for mean speed say 100km/hr
# do conversion
speed_base = 100 * 0.28

# because I have one mass and area for each car and only 1 speed
# I can estimate power for each car type
# add to the data structure
possible_cars$power = autopower(V=speed_base, A = possible_cars$area, m=possible_cars$area)

head(possible_cars)

# show results                         
ggplot(possible_cars, aes(x=mass, y=power, fill=area))+geom_col()+labs(y="Power W", x="Mass (kg)")

```

# Building our highway

What could be the total power consummed if there are 100 cars on this highway each hour,
they are not all travelling at 100km/hr - but that is the average (and speeds tend to
                                                                   be normally distributed)


How could we take into account probbility of a given car? AND distribution of speeds?
  
  
  We will use sample here

```{r sampling2}

# what is I want to estimate average power use given different probabilities of particular cars

# define probablity, must sum to 1 ?
# why
possible_cars$prob = c(0.4, 0.4, 0.2)

possible_cars

# use sample to generate test cases
# first generate our data structure
# assume log normal distribution of speeds with mean 100km/hr
# recall our function needs spped in m/s not km/hr

nsample = 100
mean_speed = log(100*0.277)

speeds = rlnorm(mean=mean_speed, sd=0.1*mean_speed, nsample)
summary(speeds)

plot(density(speeds), ylab="Distribution of Speeds in (m/s)")


# create a new data frame to store results from sampling
results = data.frame(speed=speeds, power=NA)


# for each speed guess which car
# use sample
# why base?
# give each car type an id

possible_cars$row = seq(from=1, to=nrow(possible_cars))
whichcar = base::sample(possible_cars$row, size=nsample, prob=possible_cars$prob, replace=TRUE)

# what is whichcar?
head(whichcar)
possible_cars[whichcar,]


# add a car to the our data structure that contains speeds
results$mass = possible_cars$mass[whichcar]

head(results)

#  add the area
results$area = possible_cars$area[whichcar]

# now lets get power for all of our samples of speed and car type
results$power = autopower(A=results$area, V=results$speed, m=results$mass)

summary(results$power)
ggplot(results,aes(x="", y=power/1000))+geom_boxplot(fill="red")+labs("Power kW")


# try adding an additional car type - with mass 30000 and area 10

# what if we use more samples (200); how do estimates of mean power change



```


# Exploring multiple dimensions at once

We might also want to compute for a range of drag coefficients

Say for 0.1 to 0.4 in steps

This gets a bit more complicated - what if we want to look at our samples of different cars for EACH drag coefficient

# Loops

Available in all progamming language
A type of "flow control" - flow from on instruction to the next

* For
* While
* If/else
  
  In R, *apply* family often replaces loops (although traditional loops also available)

# Use sapply


We also take advantage of a *sapply* function definition that allows use to run multiple steps for each value in the sequence

Syntax is
sapply(sequence, function, parameters)  

OR to define a couple of steps on the fly

sapply(sequence, function(parms) {definition})

See example below

```{r sampling3}

# create a sequence of drag efficienies
cdrag = seq(from=0.3, to=0.5, by=0.05)
length(cdrag)


# use sapply to run for each value of cdrag, for car with area=50, mass=2000 and speed 80m/s
sapply(cdrag, autopower, A=50, V=80, m=2000)
cdrag


# now lets do this for our highway
View(results)
res = sapply(cdrag,  autopower, A=results$area, V=results$speed, m=results$mass)

# we needed a new data structure
# can you guess what is in this data structure - what are columns and what are rows
head(res)



# rearrange to plot - common way to get data into a form that works with ggplot
colnames(res)=cdrag
resl=as.data.frame(res) %>% gather(cdrag, power)
ggplot(resl, aes(cdrag, power, fill=cdrag))+geom_boxplot() + labs(y="Rangoe of Power (W/s", "Drag Coefficient")

# what if we design new highways that have lower rolling coefficients 
#  we can reduce the rolling coefficient by 50%
# or we can reduce the mean speed to 80 km/h (still with 10% standard deviation)
# calculate mean power for both (assuming the same car probabilities above)
# which is better



```


# What we've learned so far

* how to make a function
* how to generate data for a function
* by sampling from know distributions (e.g normal)
* sampling from a sequence with assigned probablities
* how to repeat our function for multiple parameter values
* how to create data structures to store the output of multiple uses of the function


#  <span style="color:blue"> A bit more on Looping <\span>

Loops - similar to apply but more general

```{r loop }

# repeat statement
a=0
for (i in 1:5) {
  a = a+i
}
a

# find the maximum speed
speeds = runif(min=0, max=100, n=300)

maxspeed=0
for ( i  in 1:length(speeds)) {
  maxspeed = ifelse(speeds[i] > maxspeed, speeds[i], maxspeed)
}

maxspeed
max(speeds)

head(results)

# apply's are R's internal loops - FAST
# by column
results_means = apply(results, 2, mean)
# by row
silly = apply(results,1,mean)

# make a for loop to compute results_means
```

```{r}
# try to get meanspeed 

speeds = runif(min=0, max=100, n=300)

for ( i  in 1:length(speeds)) {
  
  sum_speeds = sum_speeds + speeds[i]
}

mean_speeds/length(speeds)
mean(speeds)
```

# Loops can be "nested" on loop in side the other

Exampe: Calculate NPV or a range of different interest rates and a range of damages that may be incurred 10 years in the future

Steps

* define inputs (interest rates, damages)
* define output (NPV)
* write the function
* create a data structure to store results where we vary both interest rates and damages
* use nested for loops to fill in the data structure

Try it first...


```{r npvfor, echo=FALSE}

# write a function to compute npv
#source("../../src/dataf/R/compute_NPV.R") # dont have the scrpit file

#function to compute NPV
compute_NPV = function(value, time, discount){
  result = value/(1 + discount)^time
  return(result)
}

compute_NPV(20, discount=0.01, time=20)


#generate some input
damages = c(25,33,91,24)
# sensitivity to discount rate
discount_rates = seq(from=0.01, to=0.04, by=0.005)
yr=10

# compute some npv's for different discount rates
# first generate a dataframe to store results
npvs = as.data.frame(matrix(nrow=length(damages), ncol=length(discount_rates)))

# now use a for loop to populate
for (i in 1:length(damages)) {
  for (j in 1:length(discount_rates)) {
    npvs[i,j]= compute_NPV(value=damages[i], discount=discount_rates[j],time=yr )
    
  }
}
npvs


#some data wrangling
colnames(npvs)=discount_rates
rownames(npvs)=damages
npvs


npvs = gather(npvs, 1:7, key=dis, value=npv)
head(npvs)
ggplot(npvs, aes(x=npv, col=as.factor(dis)))+geom_density(size=2)+scale_color_brewer(type="seq", name="Discount")

# how about summing all the damages
npv.total =npvs %>% group_by(dis) %>% summarize(t=sum(npv))
ggplot(npv.total, aes(dis,t, fill=dis))+geom_col() + labs(x="Discount Rate", y="Total ($)")


```

# Some other types of loops

* while
useful for repeating until a condition is met

Example
if a metal toxin in a lake increases by 1% per year, how many years will it take for the metal level to be greater than 30 units, if toxin is current at 5 units


```{r} 

# accumulate pollutant until a threshold - how many years does it take

# initial conditions
yr=1
pollutant_level = 5

# loop
while (pollutant_level < 30)   {
  # increase pollutant
  pollutant_level = pollutant_level + 0.01* pollutant_level 
  # keep track of time
  yr = yr + 1
}

pollutant_level
yr

# while loop dangers

```

# <span style="color:blue"> Data types 

All programing languages use data-types, or structures to hold information

* integer
* floating point/ real / numeric
* character 
* string

Often data types are multi-dimensional 
Some useful ones in R

* vector
* matrix
* data frame
* tibble
* factors
* lists

Programming often involves selecting and building data structures. Like the **res** matrix we built last class to hold the results from our **for** loop

Good data structures are

* as simple as possible
* easy to understand (readable names)
* easy to manipulate 
* easy to visualize

# <span style="color:blue"> Factors \span

something that has different **classes** or **groups**
  useful for doing calculations with categories

Here's an example:

First lets look at a standard numeric vector

```{r} 
a = c(1.3, 1, 4, 1.3, 22)
# compute the mean
mean(a)
```

What if **a** is a factor

What do commands like **mean** do
```{r} 
a = as.factor(a)
# compute the mean
mean(a)

#why? lets look
a




```

We can use **summary** with factors to get frequencies in each category (or “level” )



```{r fishes}

# create vector of possible fish 
possible.fish = c("salmon","steelhead","shark","tuna","cod")

# we can use sample to simulate a random recording of catch by fisherman, lets say we pick 20 fish from the net

catch1 = sample(possible.fish, size=20, replace=T)
# because possible.fish was a factor catch1 will be a factor
catch1

summary(catch1)
# if we want summary to be more useful - make this a factor
catch1 = as.factor(catch1)


# to quickly get frequencies of different fish and to plot 
summary(catch1)
plot(catch1, col="blue")


# we can also use summary to explore and return information about the distribution
# mean frequency of a particular type of fish
mean(summary(catch1))

# maximum frequency
max(summary(catch1))

# which fish was most frequently caught
which.max(summary(catch1))

#to get just the name 
names(which.max(summary(catch1)))

# use results for creating text
# sprintf creates a string %s mean use what ever is after the , to fill in a string
plottitle=sprintf("We like %s", names(which.max(summary(catch1))))

plot(catch1, col="blue", main=plottitle)

# you can also add numbers to the string
plottitle=sprintf("We mostly caught %s \n max catch(%d)", names(which.max(summary(catch1))), max(summary(catch1)))
plot(catch1, col="blue", main=plottitle)

#How do you figure out the rarest fish in our simulated ocean

# bigger challenge how would use pre-assign probabilities to different fish and then generate your ocean, hint look at help page for sample
```

# Aside **sprintf**

some useful syntax if you want to generate strings

* **%s** replace with a string
* **%d** replace with an integer value
* **%f** replace with a real value
* **%4.1f** replace with a real value with 4 digist, two after decimal
* **\n** add a line return

Try it

make a string with **sprintf** and add to plot title


# <span style="color:blue"> Functions with factors 

Lets generate a function that makes use of categorical data
species diversity is a good example

"Simpson's Index (D) measures the probability that two individuals randomly selected from a sample will belong to the same species 

Value is between 0 and 1, with lower values associated with *lower* diversity

See 
[Simpson Biodiversity](http://www.countrysideinfo.co.uk/simpsons.htm)


```{r diversity, echo=TRUE}

source("../../src/dataf/R/compute_simpson_index.R")
compute_simpson_index


possible.fish = as.factor(c("salmon","steelhead","shark","tuna","cod"))
# simulate a random recording of catch by fisherman


# note here is answer to above challenge
catch1 = sample(possible.fish, size=10, prob = c(0.2, 0.2, 0.1, 0.1, 0.4), replace=T)
# lets create a test case that should have low diversity, by repeating the same thing
catch2 = c(rep("salmon", times=10), rep("cod", times=10))

compute_simpson_index(catch1)
compute_simpson_index(catch2)

```

What would be a useful error check here!
  
  Repeat for the alternative Simpson Diversity Index
Test on the **fish.txt** 
  
  Divide by zero - one of the most common errors! 
  
  Sometimes you don't want factors and R thinks something should be
How to change back? **as.numeric** makes sense ...but



```{r, echo=TRUE}

a = as.factor(c(1.3, 1, 4, 1.3, 22))
#sum(a)

# try to make a numeric version from the factor
b = as.numeric(a)
sum(b)
b

# better
b = as.character(a)
b = as.numeric(b)
b
sum(b)
```

#  <span style="color:blue"> Returning multiple things from a function \span

In R, to do this we use LISTS

* Lists are the most “informal” data structures in R
* List are really useful for keeping track of and organizing groups of things that are not all the same
* A list could be a table where number of rows is different for each column
* A list can have numeric, character, factors all mixed together
* List are often used for returning more complex information from function (e.g. lm)

```{r introlist, echo=TRUE}

# make a list
sale = list(id=2, quality="high", contents=c("apple","cherry"), cost=c(4,5))
sale

#ways to access elements
sale$id
sale$what

# you can also access by list item number
# the [x] denotes the xth item in the list
sale[[3]]
sale[["contents"]]


# how do you get the second element in the vector that has the contents
# there are two ways


# add to a list
sale$location = "Farmers market"
sale
# or remove
sale$location = NULL
sale

# some tricky things
# correct accessing items in list
sale$cost
sale[[4]]

# works but
#sale[4]


sum(sale$cost)
sum(sale[[4]])

```

# So why use these complex data types?

R functions return *lists* and useful when you don't know how many rows you will need in a data frame or matrix

consider *lm*
  
  
  ```{r lmlist, echo=TRUE}

# read in some streamflow data
sage = read.table("../../src/dataf/data/sagedata.txt", header=T)
names(sage)

# sum to water year
sage_wy = sage %>% group_by(wy) %>% summarize(tavg=mean(tavg), precip=sum(precip), trans=sum(trans), psn=sum(psn))

# regress photosynthesis (psn) against precip
res = lm(psn~precip+wy, data=sage_wy)
summary(res)

#lm returns a list so we can look at the different elements

res$coefficients
res[["coefficients"]]
res[["call"]]



```

# Using lists to return multiple items from a function

We can use *lists* to return multiple,diverse pieces of information from our functions
Lets start with diversity - many be want to know a bit more about the dataset

* Simpson diversity
* most frequent species
* number of distinct species



```{r diversitylist, echo=TRUE}

# repeat with a list
source("../../src/dataf/R/computediversity.R")

computediversity

computediversity(catch1)
computediversity(catch2)
```

In class: Try adding to your diversity function: return the rarest species; 



We can also use parameters to determine flow control in a function


```{r str, echo=FALSE}

source("../../src/dataf/R/compute_season_meanflow.R")

str = read.table("../src/dataf/data/str.txt", header=T)
compute_season_flow(str)

compute_season_flow(str, kind="max")
```

What you've learned

* common data types
* common flow control approaches
* returning multiple items from a function

###################################### ESM 262: computdiversity2 ###########################

#' Describe diversity based on a list of species 
#' 
#' Compute a species diversity index
#' @param species list of species (names, or code) 
#' @return list with the following items
#' \describe{
#' \item{num}{ Number of distinct species}
#' \item{simpson}{Value of simpson diversity index}
#' \item{dominant}{Name of the most frequently occuring species}
#' }
#' @examples
#' computediversity(c("butterfly","butterfly","mosquito","butterfly","ladybug","ladybug")))
#' @references
#' http://www.tiem.utk.edu/~gross/bioed/bealsmodules/simpsonDI.html

computediversity = function(species, index="A", showplot=FALSE) {
  
  species = as.factor(species)
  
  # decide which index calculation to use
  if (index=="B")
    diversity=compute_simpson_indexB(species)
  else
    diversity =compute_simpson_index(species)
  
  sm = as.data.frame(summary(species))
  colnames(sm)="frequency"
  sm$fish = rownames(sm)
  
  if (showplot) 
    p=ggplot(sm,aes(x=fish,y=frequency, fill=fish))+geom_col()
  else
    p=NULL
  
  
  # number of species
  nspecies = length(summary(species))
  
  # which is the most frequent
  tmp = which.max(summary(species))
  dominant = names(summary(species)[tmp])
  
  # output from function
  return(list(num=nspecies, simpson=diversity, dominant=dominant, plt=p))
}

########################### ESM 262: compute simpson index #################################

compute_simpson_index = function(species) {
  
  species = as.factor(species)
  tmp = (summary(species)/sum(summary(species))) ** 2
  diversity = sum(tmp)
  return(diversity)
}


###################################### ESM 262: Feb17 ################################


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo =TRUE)
knitr::opts_chunk$set(error=TRUE)
library(tidyverse)
```

# A reminder about naming objects in R

Note that by *sourcing* a function - it will essentially overwrite anything else in your workspace with the same name 

*Warning* - if you name your function, the same as an internal R function, your new function will take precidence, and **hide** the internal R function

In R, functions are organized in **Packages**
  
  You've probably loaded different packages, that provide different functions
There are a number of packages **base**, **stats** that are automatically loaded
You can usually find the package associated with any function from online help

* consider **runif** function in the **stats** package

[runif](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/Uniform.html)

To use a function associated with a particular package

**package::function**


# Generating Data for your function

You often want to apply your function to a range of data, that can come from

* files you read in to R (measured height and flow rate values)
* output from other functions/models 
* data that you generate
* sensitivity analysis
* testing your model
* stochastic models


# Random Numbers as Inputs

* sample from distributions (normal, uniform, poisson), which distribution depends on the model
* R has many tools for generating samples from distributions with known parameters (such as mean, standard deviation, or min/max)
*  generating rainfall for a hydrologic model given know mean and variance of rainfall

* R also has tools for picking samples from collections 
* generating fish catches based on populations for an economic model


Others?
***

# Steps for running your model over multiple inputs

1. design a data structure to store results: sometimes this is automatic but not always
2. generate the input data
3. apply to the model



# Lets consider a new function estimates the power required to keep a car moving at a given speed



<span style="color:coral">
**Pb = crolling * m * g * V + 1/2 A * pair * cdrag * V3**
</span>

where *crolling* and *cdrag* are rolling and aerodynamic resistive coefficients, typical values are 0.015 and 0.3, respectively

*V*: is vehicle speed (assuming no headwind) in m/s (or mps) m: is vehicle mass in kg

*A* is surface area of car (m2)

*g* is acceleration due to gravity (9.8 m/s2) pair = density of air (1.2kg/m3)

*Pb* is power in Watts


# Code

```{r powerexample}

#' Power Required by Speed
#'
#' This function determines the power required to keep a vehicle moving at 
#' a given speed
#' @param cdrag coefficient due to drag default=0.3 
#' @param crolling coefficient due to rolling/friction default=0.015
#' @param v vehicle speed (m/2)
#' @param m vehicle mass (kg)
#' @param A area of front of vehicle (m2)
#' @param g acceleration due to gravity (m/s) default=9.8
#' @param pair (kg/m3) default =1.2
#' @return power (W)

autopower = function(V, m, A, cdrag=0.3, crolling=0.015,pair=1.2,g=9.8) {
  P = crolling*m*g*V + 1/2*A*pair*cdrag*V**3
  return(P)
}

```

I've also stored the autopower.R function as a file = using the recommended naming convention

**autopower.R**


# Generating data for the function

Example use: Imagine we want to see how much power is generated given scenario where we know the mean and standard deviation of vehicles speeds


```{r sampling}

#source("./src/dataf/R/autopower.R")

# generate sample speeds from a distribution
nsample = 100
speeds = rnorm(mean=25, sd=4, n=nsample)

# Step 1  create data frame to store results 
# how many simulations, what do you want to keep

#create a dataframe that has rows for each model run
# columns for height, flowrate and power estimate
results = data.frame(speed=speeds, power=NA)

head(results)

# if you only have one input and everything elese is the same, R is smart enough
# to create a set of outputs
results$power = autopower(V=speeds, A=25, m=20000)

# ALWAYS look at your results to make sure it make sense
ggplot(results, aes(speed, power/1000))+geom_point()+labs(x="speed in m/s", y="power in kW")
ggplot(results, aes(x="Across Speed",y=power/1000))+geom_boxplot()+labs(y="power in kW")

```


# Multiple inputs 

For a function with more than one input that could have multiple values, R can handle this
if 

* all (multiple) inputs have the same number of values 
* all other inputs or paraameters have a single value


For other cases, additional instructions would need to be included in the function to help R understand what to do


Consider our reservoir function  (see Rmd from functions2 for power_gen.R code)

```{r sampling2}

#source("src/dataf/R/power_gen.R")

power_gen = function(height, flow, rho=1000, g=9.8, Keff=0.8) {
# make sure inputs are positive and if not set as NA so result will be NA
height = ifelse( (height < 0), NA, height)

# an alterative is to exit the function prematurely 
flow = ifelse((flow < 0), return("flow must be greater than zero"), flow)

#    return("Height cannot be less than 0")

# calculate power
result = rho * height * flow * g * Keff
return(result)
}

# single
power_gen(height=2, flow=2)

# multiple heights
# generate a strucutre to store results
# consider all heights from 1 to 100

resv_results = data.frame(height=seq(from=1, to=100))

# compute power generated for EACH height
resv_results$power = power_gen(resv_results$height, flow=2)

# plot
ggplot(resv_results, aes(height, power))+geom_point(size=3)+labs(y="Power (KW)", x="Height (m)")

# random combinations of height and flow
resv_results$height = rnorm(mean=10, sd=2, n=100)
resv_results$flow = rnorm(mean=20, sd=1, n=100)
# compute power generated for EACH height
resv_results$power = power_gen(resv_results$height, flow=resv_results$flow)

# plot
ggplot(resv_results, aes(height, power, col=flow))+geom_point(size=3)+labs(y="Power (KW)", x="Height (m)")

# Unequal lengths - this will "work" but inputs get recycled unclear what is happening
# NOT recommended

eheight = seq(from=1, to=50)
eflow = runif(min=0.1, max=2, n=100)
resv_result = power_gen(eheight, eflow)

# better
eheight = seq(from=1, to=50)
eflow = runif(min=0.1, max=2, n=50)
resv_result = power_gen(eheight, eflow)

# another example of the problem of unequal length inputs
power_gen(eheight, eflow, g=c(9,10,11))

#better
g = runif(min=8, max=10, n=50)
resv_result = power_gen(eheight, eflow, g)


```
# Assignment 1


With a partner

Write function to compute something that is of interest to you, make sure it has at least 2 inputs and several parameters

Add some error checking (at least two different kinds)

Save your function in a .R file

(Make sure you add comments to your function)

In an Rmarkdown document, generate saample data for your function in two different ways, 

Apply the data to your function

Graph and add some text describing how to interpret the results

Upload both the function and the Rmarkdown file to your github - in a subfolder called Assignment 1

Add a link to your github site for Assignment 1 on Gauchospace

################################## ESM 262: climateimpacts - R #########################################


library(devtools)
library(roxygen2)

meanwinterprecip = function(sum_winter_precip, no_of_samples) {
# set parameters
if (sum_winter_precip < 0) return("sum_winter_precip cannot be less than zero")
if (no_of_samples < 0) return("no_of_samples cannot be less than zero")

# equation
result = (sum_winter_precip/no_of_samples)

return(result)
}

# Mean Winter Precipitation Package
# Data and Documentation

# upload packages
library(tidyverse)

precip_inches = runif(min=2, max=35, n=100)
ca_city = seq(1, 100, by=1)

precip_totals = data.frame(precip_inches, ca_city)

view(precip_totals)

# Documentation for mean winter precipitation data

#' Mean Winter Precipitation
#'
#' This function calculates mean winter precipitation. Data from Anne-Marie Parkinson & Hannah Garcia-Wickstrum, winter precipitation measurements from 100 California cities
#'
#' @format A data frame with 100 rows and 2 columns
#' \describe{
#' \item{precip_inches}{total precipitation between 2 and 25 inches (in)}
#' \item{ca_city}{California cities numbered 1-100}
#' }
#'
#' @authors Anne-Marie Parkinson & Hannah Garcia-Wickstrum, fake climate scientists
#' @return mean winter precipitation (cm)


# uplaod packages
library(devtools)
library(roxygen2)
library(tidyverse)

#' Maximum and Minimum Temperatures
#'
#' This function returns the maximum and minimum temperatures from a list
#' @param df refers to the data frame which has a list of temperatures
#' @author Anne-Marie and Hannah
#' @examples max_min_temp(ex_df)
#' @return maximum and minimum temperatures

max_min_temp = function(df) {
  # parameters
  mintemp = apply(df, 2, min)
  maxtemp = apply(df, 2, max)
  return(list(min=  mintemp, max = maxtemp))
}


# Documentation for maximum and minimum temperature data

#' example_temp_df from (package name here)
#'
#' Data from Anne-Marie Parkinson & Hannah Garcia-Wickstrum, list of 100 temperatures
#'
#' @format A data frame with 100 rows and 1 column
#' \describe{
#' \item{temp}{recorded temperature between 35 and 110 degrees F}
#' }
#'
#' @source: Anne-Marie Parkinson & Hannah Garcia-Wickstrum, fake climate scientists
#'

#' Index to assess risk of home loss in the event of a wildfire
#'
#' Scientists predict more frequent and more severe wildifre events in the event of climate chage. However, many areas
#' are increasing in population size which results in developers and cities building into the Wildland Urban Interface (WUI)
#' which has higher occruances of wildlifre events. Parameters include both weather variables, fire characteristics,
#' and characteristics of a home.
#'
#' @param defensible_space  is the area around a home or other structure that has been modified to reduce fire hazard. Options are between 0 to +100ft.
#' @param fire_proofing_degree  amount of fire proofing done by a household to prevent things like embers from entering the home or reduce flamability of the house.
#' values ranked from 0 (none) to 5 (very high).
#' @param temp  temperature (degrees F)
#' @param humidity humidity (percent)
#' @param wind_speed  wind speed (mph)
#' @author Hannah Garcia-Wickstrum and Anne-Marie Parkinson
#' @examples home_risk_index(data_frame)
#' @return Ranking (Very Low to Extreme) of ow vulnerable a home is being damaged by wildfire during a wildfire event.



# function
home_risk_index = function(index_data) {
  
  
  
  # assign variable values
  defensible_space_score = ifelse(index_data$defensible_space == 0, 5,
                                  ifelse(index_data$defensible_space < 30, 3,
                                         ifelse(index_data$defensible_space <= 99, 2,
                                                ifelse(index_data$defensible_space >= 100, 2, NA))))
  
  fire_proofing_score = ifelse(index_data$fire_proofing_degree == 0, 5,
                               ifelse(index_data$fire_proofing_degree == 1, 4,
                                      ifelse(index_data$fire_proofing_degree == 2, 3,
                                             ifelse(index_data$fire_proofing_degree == 3, 3,
                                                    ifelse(index_data$fire_proofing_degree == 4, 2,
                                                           ifelse(index_data$fire_proofing_degree == 5, 1, NA))))))
  
  temperature_score = ifelse(index_data$temp < 50, 1,
                             ifelse(index_data$temp < 60, 2,
                                    ifelse(index_data$temp < 70, 3,
                                           ifelse(index_data$temp < 80, 4,
                                                  ifelse(index_data$temp >= 80, 5, NA)))))
  
  humidity_score = ifelse(index_data$humidity < 10, 5,
                          ifelse(index_data$humidity < 25, 4,
                                 ifelse(index_data$humidity < 50, 3,
                                        ifelse(index_data$humidity < 60, 2,
                                               ifelse(index_data$humidity < 100, 1)))))
  
  wind_speed_score = ifelse(index_data$wind_speed < 5, 1,
                            ifelse(index_data$wind_speed < 10, 2,
                                   ifelse(index_data$wind_speed < 20, 3,
                                          ifelse(index_data$wind_speed < 40, 4,
                                                 ifelse(index_data$wind_speed < 150, 5)))))
  
  # determine vulnerability score
  vulnerability_score = (defensible_space_score + fire_proofing_score + temperature_score +
                           humidity_score + wind_speed_score)/5
  
  # qualitative vulnerability score
  vulnerability_assessment = ifelse(vulnerability_score <= 1, "very Low",
                                    ifelse(vulnerability_score <= 2, "Low",
                                           ifelse(vulnerability_score <= 3, "Moderate",
                                                  ifelse(vulnerability_score <=4, "High",
                                                         ifelse(vulnerability_score <= 5, "Extreme")))))
  
  vulnerability_assessment = as.data.frame(vulnerability_assessment)
  
  # combine score to entered data frame
  vulnerability_assessment_df = cbind(index_data, vulnerability_assessment)
  
  
  return(vulnerability_assessment_df)
  
}

# test function
home_risk_index(mock_home_fire_data)


# issues:
#function works when data frame has 1 row, error when there are more than 1 rows: Error in ifelse(index_data$fire_proofing_degree == 5, 1) : argument "no" is missing, with no default (dont get this error with one row)
#issue when try to document: Error in runif(min = 0, max = 150, n = 2) : could not find function "runif" --> idk why this matters. i just want documentation just for the function not the data for now

########################################### ESM 262: climateimpacts - Data ################################

# Data and documentation
temp = runif(min=35, max=110, n=100)

# Create a data frame
example_temp_df = data.frame(temp)

# run function
max_min_temp(example_temp_df)


#library(tidyverse)

# mock data -----------------------------------

# defensible_space = sample(x=c(0, 30, 90, 120), size = 100, replace = T)
# fire_proofing_degree = sample(x=1:5, size = 100, replace = T)
# temp = runif(min = 40, max = 105, n = 100)
# humidity = runif(min = 0, max =100, n = 100)
# wind_speed = runif(min = 0, max = 150, n = 100)
# home = seq(1, 100, by=1)
#
# # combine mock data into a data frame
# mock_home_fire_data = data.frame(defensible_space, fire_proofing_degree, temp, humidity, wind_speed, home)
#


# upload packages
library(tidyverse)
library(devtools)
library(roxygen2)

precip_inches = runif(min=2, max=35, n=100)
ca_city = seq(1, 100, by=1)

precip_totals = data.frame(precip_inches, ca_city)

view(precip_totals)

# Documentation for mean winter precipitation data

#' precip_totals from (package name here)
#'
#' Data from Hannah Garcia-Wickstrum, winter precipitation measurements from 100 California cities
#'
#' @format A data frame with 100 rows and 2 columns
#' \describe{
#' \item{precip_inches}{total precipitation between 2 and 25 inches (in)}
#' \item{ca_city}{California cities numbered 1-100}
#' }
#'
#' @source: Hannah Garcia-Wickstrum, fake climate scientist
#'

##################################### ESM 262: climateimpacts - tests #######################################


## load library
library(testthat)

## test
test_that('max_min_temp_works',
          
          {# make sure the calculated minimum temp is always less than the max temp
            expect_true(max_min_temp(example_temp_df)$min < max_min_temp(example_temp_df)$max)
            
            #make sure the values in the data are class double
            expect_type(example_temp_df$temp, "double")
            
            # make mock data to test function (trials bc they aint workin)
            mock_data = data.frame(mock_temp = c(23, 44, 55, 62, 79, 89))
            
            #use mock data to test function
            expect_equal(unname(max_min_temp(mock_data)$max), 89) # should work
            expect_equal(unname(max_min_temp(mock_data)$max), 44) # should produce an error
            expect_equal(unname(max_min_temp(mock_data)$min), 23) # should work
            expect_equal(unname(max_min_temp(mock_data)$min), 44) # should produce an error
            
          })

# use unname to remove the names/dimnames. Needed it for this code because the function needs a data frame
# while expect_equal needs a vector. So unname allows me to run the correct data format through the function and the test



########################################## ESM 262: draft final amp hgw ########################################


```{r}

#load packages ---------------------------------------------------
library(tidyverse)
library(tibble)


# create test data -------------------------------------------------------------

# generate vectors of fish species: tuna, mahimahi, shark, salmon, flounder. 
fish_species = c("tuna", "mahimahi", "shark", "salmon", 'flounder')

# generate vectors of catch locations: Naples, Mohawk, Carpinteria, Isla Vista.
catch_locations = c("Naples", "Mohawk", "Carp", "IV")

# generate vectors of fish prices
set_price = runif(min=7, max=15, n=length(fish_species))

# Create a table of prices for fish
fish_prices = data.frame(fish = fish_species, price = set_price) %>%
  arrange(fish)

# Create catch data for locations
catch_Naples = sample(fish_species, size = 100, prob = c(0.1, 0.4, 0.2, 0.15, 0.15), replace = T)
catch_Mohawk = sample(fish_species, size = 100, prob = c(0.3, 0.2, 0.15, 0.2, 0.15), replace = T)
catch_Carp = sample(fish_species, size=100, prob = c(0.4, 0.12, 0.18, 0.1, 0.2), replace = T)
catch_IV = sample(fish_species, size=100, prob = c(0.3, 0.17, 0.2, 0.23, 0.1), replace = T)

# create data-frame of fish caught at each location
catch = data.frame(catch_Naples, catch_Mohawk, catch_Carp, catch_IV) # values already recognized as a factor


```

```{r}
# Output 1: most common fish caught at location
# Function catch_summary (see catch_summary.R)

catch_summary(catch)

# FINAL: most fish type caught at each location
# Naples: mahimahi
# Mohawk: tuna
# Carpinteria (Carp): tuna
# Isla Vista (IV): tuna
```

```{r}
# Output 2: total revenue for each location
# Function location_revenue (see location_revenue.R)
location_revenue = function(fish){
  
  #create data frame to store outputs
  location_revenue_df = data.frame(matrix(ncol = ncol(catch), nrow = nrow(fish_prices)))
  
  # count number of fish
  for(i in 1:ncol(catch)) {
    location_revenue_df[i]=summary(catch[[i]])
  }
  
  #add column names
  colnames(location_revenue_df) = colnames(catch) 
  
  # add row for fish species and move this row to the front
  names=names(summary(catch[[1]]))
  location_revenue_df$fish = names
  location_revenue_df <- location_revenue_df %>% 
    dplyr::select(fish, everything())
  
  # add prices
  fish_price_df = full_join(location_revenue_df, fish_prices, by = "fish")
  
  # new df to store location revenue
  price_df = data.frame(matrix(ncol = ncol(catch), nrow = nrow(fish_prices)))
  
  # math
  for (i in 2:ncol(location_revenue_df)) {
    price_df[,i] = location_revenue_df[,i] * fish_price_df$price
  }
  
  return(price_df)
}

location_revenue(catch)

final_revenue_total <- colSums(location_revenue(catch(2:5)))

final_revenue_total_df <- as.data.frame(final_revenue_total)

final_revenue_total_df 



```


```{r}
# Output 3: total fisheries revenue sum

total_fisheries <- colSums(final_revenue_total_df, na.rm=TRUE)

total_fisheries_df <- as.data.frame(total_fisheries)

total_fisheries_df

# FINAL: total revenue from all locations = $3,687.85
```


```{r}
# Graph of revenue by location (as text, whatever that means)

# if(graph == TRUE) 
# {
#   total_fisheries_df_graph=(data=final_revenue_total_df, rows = (V2:V:5), names_to = "catch_locations", values_to = "location_revenue_df")
# 
#   revenue_graph=ggplot(data=total_fisheries_df_graph, aes(x=catch_locations, y=revenue)) +
#     geom_col()
# 
# return(list(frequency, final_revenue_total_df, total_revenue, revenue_graph))
# 
# }

final_revenue_total_df_2 <- final_revenue_total_df[-c(1),]
names(final_revenue_total_df_2) <- c("Naples", "Mohawk", "Carp", "IV")
final_revenue_total_df_2 <- as.data.frame(final_revenue_total_df_2)

# Still no idea. This is way too complicated. 

revenue_graph <- ggplot(data=final_revenue_total_df_2, aes(y=V1)) +
  geom_col()

revenue_graph
```

######################################## ESM 262: functions assignment 2 #####################################

# Location Revenue function (used to calculate output 2 of assignment 2)

location_revenue = function(fish){
  
  #create data frame to store outputs
  location_revenue_df = data.frame(matrix(ncol = ncol(catch), nrow = nrow(fish_prices)))
  
  # count number of fish
  for(i in 1:ncol(catch)) {
    location_revenue_df[i]=summary(catch[[i]])
  }
  
  #add column names
  colnames(location_revenue_df) = colnames(catch) 
  
  # add row for fish species and move this row to the front
  names=names(summary(catch[[1]]))
  location_revenue_df$fish = names
  location_revenue_df <- location_revenue_df %>% 
    dplyr::select(fish, everything())
  
  # add prices
  fish_price_df = full_join(location_revenue_df, fish_prices, by = "fish")
  
  # new df to store location revenue
  price_df = data.frame(matrix(ncol = ncol(catch), nrow = nrow(fish_prices)))
  
  # math
  for (i in 2:ncol(location_revenue_df)) {
    price_df[,i] = location_revenue_df[,i] * fish_price_df$price
  }
  
  return(price_df)
}

# output 1: most common fish caught at location
# Dom fish function 

dom_fish = function(fish){
  max_species = which.max(summary(fish))
  dom = names(summary(fish) [max_species])
  return(dom)
}

dom_fish(catch$catch_Mohawk)

# Catch summary function 
# used for output 1 on assignment 2

catch_summary = function (fish) {
  
  #create data frame to store outputs
  dom_df = data.frame(matrix(ncol = 2, nrow = ncol(catch)))
  dom_df = setNames(dom_df, c("most_frequent_fish", "location"))
  
  # determine most frequent fish caught in each location
  for (i in 1:ncol(fish)) {
    #dom_df[[i, 1]] = which.max(summary(catch[[i]]))  # dont need this bc it reports the number of occurances of the most frequent fish and we just want the name
    dom_df[[i, 1]] = names(which.max(summary(fish[[i]])))
    dom_df[[i, 2]] = colnames(fish)[i]
  }
  return(dom_df)
}

catch_summary(catch)









