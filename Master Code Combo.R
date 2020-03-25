
####################### ESM 244: Lab 1 ##############################################
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












    






















