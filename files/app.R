library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinybusy)
library(readxl)
library(dplyr)
library(tidyverse)
library(maps)
library(mapproj)
library(ggplot2)
library(plotly)
library(gganimate)
library(scales)
library(readr)
library(chron)
library(sf)
source("helpers.R")

raw_17 <- read_csv("data/sqf-2017.csv")
raw_18 <- read_csv("data/sqf-2018.csv")
raw_19 <- read_csv("data/sqf-2019.csv")
precinct_pop <- read_csv("data/precinct_pop.csv")


precinct_pct <- precinct_pop %>% 
  mutate(pct_blk = black / total_pop, 
         pct_hisp = hisp_latino / total_pop,
         pct_white = white / total_pop) %>% 
  arrange(desc(pct_blk)) %>% 
  dplyr::select(precinct_2020, total_pop, pct_blk, pct_hisp, pct_white)
# get vectors containing precincts with majority Black, Hispanic, and White populations
blk_maj <- precinct_pct$precinct_2020[precinct_pct$pct_blk >= 0.5]
hisp_maj <- precinct_pct$precinct_2020[precinct_pct$pct_hisp >= 0.5]
white_maj <- precinct_pct$precinct_2020[precinct_pct$pct_white >= 0.5]


# shift over the mis-entered data from the sqf-17 csv file
#https://stackoverflow.com/questions/58212456/shifting-all-data-in-row-over-one-column
rows <- raw_17$SUSPECT_RACE_DESCRIPTION == "MALE"
raw_17[rows, 63:(ncol(raw_17) - 1)] <- raw_17[rows, 64:ncol(raw_17)] 
raw_17[rows, ncol(raw_17)] <- NA
# Fix precinct incorrect entry
rowz <- raw_17$STOP_LOCATION_PRECINCT == 208760
raw_17$STOP_LOCATION_PRECINCT[rowz] <- raw_17$STOP_LOCATION_PREMISES_NAME[rowz] 
# change data types of select columns in 2017 dataset to match 2018 and 2019 datasets
raw_17$STOP_FRISK_DATE <- as.character(raw_17$STOP_FRISK_DATE)
raw_17$ISSUING_OFFICER_COMMAND_CODE <- as.double(raw_17$ISSUING_OFFICER_COMMAND_CODE)
raw_17$SUPERVISING_OFFICER_COMMAND_CODE <- as.double(raw_17$SUPERVISING_OFFICER_COMMAND_CODE)
raw_17$STOP_LOCATION_PRECINCT <- as.double(raw_17$STOP_LOCATION_PRECINCT)
raw_17$STOP_LOCATION_X <- as.double(raw_17$STOP_LOCATION_X)
raw_17$STOP_LOCATION_Y <- as.double(raw_17$STOP_LOCATION_Y)
raw_17$STOP_FRISK_TIME <- as.character(raw_17$STOP_FRISK_TIME)
# change Stop Frisk Time variable name in 2018 to match 2017 and 2019 datasets
names(raw_18) <- toupper(str_replace_all(names(raw_18), c(" " = "_")))
# change data types of select columns in 2019 dataset to match
raw_19$STOP_FRISK_TIME <- as.character(raw_19$STOP_FRISK_TIME)
raw_19$SUSPECT_HEIGHT <- as.character(raw_19$SUSPECT_HEIGHT)
raw_19$SUSPECT_WEIGHT <- as.character(raw_19$SUSPECT_WEIGHT)
# change "STOP_ID_ANONY" in 2019 to "STOP_ID"
names(raw_19)[names(raw_19) == "STOP_ID_ANONY"] <- "STOP_FRISK_ID"
# remove "ASK_FOR_CONSENT_FLG" and "CONSENT_GIVEN_FLG" from 2019 datasets since they are missing from 2017 and 2018 datasets
raw_19 <- raw_19 %>% dplyr::select(-c("ASK_FOR_CONSENT_FLG", "CONSENT_GIVEN_FLG"))
# remove "SUPERVISING_ACTION_CORRESPONDING_ACTIVITY_LOG_ENTRY_REVIEWED" and "STOP_LOCATION_PREMISES_NAME" from 2017 and 2018 datsets since they are missing from 2019 datset
raw_17 <- raw_17 %>% dplyr::select(-c("SUPERVISING_ACTION_CORRESPONDING_ACTIVITY_LOG_ENTRY_REVIEWED",
                                      "STOP_LOCATION_PREMISES_NAME"))
raw_18 <- raw_18 %>% dplyr::select(-c("SUPERVISING_ACTION_CORRESPONDING_ACTIVITY_LOG_ENTRY_REVIEWED",
                                      "STOP_LOCATION_PREMISES_NAME"))

#add year variable 
raw_19$year <- 2019
raw_18$year <- 2018
raw_17$year <- 2017

# merge datasets
sqf <- bind_rows(raw_17, raw_18)
sqf <- bind_rows(sqf, raw_19)

sqf_sub <- sqf %>% dplyr::select(-c("STOP_FRISK_ID", "STOP_FRISK_DATE", "MONTH2", "DAY2",
                                    "RECORD_STATUS_CODE", "ISSUING_OFFICER_COMMAND_CODE",
                                    "SUPERVISING_OFFICER_COMMAND_CODE", 
                                    "LOCATION_IN_OUT_CODE", "JURISDICTION_CODE", 
                                    "JURISDICTION_DESCRIPTION", "OFFICER_NOT_EXPLAINED_STOP_DESCRIPTION",
                                    "DEMEANOR_CODE", "SUSPECT_BODY_BUILD_TYPE", "SUSPECT_EYE_COLOR",
                                    "SUSPECT_HAIR_COLOR", "SUSPECT_OTHER_DESCRIPTION", "STOP_LOCATION_SECTOR_CODE",
                                    "STOP_LOCATION_SECTOR_CODE", "STOP_LOCATION_APARTMENT",
                                    "STOP_LOCATION_FULL_ADDRESS", "STOP_LOCATION_STREET_NAME",
                                    "STOP_LOCATION_X", "STOP_LOCATION_Y", "STOP_LOCATION_ZIP_CODE",
                                    "STOP_LOCATION_PATROL_BORO_NAME", "STOP_LOCATION_BORO_NAME"
))

# replace NULL values in stop_was_initiated, age, race, height, weight, and precinct with NAs
sqf_mod <- sqf_sub
sqf_mod$SUSPECT_REPORTED_AGE[sqf_mod$SUSPECT_REPORTED_AGE == "(null)"] <- NA
sqf_mod$SUSPECT_RACE_DESCRIPTION[sqf_mod$SUSPECT_RACE_DESCRIPTION == "(null)"] <- NA
sqf_mod$SUSPECT_HEIGHT[sqf_mod$SUSPECT_HEIGHT == "#NULL!" | sqf_mod$SUSPECT_HEIGHT == "(null)"] <- NA
sqf_mod$SUSPECT_WEIGHT[sqf_mod$SUSPECT_WEIGHT == "(null)"] <- NA
sqf_mod$STOP_LOCATION_PRECINCT[sqf_mod$STOP_LOCATION_PRECINCT == "(null)"] <- NA
sqf_mod$STOP_WAS_INITIATED[sqf_mod$STOP_WAS_INITIATED == "(null)"] <- NA
sqf_mod$SUSPECT_SEX[sqf_mod$SUSPECT_SEX == "(null)"] <- NA
sqf_mod$WEAPON_FOUND_FLAG[sqf_mod$WEAPON_FOUND_FLAG == "("] <- NA # replace mis-entered values with NA
# replace (null) values elsewhere with N
sqf_mod[sqf_mod == "(null)"] <- "N"
# convert variables to appropriate data types
sqf_mod$SUSPECT_REPORTED_AGE <- as.double(sqf_mod$SUSPECT_REPORTED_AGE)
sqf_mod$SUSPECT_HEIGHT <- as.double(sqf_mod$SUSPECT_HEIGHT)
sqf_mod$SUSPECT_WEIGHT <- as.double(sqf_mod$SUSPECT_WEIGHT)

sqf_final <- sqf_mod
# Convert height to inches
sqf_final$SUSPECT_HEIGHT <- as.character(sqf_final$SUSPECT_HEIGHT)
sqf_final <- sqf_final %>% separate(col = SUSPECT_HEIGHT, into = c("FEET", "INCHES"),
                                    remove = TRUE)
sqf_final <- sqf_final %>% mutate(SUSPECT_HEIGHT = 12* as.double(FEET) + if_else(is.na(INCHES), 0,
                                                                                 as.double(INCHES))) %>% 
  dplyr::select(-c("FEET","INCHES"))
# change type of race variable to factor so that it can be releveled later to set white as the baseline
sqf_final$SUSPECT_RACE_DESCRIPTION <- as.factor(sqf_final$SUSPECT_RACE_DESCRIPTION)
# create variable to differentiate nighttime from daytime stops
sqf_final$STOP_FRISK_TIME <- chron(times = sqf_final$STOP_FRISK_TIME)
sqf_final <- sqf_final %>% mutate(IS_NIGHT = if_else((STOP_FRISK_TIME < "06:00:00") | 
                                                       (STOP_FRISK_TIME > "18:00:00"), "Y", "N"))
# condense race variable, reduce redunancy of categories
sqf_final$SUSPECT_RACE_DESCRIPTION <- sqf_final$SUSPECT_RACE_DESCRIPTION %>% recode(
  `BLACK` = "BLACK", `WHITE` = "WHITE",
  `WHITE HISPANIC` ="HISPANIC", `BLACK HISPANIC` = "HISPANIC",
  `ASIAN / PACIFIC ISLANDER` = "ASIAN/PAC.ISL",
  `AMERICAN INDIAN/ALASKAN NATIVE` = "AMER.IND",
  `AMERICAN IND` = "AMER.IND",
  `AMERICAN INDIAN/ALASKAN N` = "AMER.IND",
  .default = NA_character_)
# create Black and Hispanic binary variables to indicate if the stop was conducted in a minority neighborhood
sqf_final$MAJ_BLK <- if_else(sqf_final$STOP_LOCATION_PRECINCT %in% blk_maj, "Y", "N")
sqf_final$MAJ_HISP <- if_else(sqf_final$STOP_LOCATION_PRECINCT %in% hisp_maj, "Y", "N")
sqf_final$MAJ_WHITE <- if_else(sqf_final$STOP_LOCATION_PRECINCT %in% white_maj, "Y", "N")

# create ordinal response variable for force severity
# create condensed ordinal variable for suspected crimes by severity
# create night and day variable
sqf_final <- sqf_final %>% 
  mutate(FORCE_SEV = factor(if_else((PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG == "Y") |
                                      (PHYSICAL_FORCE_WEAPON_IMPACT_FLAG == "Y"), 4,
                                    if_else((PHYSICAL_FORCE_OC_SPRAY_USED_FLAG == "Y") |
                                              (PHYSICAL_FORCE_CEW_FLAG == "Y") | 
                                              (PHYSICAL_FORCE_RESTRAINT_USED_FLAG == "Y") |
                                              (PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG == "Y"), 3,
                                            if_else(PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG == "Y", 2,
                                                    1))))) %>% 
  mutate(CRIME_SEV = as.factor(if_else(SUSPECTED_CRIME_DESCRIPTION %in% c("TERRORISM", "ROBBERY",
                                                                          "ASSAULT", "MURDER",
                                                                          "RAPE"), 
                                       4, 
                                       if_else(SUSPECTED_CRIME_DESCRIPTION %in% c("GRAND LARCENY",
                                                                                  "BURGLARY",
                                                                                  "CPW", 
                                                                                  "GRAND LARCENY AUTO",
                                                                                  "CRIMINAL TRESPASS",
                                                                                  "CPSP",
                                                                                  "AUTO STRIPPIG",
                                                                                  "FELONY", 
                                                                                  "CRIMINAL SALE OF CONTROLLED SUBSTANCE",
                                                                                  "CRIMINAL SALE OF MARIHUANA"
                                       ), 
                                       3,
                                       if_else(SUSPECTED_CRIME_DESCRIPTION %in% c(
                                         "CRIMINAL POSSESSION OF MARIHUANA",
                                         "CRIMINAL POSSESSION OF CONTROLLED SUBSTANCE"
                                       ),
                                       2, 
                                       1))))) %>% 
  mutate(MAXIMUM_FORCE = if_else((PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG == "Y") |
                                   (PHYSICAL_FORCE_WEAPON_IMPACT_FLAG == "Y") |
                                   (PHYSICAL_FORCE_OC_SPRAY_USED_FLAG == "Y") |
                                   (PHYSICAL_FORCE_CEW_FLAG == "Y") |
                                   (PHYSICAL_FORCE_RESTRAINT_USED_FLAG == "Y") |
                                   (PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG == "Y"), "Y",
                                 "N"))

#rename variables to shorten names
sqf_final <- sqf_final %>% rename(
  RACE = SUSPECT_RACE_DESCRIPTION, 
  STOP_REASON = STOP_WAS_INITIATED,
  STOP_EXPLAIN = OFFICER_EXPLAINED_STOP_FLAG,
  WEAPON_FOUND = WEAPON_FOUND_FLAG,
  OTHER_CONTRA = OTHER_CONTRABAND_FLAG,
  HIST_VIO = BACKROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG,
  HIST_CARRY = BACKROUND_CIRCUMSTANCES_SUSPECT_KNOWN_TO_CARRY_WEAPON_FLAG,
  ACT_CARRY = SUSPECTS_ACTIONS_CONCEALED_POSSESSION_WEAPON_FLAG,
  ACT_DRUG = SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG,
  OBS_DUR = OBSERVED_DURATION_MINUTES,
  STOP_DUR = STOP_DURATION_MINUTES,
  AGE = SUSPECT_REPORTED_AGE,
  SEX = SUSPECT_SEX,
  HEIGHT = SUSPECT_HEIGHT,
  WEIGHT = SUSPECT_WEIGHT
)

# get datasets with precinct level data
sqf_precinct <- sqf_final %>% 
  dplyr::select(CRIME_SEV, FORCE_SEV, MAXIMUM_FORCE, MAJ_WHITE, MAJ_HISP, MAJ_BLK, IS_NIGHT,
                STOP_LOCATION_PRECINCT, 
                HIST_VIO, HIST_CARRY, # aggregate violence and carry to assess precinct perceived 'danger'
                WEAPON_FOUND, OTHER_CONTRA, # actual 'danger' measures
                SUSPECT_ARRESTED_FLAG, STOP_EXPLAIN, STOP_DUR, OBS_DUR, RACE, YEAR2) %>% 
  drop_na(STOP_LOCATION_PRECINCT) # remove NA precinct values
# sqf_precinct[!complete.cases(sqf_precinct),]
sqf_precinct <- sqf_precinct %>% 
  na.omit() %>%  # remove 14 NA in IsNight or WeaponFound
  group_by(STOP_LOCATION_PRECINCT, YEAR2) %>% 
  summarise(MaxForce = sum(MAXIMUM_FORCE == "Y")/length(MAXIMUM_FORCE),
            ViolentCrimeFrequency = sum(as.numeric(CRIME_SEV) == 4) / length(CRIME_SEV),
            FelonyFrequency = sum(as.numeric(CRIME_SEV) == 3) / length(CRIME_SEV),
            CrimeSeverity = mean(as.numeric(CRIME_SEV)),
            ForceSeverity = mean(as.numeric(FORCE_SEV)),
            MajorityWhite = MAJ_WHITE[1], # these are already precinct level variables
            MajorityHispanic = MAJ_HISP[1],
            MajorityBlack = MAJ_BLK[1],
            IsNight = sum(IS_NIGHT == "Y")/length(IS_NIGHT),
            HistoricalDanger = sum(if_else(HIST_VIO == "Y" | HIST_CARRY == "Y", 1, 0))/length(HIST_VIO),
            ActualDanger = sum(if_else(WEAPON_FOUND == "Y" | OTHER_CONTRA == "Y"
                                       , 1, 0))/length(WEAPON_FOUND),
            Arrests = sum(SUSPECT_ARRESTED_FLAG == "Y")/length(SUSPECT_ARRESTED_FLAG),
            ExplanationGiven = sum(STOP_EXPLAIN == "Y") / length(STOP_EXPLAIN),
            StopDuration = mean(STOP_DUR),
            ObsDuration = mean(OBS_DUR),
            SQFPropBlack = sum(RACE == "BLACK") / length(RACE),
            SQFPropHispanic = sum(RACE == "HISPANIC") / length(RACE))
sqf_precinct <- rename(sqf_precinct, Precinct = STOP_LOCATION_PRECINCT)
precinct_pct <- rename(precinct_pct, Precinct = precinct_2020)
sqf_precinct <- sqf_precinct %>% merge(precinct_pct, by = "Precinct") %>% 
  #   mutate(BlackDifferential = SQFPropBlack - pct_blk,
  #         HispanicDifferential = SQFPropHispanic - pct_hisp) %>% 
  dplyr::select(-c(total_pop, pct_white))

# write dataset
write.csv(sqf_precinct, "data/sqf_precinct.csv")

# read json and precinct datasets
sqf_precinct <- read.csv("data/sqf_precinct.csv")
nypp_json <- st_read("data/nypp.geojson", quiet = T)
nypp_json <- nypp_json[,-1]
data <- merge(nypp_json, sqf_precinct, by = "Precinct")
data <- data[,-4]


ui <- dashboardPage(
  dashboardHeader(title = "SQF Events"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("accusoft")),
      menuItem("Arrests", tabName = "Arrests", icon = icon("asymmetrik")),
      menuItem("Force", tabName = "Force", icon = icon("mandalorian"))
    )
  ),

  dashboardBody(
    useShinyalert(),
    
    tabItems(
      tabItem(
        tabName = "Home",
        fluidRow(
          tabBox(
            id = "info", height = "300px", width = 12,
            tabPanel(title = "Context",
                     p("In the late 1990s and early 2000s, New York City experienced an increase in crime rates and its administration was pressured into creating policies to try to quell the issue. As a result, former mayor Michael Bloomberg encouraged the practice of Stop, Question, and Frisk (SQF). Under the SQF protocol, New York Police Department (NYPD) officers were encouraged to stop, question, and frisk individuals that may be deemed as suspicious, with the idea that doing so would decrease gun and drug violence. In this analysis, you will be able to view discrepancies in the arrest rates and proportion of SQF events resulting in the use of force by precinct during three different years: 2017, 2018, and 2019.")
            ))
          )
        ),
      tabItem(
        tabName = "Arrests",
        fluidRow(
          box(title = "Inputs", status = "primary", solidHeader = TRUE,
              selectInput(inputId = "year1", label = "Year", 
                          choices = c("2017", "2018", "2019"),
                          selected = "2017"
              )
          ),
          box(title = "Choropleth Map of Arrest Rate", status = "primary", solidHeader = TRUE,
              add_busy_spinner(spin = "trinity-rings"),
              plotOutput(outputId = "arrest")
          )
        )
      ),
      tabItem(
        tabName = "Force",
        fluidRow(
          box(title = "Inputs", status = "primary", solidHeader = TRUE,
              selectInput(inputId = "year2", label = "Year", 
                          choices = c("2017", "2018", "2019"),
                          selected = "2017"
              )
          ),
          box(title = "Choropleth Map of Use of Physical Force Rate", status = "primary", solidHeader = TRUE,
              add_busy_spinner(spin = "trinity-rings"),
              plotOutput(outputId = "force")
          )
        )
      )
      
    )
  )
)

#Define server logic required to plot spatial map
server <- function(input, output) {
  output$arrest <- renderPlot({
    req(input$year1)
    arrest_plotter(data, input$year1)
  })
  
  output$force <- renderPlot({
    req(input$year2)
    force_plotter(data, input$year2)
  })

}

# Run the application
shinyApp(ui = ui, server = server)

