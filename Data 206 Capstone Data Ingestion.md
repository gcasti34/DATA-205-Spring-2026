# Data 206 Capstone Data ingestion
Gabriel Castillo

## Installing libraries

``` r
library(dplyr) 
```

    Warning: package 'dplyr' was built under R version 4.4.3


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(ggplot2) 
```

    Warning: package 'ggplot2' was built under R version 4.4.3

``` r
library(lubridate)
```


    Attaching package: 'lubridate'

    The following objects are masked from 'package:base':

        date, intersect, setdiff, union

## Inviting the two data sets

``` r
Impound <- read.csv("C:/Users/casti/OneDrive/Documents/Data 206 Capstone Project/.quarto/project-cache/Animal Impound.csv") 
Shelter <- read.csv("C:/Users/casti/OneDrive/Documents/Data 206 Capstone Project/.quarto/project-cache/Animal Pathway.csv") #Adding the two datasets 
```

## Joining the data sets and viewing how many animals IDs linked

``` r
colnames(Impound)[colnames(Impound) == "Animal.identification.number"] <- "Animal.ID" 
colnames(Shelter)[colnames(Shelter) == "Animal.identification.number"] <- "Animal.ID" # Changing the name of the column to make it easier 
combined <- left_join(Impound,Shelter, by = "Animal.ID") #The merging of the two datasets 
```

    Warning in left_join(Impound, Shelter, by = "Animal.ID"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ℹ Row 26 of `x` matches multiple rows in `y`.
    ℹ Row 21 of `y` matches multiple rows in `x`.
    ℹ If a many-to-many relationship is expected, set `relationship =
      "many-to-many"` to silence this warning.

``` r
sum(Impound$Animal.ID %in% Shelter$Animal.ID) # To see how many of the ID joined
```

    [1] 5847

``` r
names(Shelter) # To see some of the variables for further exploration 
```

     [1] "Impound.number"           "Kennel.number.location"  
     [3] "Animal.ID"                "Animal.source"           
     [5] "Animal.owner"             "jurisdiction"            
     [7] "Impound.type"             "Intake.subcategory"      
     [9] "Impound.date.and.time"    "Release.date"            
    [11] "Surrender.reason"         "source"                  
    [13] "Total.number.per.impound" "Animal.intake.condition" 
    [15] "Animal.control.hold"      "Outcome.type"            
    [17] "Outcome.subtype"          "Animal.outcome.condition"
    [19] "Departure.date.and.time"  "Animal.name"             
    [21] "Animal.type"              "Sex.of.animal"           
    [23] "Pet.s.age.in.years"       "Pet.s.age.in.months"     
    [25] "Pet.s.date.of.birth"      "Pet.s.age"               
    [27] "Bite.history"             "Animal.size"             
    [29] "Color.of.animal"          "Breed.of.animal"         
    [31] "Collar.present"          

## Changing some column names

``` r
combined <- combined %>%
  rename(
    Age_Year = Pet.s.age.in.years,
    Animal_Type = Animal.type,
    Bite_History = Bite.history,
    Color = Color.of.animal,
    Breed = Breed.of.animal
  )
```

## My first attempt to create datetime variables for future analysis

``` r
combined <- combined %>%
  mutate(
    Departure = mdy_hm(Departure.date.and.time),
    Intake = mdy_hm(Impound.date.and.time), #Creating new variables for datetime to take the difference.
    LengthOfStay = round(as.numeric(difftime(Departure, Intake, units = "days")), 2)
  )
```

## Creating a day hour and minute variable for how long the animals have been in the shelter

``` r
combined <- combined %>%
  mutate(
    StayDays = floor(LengthOfStay),#Looking at the first number of the decimal for the hour 
    

    StayHours = floor((LengthOfStay - floor(LengthOfStay)) * 24), # Removes the front decimal and looks at the second digit to get hours
    
    StayMinutes = round(((LengthOfStay - floor(LengthOfStay)) * 24 - StayHours) * 60) # Looks at the last digit of the decimal and creates it into minutes
  )
```

## Creating a year/month dataframe

``` r
Year_month <- combined %>%
  filter(!is.na(Departure.date.and.time)) # Made a new dataframe for Yearmonth due to complications of doing it to combined dataframe and filter for no NAs

Year_month$Departure.date.and.time[Year_month$Departure.date.and.time == ""] <- NA # Getting rid of negative space as some inputs in the row were " " to the word NA
Year_month <- combined %>%
  filter(!is.na(Departure.date.and.time)) # Had to do it twice as the spaces were now turned to NAs
head(Year_month$Departure.date.and.time) # To check for NAs
```

    [1] "2/27/2026 18:58" "2/27/2026 18:31" "2/26/2026 20:53" "2/26/2026 14:43"
    [5] ""                "2/26/2026 12:01"

``` r
Year_month$Departure.date.and.time <- as.POSIXct(
 Year_month$Departure.date.and.time,
  format = "%m/%d/%Y %H:%M" # Had to do this again different method to get the format right this time
)
Year_month$YearMonth <- format(Year_month$Departure.date.and.time, "%Y-%m")
# Having year/month as a new variable to be part of my graph below to have a compact x axis
```

``` r
monthly_outcomes <- Year_month %>%
  filter(!is.na(Departure.date.and.time)) %>% # making sure there is no NAs 
  group_by(YearMonth, Outcome.type.y) %>% # To create a group for each month for each outcome
  summarise(count = n(), .groups = "drop") #To make each outcome a new row for each month
```

## Creating a second data frame for a possible graph

``` r
Euthanized <- combined %>%
  filter(Outcome.type.y == "EUTH") #Filter for Euthanization

Euthanized <- Euthanized %>%
  filter(!is.na(Age_Year)) #Remove any NAs
Euthanized <- Euthanized %>%
  mutate(
    Age_group = case_when(
     Age_Year < 1 ~ "Under 1", # Create age groups as every number will be hard to graph
      Age_Year >= 1 & Age_Year < 3 ~ "1-3 Age group",
      Age_Year >= 3 & Age_Year < 7 ~ "3-7 Age group",
      Age_Year >= 7& Age_Year < 10 ~ "7-10 Age group",
      Age_Year > 10 ~ "10+ Age group"
    )
  )
Euthanized <- Euthanized %>%
  filter(!is.na(Age_group), !is.na(Animal_Type)) # Make sure any Nas are gone
```

## Creating a new dataframe to get all the animal outcomes in percentage and graph it

``` r
summary_data <- combined %>%
  filter(!is.na(Outcome.type.x)) %>%       # remove missing outcomes
  group_by(Outcome.type.x) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
```

## Creating a dataframe for the different breeds of animals to remove any NAs

``` r
breed_counts <- combined %>%
  filter(!is.na(Breed)) %>%       
  group_by(Breed) %>%
  summarise(count = n()) %>%
  arrange(desc(count))    
```

## Creating the bite dataframe to remove NAs

``` r
Bite_animal <- combined %>%
  filter(!is.na(Bite_History), Bite_History !="") #Removing NAs and blanks
```

## Creating a new summary dataframe to capture the months of the year and their average StayTime

``` r
  Year_month %>%  
  group_by(YearMonth) %>%
  summarise(avg_stay = mean(StayDays, na.rm = TRUE)) 
```

    # A tibble: 15 × 2
       YearMonth avg_stay
       <chr>        <dbl>
     1 2025-01       5.22
     2 2025-02       9.54
     3 2025-03       9.24
     4 2025-04      12.3 
     5 2025-05      15.6 
     6 2025-06      16.5 
     7 2025-07      17.2 
     8 2025-08      22.6 
     9 2025-09      17.7 
    10 2025-10      17.1 
    11 2025-11      18.1 
    12 2025-12      26.5 
    13 2026-01      25.7 
    14 2026-02      18.1 
    15 <NA>        NaN   
