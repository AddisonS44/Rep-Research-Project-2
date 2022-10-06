---
title: "Reproducible Research: NOAA Data Analysis"
output: 
  html_document:
    keep_md: true
---



# Data Processing

We will begin, simply, by loading the data from the provided link into a local file.

```r
if(!file.exists("Data")) {dir.create("Data")}

if(!file.exists("Data/StormData.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "Data/StormData.csv.bz2")
  bunzip2("Data/StormData.csv.bz2", remove = FALSE)
  }
StormData <- read.csv("Data/StormData.csv")
```

## Filtering
The first order of business on this data is the fact that not all of the data was 
actually recorded every year. In fact, even though the data starts with 1950, it 
isn't until January 1996 that all of the potential event types are present.


```r
StormData <- StormData[as.Date(StormData$BGN_DATE, format =  "%m/%d/%Y %H:%M:%S") > "1996-01-01",]
etypes <- unique(StormData$EVTYPE)
length(etypes)
```

```
## [1] 515
```

Okay, but what comes after that? Well the biggest piece of concern is that the 
data doesn't currently reflect the individual data points. There are approximately
48 of them, but as shown above, there are considerably more event types listed. 
It's not quite reasonable to list them all out here, but here is a small selection 
of the EVTYPE values containing "FLASH" like in "flash flood":


```r
unique(StormData$EVTYPE[grepl("FLASH", StormData$EVTYPE)])
```

```
## [1] "FLASH FLOOD"       "FLASH FLOOD/FLOOD" "FLASH FLOODING"   
## [4] "FLOOD/FLASH/FLOOD" " FLASH FLOOD"
```

So what to do? We're going to need some way to combine like categories and some 
concessions will need to be made. Checking through the unique EVTYPE values you 
will see that at this point there are 515 unique values for EVTYPE.

## Sorting Event Types
First, what can we excluded? Well combing through this list reveals a number of 
"Summary" values. Beyond this, it's not particularly easy to throw out any specific 
event types, unless they specifically do not wind up matching any of the specified 
48 EVTYPE values given in the documentation. Let's start by just stripping out 
these summary values to reduce the total number of values to 448. I'm planning
to throw all the extra EVTYPE values into an "other" category, so this becomes
important later.

```r
summaries_list <- tolower(etypes[grep("summary", etypes, ignore.case = TRUE)])
etypes <- etypes[!(tolower(etypes) %in% summaries_list)]
```

We will have to look at actually reducing these evtypes in the dataframe momentarily, 
but for now this is acceptable. We can begin taking a look at the list of remaining 
event types now to find what can be filtered out. From here it would be frankly
too much text to explain every little decision made for what is included anywhere.
The code I intend to use to make these lists is going to be shown below, and at 
the end each unique event type is going to have a character vector in its name 
that includes exactly the list of listed ETYPE values from the original dataframe 
that will be placed under its label.

In general, I am employing these basic guidelines to select what fits what event
type:

1. Types are opt-in. If an EVTYPE value remains afterwards, it must not have fit
any listed event type.
2. Each list should be non-intersecting to avoid duplicating data. However, if 
there is a value that equally fits two or more event types, this can and should 
be reconsidered.
3. I will endeavor to save time using broad strokes to select EVTYPE values. For
example, all values containing "tornado" should likely go in the Tornado value. 
Before doing this, of course, all such values should be checked in case something
like "ice tornado" is present (god, I hope not...).
4. After that, some values will need to be specifically sorted. Some such examples 
are the values containing "FLD" or "FLDG" which I would have absolutely missed 
with just a cursory glance.

```r
## I'm going to start by creating a simple search function to save myself the 
## effort of typing out the grep() call that I'm using for this
search_list <- function(list, s_value) {
  trimmed_list <- list[grep(s_value, list, ignore.case = TRUE)]
  return(trimmed_list)
}
## These are the types I found the easiest to filter:
avalanche <- search_list(etypes, "avalanche") ## One down, 47 to go...
drought <- search_list(etypes, "drought")
hurricane_typhoon <- c(search_list(etypes, "typh"), search_list(etypes, "hurr"))
marine_hail <- search_list(etypes, "marine hail")
marine_high_wind <- "MARINE HIGH WIND"
marine_strong_wind <- "MARINE STRONG WIND"
marine_thunderstorm_wind <- c("MARINE THUNDERSTORM WIND", "MARINE TSTM WIND")
seiche <- search_list(etypes, "seiche")
tornado <- c(search_list(etypes, "tornado"), "LANDSPOUT")
volcanic_ash <- search_list(etypes, "volcanic")
waterspout <- search_list(etypes, "waterspout")

## To make sure I don't accidentally pull in anything I decided was for the prior
## categories, I'm going to trim down the etypes list here
etypes <- etypes[!(etypes %in% c(avalanche, drought, hurricane_typhoon, marine_hail, marine_high_wind, marine_strong_wind, marine_thunderstorm_wind, seiche, tornado, volcanic_ash, waterspout))]
## This basically just says "all of etypes that isn't in any of the other lists"
```

So here's where we start to see the issues with point 2. When looking at the 
thunderstorm wind listings, we see things like "TSTM WIND AND LIGHTNING" and 
"TSTM WIND/HAIL" which are also going to fall under Lightning and Hail, obviously.
Basically, I'm going to start taking these listings, removing them from the etypes
list and readding any of these specific exceptions back to the list.

```r
dense_fog <- search_list(etypes, "og")     ## Would you believe I caught "VOG" without having to go back for it?
dense_smoke <- search_list(etypes, "smoke")
dust_devil <- search_list(etypes, "dust de")
dust_storm <- search_list(etypes, "dust")
dust_storm <- dust_storm[!(dust_storm %in% dust_devil)]
freezing_fog <- c(search_list(etypes, "freezing fog"), "Ice Fog")
dense_fog <- dense_fog[!(dense_fog %in% freezing_fog)]
funnel_cloud <- search_list(etypes, "funnel")
hail <- search_list(etypes, "hail")
lightning <- search_list(etypes, "lightning")
rip_current <- search_list(etypes, "rip")
thunderstorm_wind <- c(search_list(etypes, "thunderstorm wind"), search_list(etypes, "tstm wind"))
thunderstorm_wind <- thunderstorm_wind[!(thunderstorm_wind %in% search_list(thunderstorm_wind, "non"))]
wildfire <- search_list(etypes, "fire")
addback <- c("GUSTY WIND/HAIL", "HAIL/WIND")

## Just trying to keep this clean
etypes <- etypes[!(etypes %in% c(dense_smoke, dust_devil, dust_storm, dense_fog, freezing_fog, funnel_cloud, hail, lightning, rip_current, thunderstorm_wind, wildfire))]
etypes <- c(etypes, addback)

astronomical_low_tide <- c("ASTRONOMICAL LOW TIDE")
coastal_flood <- search_list(c(search_list(etypes, "cstl"), search_list(etypes, "coastal")), "flood")
lake_effect_snow <- search_list(etypes, "effect snow")
lakeshore_flood <- search_list(etypes, "lakeshore")
storm_surge_tide <- c(search_list(etypes, "surge"), search_list(etypes, "tide"))
storm_surge_tide <- storm_surge_tide[!(storm_surge_tide %in% astronomical_low_tide)]
tropical_depression <- search_list(etypes, "depre")
tropical_storm <- search_list(etypes, "tropical st")
tsunami <- search_list(etypes, "tsunami")

etypes <- etypes[!(etypes %in% c(astronomical_low_tide, coastal_flood, lake_effect_snow, lakeshore_flood, storm_surge_tide, tropical_depression, tropical_storm, tsunami))]

## The choice of separation between "heat" and "excessive heat" is a bit arbitrary,
## But I'm recording special heat events as "excessive" and heat waves as just "heat"
excessive_heat <- c("EXCESSIVE HEAT", "Heatburst", "Record Heat", "RECORD HEAT")
heat <- search_list(etypes, "heat")
heat <- heat[!(heat %in% excessive_heat)]

## All the cold stuff goes here. I'm gonna just match in bulk and let the later 
## steps sort out the duplicates. I do have a plan for that.
blizzard <- search_list(etypes, "blizz")
icestorm <- search_list(search_list(etypes, "ice"), "storm")
winter_storm <- c("WINTER STORM", "Record Winter Snow")
frost_freeze <- unique(c(search_list(etypes, "freeze"), search_list(etypes, "frost")))
cold_wind_chill <- c("Cold", "Unseasonable Cold", "COLD", "Cold Temperature", 
          "COLD AND SNOW", "UNSEASONABLY COLD", "Prolong Cold", "Cold and Frost", 
          "PROLONG COLD", "COLD AND FROST", "COLD TEMPERATURES", "COLD WIND CHILL TEMPERATURES", 
          "UNUSUALLY COLD", "COLD WEATHER", "COLD/WIND CHILL", "WIND CHILL",
          "BITTER WIND CHILL", "BITTER WIND CHILL TEMPERATURES")
extreme_cold_wind_chill <- c(search_list(etypes, "cold"), search_list(etypes, "chill"))
extreme_cold_wind_chill <- extreme_cold_wind_chill[!(extreme_cold_wind_chill %in% cold_wind_chill)]
sleet <- search_list(etypes, "sleet")
heavy_snow <- c("HEAVY SNOW", "Snow Squalls", "Record Winter Snow", "Heavy snow shower", 
                "Record May Snow", "Thundersnow shower", "SNOW SQUALL", 
                "HEAVY SNOW SQUALLS", "SNOW SQUALLS", "RECORD SNOWFALL", 
                "EXCESSIVE SNOW", "RECORD SNOW")
## And after some debate with my rubber duck, everything left over is going to 
## be tossed into the "winter weather" section. So literally everyting with
## "cold", "snow", "freez[e/ing]", etc goes in here. Then just filter out like
## I've been doing with "evtype"
winter_weather <- c(search_list(etypes, "cold"), search_list(etypes, "snow"), 
                    search_list(etypes, "freez"), search_list(etypes, "ice"), 
                    search_list(etypes, "icy"), search_list(etypes, "winter"))
winter_weather <- winter_weather[!(winter_weather %in% c(blizzard, icestorm, 
                    winter_storm, frost_freeze, cold_wind_chill, 
                    extreme_cold_wind_chill, sleet, heavy_snow))]
etypes <- etypes[!(etypes %in% c(blizzard, icestorm, winter_storm, frost_freeze, 
                    cold_wind_chill, extreme_cold_wind_chill, winter_weather))]
```

That was a lot of code, so let's take a moment to dive into exactly what I did:

There were really 3 sections: some easy filters, heat, and cold. The easy filters
are just like the ones I'd done before. Essentially, I find a specific term or two 
that filters out only exactly the values I think are important, or something that 
gets very close and add in the specific values missing.

The heat events section is a good example of what I'll be doing a few times going
forward. What actually qualifies as "heat" or "excessive heat" is up to speculation. 
As stated in the comments in the code, the "heat" values I chose were things that 
are somewhat typical. A heat wave isn't normal, but it's really just hot. If it 
gets into a situation like the heat wave that struck England in the summer of 
2022, then it should be listed under excessive heat if this data is reliable. It 
technically does qualify for both, but this is a concession that needs to be made 
because I simply cannot filter through every major weather event of the last several 
decades to find what actually deserves to be listed where (not that I'd necessarily 
be any more accurate anyways). 

Finally, cold events were the trickiest. This was a lot of manual listing of the 
events, because you can't just say one exact term to summarize what a storm is 
or what heavy snow really falls to. Another person would absolutely make different 
decisions here. That's okay! Data analysis is unfortunately not an exact science 
the data is cleaned (and even afterwards, don't get me started on statistical 
significance). The decisions I've made are somewhat arbitrary, but they will allow 
for a general understanding of what is and isn't really impactful within the 48 
given categories.
