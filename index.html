---
title: "Japanese earthqyakes dataset"
author: "Stavrou Athanasios (stavrouath@outlook.com)"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    keep_md: true
    number_sections: true
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Introduction

The aim of this presentation is to show some ``ggplot`` graphs. Boxplots, violin plots and timelapses are some of them. To do so, we will analyze a dataset of Japanese earthquakes, from 2001 to 2018. The dataset was acquired by [this source](https://bit.ly/2FkdR2Z).

# Data curation

First, we read the dataset and curate it so that it takes the form that we want:

```{r read}
data <- read.csv("C:/Users/whoka/Documents/R/earthquakes/earthquakes.csv", header = T, stringsAsFactors = F);
head(data)
```

The dataset consists of 22 variables, it contains all earthquakes from 2001 to 2018 with magnitude of at least 4.5 in the Richter scale. The dataset is in a different form the one we want, so we curate it. To start with, we need to break the ``time`` variable into new, separate variables for the year, month, day and time of each earthquake. We save them as integers and then match them with the respective month names. Finally, we reorder some variables so that it is in a convenient for us form, and then sort them chronologically.

```{r Curate}
#create new variables with year, month and day
data$year <- as.integer(substr(data$time, 0, 4))  #saving as int to make some calculations later on
data$month <- as.integer(substr(data$time, 6, 7)) #saving as int to use month.name function below
data$day <- substr(data$time, 9, 10)
data$time <- substr(data$time, 12, 16)
data$Month <- month.name[data$month]
data$date <- as.Date(with(data, paste(year, month, day,sep="-")), "%Y-%m-%d") #variable containing all dates
data$Month <- factor(data$Month, levels = c("January", "February", "March", "April", "May", 
                                    "June","July", "August", "September", "October", "November", "December"))
#reorder the columns and drop "updated" and "id" since I won't be needing them for my EDA
data <- data[,c(23:27,1:22)]
data <- data[order(data$date),] #sorting by date
head(data)
```

We now drop the variables that we will not be using. This step is not necessary, but we will keep the dataset as clean / easy to read as possible. However, we will give a short explanation of every variable dropped, so that future users understand the dataset.
 
The variables that we keep are pretty straight-forward; year, month, month name, day, time, longitude and latitude, depth and manitude of the earthquakes. The variables that we drop are:

| Variable | Description |
| :--- | :--- |
| magType | The method or algorithm used to calculate the preferred magnitude for the event. |
| nst | Number of seismic stations used to locate earthquake location . |
| gap | The largest azimuthal gap between azimuthally adjacent stations (in degrees). In general, the smaller this number, the more reliable is the calculated horizontal position of the earthquake. Earthquake locations in which the azimuthal gap exceeds 180 degrees typically have large location and depth uncertainties. |
| dmin | Horizontal distance from the epicenter to the nearest station (in degrees). 1 degree is approximately 111.2 kilometers. In general, the smaller this number, the more reliable is the calculated depth of the earthquake. |
| rms | The root-mean-square (RMS) travel time residual, in sec, using all weights. This parameter provides a measure of the fit of the observed arrival times to the predicted arrival times for this location. Smaller numbers reflect a better fit of the data. The value is dependent on the accuracy of the velocity model used to compute the earthquake location, the quality weights assigned to the arrival time data, and the procedure used to locate the earthquake. |
| net | The ID of a data contributor. Identifies the network considered to be the preferred source of information for this event. |
| id | A unique identifier for the event. This is the current preferred id for the event, and may change over time. See the "ids" GeoJSON format property. |
| update | Time when the event was most recently updated. Times are reported in milliseconds since the epoch. In certain output formats, the date is formatted for readability. |
| place | Textual description of named geographic region near to the event. This may be a city name, or a Flinn-Engdahl Region name. |
| type | Type of seismic event. Typical values: “earthquake”, “quarry” |
| horizontalError | The horizontal location error, in km, defined as the length of the largest projection of the three principal errors on a horizontal plane. The principal errors are the major axes of the error ellipsoid, and are mutually perpendicular. The horizontal and vertical uncertainties in an event's location varies from about 100 m horizontally and 300 meters vertically for the best located events, those in the middle of densely spaced seismograph networks, to 10s of kilometers for global events in many parts of the world. We report an "unknown" value if the contributing seismic network does not supply uncertainty estimates. |
| depthError | The depth error, in km, defined as the largest projection of the three principal errors on a vertical line. |
| magError | Uncertainty of reported magnitude of the event. The estimated standard error of the magnitude. The uncertainty corresponds to the specific magnitude type being reported and does not take into account magnitude variations and biases between different magnitude scales. We report an "unknown" value if the contributing seismic network does not supply uncertainty estimates. |
| magNst | The total number of seismic stations used to calculate the magnitude for this earthquake. |
| status | Status is either automatic or reviewed. Automatic events are directly posted by automatic processing systems and have not been verified or altered by a human. Reviewed events have been looked at by a human. The level of review can range from a quick validity check to a careful reanalysis of the event.|
| locationSource | The network that originally authored the reported location of this event. |
| magSource | Network that originally authored the reported magnitude for this event. Typical values: ak, at, ci, hv, ld, mb, nc, nm, nn, pr, pt, se, us, uu, uw |

In no case does the fact that we drop these variables mean that they are insignificant. Instead, we are only interested in analyzing the depth, magnitude and time of the earthquakes. 

```{r drop}
data <- data[, -c(11:27)]
head(data)
```

# Explanatory Data Analysis
## Boxplots

Now that our dataset is clean, we can move forward. Some interesting graphs would be the distributions of the earthquakes per month or year. To do so, we use the ``ggplot2`` library and we plot them together with the ``gridExtra`` library:

```{r magplots, fig.width=12, fig.height=9, warning = FALSE}
library(ggplot2)

magplot1 <- ggplot(data, aes(x=Month, y=mag, group = Month)) + 
                   geom_boxplot(color="red", fill="orange", alpha=0.2) +
                   labs(title = "Earthquake magnitude distribution per month", x = "Month", y = "Magnitude");

magplot2 <- ggplot(data, aes(x=year, y=mag, group = year)) + 
                   geom_boxplot(color="blue", fill="cyan", alpha=0.2) +
                   labs(title = "Earthquake magnitude distribution per year", x = "Year", y = "Magnitude");

library(gridExtra)  #arrange graphs
magplots <- grid.arrange(magplot1, magplot2, nrow=2)
```

The boxplots are quite interesting when we want to compare distributions. We can see the median, the standard deviations and the outliers. We notice that there are no major ditribution differences among months or years.

## Violin Plots
A graph that shows the distribution better than botplots is a violin box. We can see the actual distribution of the observations, on top of every descriptive statistic mentioned above.

```{r depplots, fig.width=12, fig.height=9, warning = FALSE}
depplot1 <- ggplot(data, aes(x=Month, y=depth, group = Month)) + 
                   geom_violin(color="red", fill="orange", alpha=0.2) +
                   labs(title = "Earthquake depth distribution per month", x = "Month", y = "Depth (km)");

depplot2 <- ggplot(data, aes(x=year, y=depth, group = year)) + 
                   geom_violin(color="blue", fill="cyan", alpha=0.2) +
                   labs(title = "Earthquake depth distribution per year", x = "Year", y = "Depth (km)");

depplots <- grid.arrange(depplot1, depplot2, nrow=2)
```

Now that the distributions are easier to see, we notice some differences that weren't visible to the naked eye before, like the fact that in November, the median depth of the eartquakes is significantly lower (as a value, meaning that the event happened closer to the Earth's surface) than the median depth of earthquakes in February. 

Comparing years, we notice differences in some distributions, like these of 2003 and 2006. We do not know if there is a pattern or a model that can explain these differences, but this surely can not be done with this dataset. Thus, we can only observe the distributions and not draw conclusions.

Moreover, with ``ggplot2``, it is possible to combine the two graphs together:

```{r complots, fig.width=12, fig.height=9, warning = FALSE}
complot <- ggplot(data, aes(x=Month, y=depth, group = Month)) + 
                  geom_violin(color="darkred", fill="#A4A4A4", alpha=0.2) +
                  geom_boxplot(width=0.1) +
                  labs(title = "Earthquake depth distribution per month", x = "Month", y = "Depth (km)");

complot
```

This combined graph is better than the previous two, as a user is able to get as much information out of a set of distributions as possible. 

# Correlation tests

In this section we will show you how to test for correlations. Always start with scatter plots to check for obvious correlations. Say we want to check if the magnitudes and depths of earthquakes correlate, because we have a theory in mind, say that the closer an earthquake to the earth's surface the greater the magnitude:

```{r corplot}
ggplot(data, aes(x=mag, y=depth)) + geom_point() +
       geom_smooth() +
       labs(title = "Magnitude vs depth", x = "Mangitude", y = "Depth (km)")
```

We can not draw any conclusions from the plot above, as there does not seem to be any correlation between the two variables. We put the regression line as well, even though there is no point for this dataset, so that it is reproducible for any data. We can run some tests for correlations using built-in R functions:

```{r tests}
cor(data$mag, data$depth, method = "pearson")
cor.test(data$mag, data$depth, method = "pearson")
```

Both commands give us Pearson's correlation coefficient, which is 

\begin{align*}
r = \frac{\sum_{n=1}^{n} (mag_i - \overline{μ}_{mag})(depth_i - \overline{μ}_{depth})}{\sqrt{\sum_{n=1}^{n} (mag_i - \overline{μ}_{mag})^2}\sqrt{\sum_{n=1}^{n} (depth_i - \overline{μ}_{depth})^2}}
\end{align*}

where $\overline{μ}_{mag}$ and $\overline{μ}_{depth}$ are the respective magnitudes' and depths' means. We are also given the $P_{value}$, degrees of freedom for the t-test, as as the t-test value, where

\begin{align*}
t = \frac{r}{{1-r}^2}\sqrt{n-2} \sim t_{14090}
\end{align*}

It is clear that there is no correlation between these variables. Keep in mind that there are other method's one can use, like Spearman's or Kendall's correlation formulas, which must be stated in `method` option.

# Data Viz
## Static Map

In this section we will visualize the dataset on a static, interactive map.

```{r smap, warning = FALSE, message = FALSE}
library(leaflet)  #interactive maps
mapdata <- data[data$mag >= 6,] #keep only observations from 2015 and on

# Create a color palette with handmade bins
mybins <- seq(6, 9.5, by=0.5)
mypalette <- colorBin(palette="YlOrBr", domain=mapdata$mag, na.color="transparent", bins=mybins)

# Prepare the text for the tooltip:
mytext <- paste(
  "Depth: ", mapdata$depth, "<br/>", 
  "Magnitude: ", mapdata$mag, sep="") %>%
  lapply(htmltools::HTML)

# Final Map
m <- leaflet(mapdata) %>% 
  addTiles()  %>% 
  setView( lat=38, lng=142 , zoom=4) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~longitude, ~latitude, 
                   fillColor = ~mypalette(mag), fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                   label = mytext,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addLegend( pal=mypalette, values=~mag, opacity=0.9, title = "Magnitude", position = "bottomright" )

m 
```

This is a static visualization of our dataset, but we only kept the earthquakes with magnitude larger than 6, since static maps are not a great visualization for large datasets (we have 14092 earthquake events from 2001 to 2018). We already see that it is hard to separate the events, but things becoome clearer as we zoom in. A batter visualization for a large number of observations is a "timelapse".

## Timelapse

A final graph that we will cover in this presentation is a bubblemap, but we will animate it from the first earthquake recorded in our dataset to the last. Note that in our dataset there are only dates when earthquakes happened, meaning that if for example there was an event in 10/4/2005, then it is recorded as an observation. However, if no event happened in this date, it will be missing from the data. This means that if a user wants to make a timelapse graph that is smoother than the one we graph below, she has to add new rows in the dataset with the dates that no event happened. For example:

```{r eg}
#Create a date sequence variable

#animdata <- data %>%
#  mutate(date = as.Date(date)) %>%
#  complete(date = seq.Date(min(date), max(date), by="day"))

#animdata$year <- as.integer(substr(animdata$date, 0, 4))  
#animdata$month <- as.integer(substr(animdata$date, 6, 7)) 
#animdata$day <- substr(animdata$date, 9, 10)
#animdata$Month <- month.name[animdata$month]
```

The dataset would now have all dates, with NA values for the newly added observations. Depending on the needs of the user, it could be a good practice to set the magnitude to 0, so that they do not appear in the timelapse, while the dates keeps movling linearly. For this analysis, we leave the dataset as it is, so that we may find a pattern of the events. We create a map of Japan, and then we create a "bubble map graph" of the events:

```{r timelapse, message = FALSE, warning = FALSE}
library(mapdata)  #library to get japanese map
library(viridis)  #color paletter
library(gganimate)  #map animation
library(hrbrthemes) #themes
library(dplyr)

df <- map_data("japan")

timelapse <- ggplot() +
             geom_polygon(data = df, aes(x=long, y=lat, group=group)) + 
             geom_point(data = data, aes(x=longitude, y=latitude, size=mag, color=mag), alpha=0.5) +
             scale_color_viridis() +
             theme_ipsum() +
             ylab("Latitude") +
             xlab("Longitude") +
             #animate the graph using gganimate
             labs(title = "Earthquakes in Japan. Date: {frame_time}") +
             transition_time(date) +
             enter_fade() +
             exit_fade() +
             ease_aes("linear")

timelapse
```

We see that there is a pattern of the events. Most earthquakes happen across the island of Japan, as well as in a line vertical to it. To keep track and visualize all of the events, we add ``shadow_mark()`` option, which yields the following graph:

```{r tl_shadow, message = FALSE, warning = FALSE}
tl_shadow <- ggplot() +
             geom_polygon(data = df, aes(x=long, y=lat, group=group)) + 
             geom_point(data = data, aes(x=longitude, y=latitude, size=mag, color=mag), alpha=0.5) +
             scale_color_viridis() +
             theme_ipsum() +
             ylab("Latitude") +
             xlab("Longitude") +
             #animate the graph using gganimate
             labs(title = "Earthquakes in Japan. Date: {frame_time}") +
             transition_time(date) +
             shadow_mark() + 
             enter_fade() +
             exit_fade() +
             ease_aes("linear")

tl_shadow
```

The before-mentioned pattern is now very clear. After looking at it, we seached for the tectonic plates in Japan and this is the result:

```{r img, warning = FALSE, message = FALSE, fig.align = 'center'}
library(imager) #load and plot image
img <- load.image("C:/Users/whoka/Documents/R/earthquakes/nippon.png")
tp <- plot(img, axes = F, main = "Figure: Tectonic Plates Around Japan")
```

This image shows the tectonic plates around Japan, where 4 of them meet just underneath the island. This explains the previous pattern. In a future project, a good idea could be to cluster the earthquakes and check differences of magnitudes and depths caused from the different tectonic plates.
