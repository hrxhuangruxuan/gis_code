---
title: "Name: Andy MacLachlan"
author: 
- |
    | Student number: TEST
date: "`r format(Sys.time(), '%X, %d %B, %Y')`"
output: html_document
---
# Originality declaration  

I, [**insert your name**], confirm that the work presented in this assessment is my own. Where information has been derived from other sources, I confirm that this has been indicated in the work.

date: `r format(Sys.time(), '%d %B, %Y')`

# Start your response here

## Initial project scope

WRITE A PROJECT SCOPE (we will cover this in the next few weeks in more detail)

* What is my research question - is it different to the set question and why

* Is it appropriate to use 2020 as a year? 

>  This research will identify spatial patterns that can be used to inform future work on spatial factors on New York evicitions

> My question is "are the evicitions in 2020 for New York spatially random or do they exhibit clustering"

> A question for Spatial autocorrelation..."are the densitites of evicitions in New York similar over community districts for 2020" 

**Note** i have decided to use the whole of the city, if i were to plot all of the data (as in all years) i might see further trends - e.g. specific streets or even buildings. If you want to selected a specific area for a more detailed analysis that is perfectly acceptable. For example, the Bronx which is considered the most deprived area of the city (although we might need more spatial data to extract the area)

* we could combine a few of these questions...

> A question for spatial regression..."What are the factors that might lead to variation in evictions across New York in 2020?"

The null hypothesis that I am going to test empirically is that there is no relationship 
  * with points 
  * densities of community districts 
  * with other factors that across New York

* Data
  * What do i have
  * What does it contain
  * What are the NA values - do they matter
  * Who collected the data - will they have any bias (e.g. remember gerrymandering / using data for a )
  * Is there any accuracy information associated with the data - probably not
  * What is the CRS - is that useful
  * Do i need anything else or what might be useful 
  
* How will i wrangle the data (based on the previous points) to apply the methods

* What are the limitations and assumptions (of either the data or the analysis)

This is an essential part of your submission and worth 20%. In the past students have just written a line and failed this criterion (you don't have to pass each criterion).

```{r}
library(tidyverse)
library(sf)
library(tmap)
library(janitor)
library(spatstat)
```

## Data loading

Read in data - note the NA value. 

```{r}
evictions_points <- read_csv("Data/Evictions.csv", na=c(" "))

community_areas <- st_read("Data/Community Districts/geo_export_95f4fa4a-5cb3-4a12-a032-2bba2be99838.shp")
```
Check class - added na argument in code above.

EXPLAIN what i am doing here - checking the variable type to make sure there are no character columns that should be numeric due to NAs

```{r}
Datatypelist <- evictions_points %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist
```
## Data wrangling

Check the coordinates on this website for the csv - https://www.latlong.net/. Looks like the are in WGS84. Convert csv to sf object the map

Missing values for coordiates thrown an error so i need to filter them out...

```{r}
points <- evictions_points%>%
  #also possible to use something like drop_na(Longitdue, Latitude) 
  filter(Longitude<0 & Latitude>0)%>%

  st_as_sf(., coords = c("Longitude", "Latitude"), 
                   crs = 4326)


```

64214 features now from 71,040 in the original dataset.

Make a map

EXPLAIN why i might want to make a map here

```{r}
tmap_mode("plot")
tm_shape(community_areas) +
  tm_polygons(col = NA, alpha = 0.5) +
tm_shape(points) +
  tm_dots(col = "blue")
```

A lot of points!

EXPLAIN...Check the are all within the boundaries...through a spatial subset...

Error of st_crs(x) == st_crs(y)  - means the CRSs of the data doesn't match. I think the error is just how they are set in the data, but i will transform the community areas 

```{r}
community_areas <- community_areas%>%
  st_transform(., 4326)

points_sub <- points[community_areas,]

```

Still have 64,214! So all were intersecting the boundary....(smith, 2020)

Ok, so now how about a question? it wants to focus on 2020 and just says eviction not possession...let's reduce our data to that..i could change the question if i wanted to, perhaps there are more possession points or of course i could even compare the clusters of eviction and possession. 

EXPLAIN...I have used string detect here to find the rows that have 2020 within the column executed_date - what might be the issues with this, why have i made this assumption...There is a package called lubridate that allows easier manipulation of dates, however we haven't covered it in this module and we can use str_detect instead.

```{r}
points_sub_2020<-points_sub%>%
  clean_names()%>%
  filter(str_detect(executed_date, "2020"))%>%
 # filter(eviction_legal_possession=="Eviction")%>%
  filter(residential_commercial=="Residential")
```

This has reduced it to 600 points, if i remove the legal possession/eviction line then it's around 2,000

```{r}
tmap_mode("plot")
tm_shape(community_areas) +
  tm_polygons(col = NA, alpha = 0.5) +
tm_shape(points_sub_2020) +
  tm_dots(col = "blue")
```
## Data analysis


Let's do some point pattern analysis...

error that only projected coordinates can be used for ppp object! let's project - https://epsg.io/2263. Note that this is in feet. 

A better one might be https://epsg.io/6538 as it uses meters 

```{r}
community_areas_projected <- community_areas %>%
  st_transform(., 6538)

points_sub_2020_projected <- points_sub_2020 %>%
  st_transform(., 6538)


window <- as.owin(community_areas_projected)
plot(window)

#create a sp object
points_sub_2020_projected_sp<- points_sub_2020_projected %>%
  as(., 'Spatial')
#create a ppp object
points_sub_2020_projected_sp.ppp <- ppp(x=points_sub_2020_projected_sp@coords[,1],
                          y=points_sub_2020_projected_sp@coords[,2],
                          window=window)
```
Ripley k

EXPLAIN...why i am using ripley's K and what does it show 

```{r}
K <- points_sub_2020_projected_sp.ppp %>%
  Kest(., correction="border") %>%
  plot()
```

EXPLAIN...why i am using DBSCAN, what does it show and:

Why did i select the values of eps and minpts 

* How many evictions do we need for a cluster
* How far must they be

Ripley's K suggests a higher eps, but doesn't consider the min points. I tried a few values and these seemed to give a reasonable result - it is a limitation and other methods (HDBSCAN) can overcome it. 

* I used the distplot in the code below - EXPLAIN what distplot does and shows.....

```{r}
library(sp)

#first extract the points from the spatial points data frame
points_todf <- points_sub_2020_projected_sp %>%
  coordinates(.)%>%
  as.data.frame()

#now run the dbscan analysis
points_todf_DBSCAN <- points_todf %>%
  fpc::dbscan(.,eps = 1000, MinPts = 50)

points_todf%>%
  dbscan::kNNdistplot(.,k=50)

#now quickly plot the results
plot(points_todf_DBSCAN, points_todf, main = "DBSCAN Output", frame = F)
plot(community_areas_projected$geometry, add=T)
```

Add the cluster information to our original dataframe

```{r}
points_todf<- points_todf %>%
  mutate(dbcluster=points_todf_DBSCAN$cluster)

```

Convert our original data frame to a sf object again

```{r}
tosf <- points_todf%>%
  st_as_sf(., coords = c("coords.x1", "coords.x2"), 
                   crs = 6538)%>%
  filter(dbcluster>0)

```

Map the data - remember we are adding layers one by one

```{r}
ggplot(data = community_areas_projected) +
  # add the geometry of the community areas
  geom_sf() +
  # add the geometry of the points - i have had to set the data here to add the layer
  geom_sf(data = tosf, size = 0.4, colour=tosf$dbcluster, fill=tosf$dbcluster)

```
or tmap...very useful colour palette help...`tmaptools::palette_explorer()` from the `tmaptools` package

```{r}
library(tmap)
library(sf)

#tmaptools::palette_explorer()
library(RColorBrewer)
library(tmaptools)
colours<- get_brewer_pal("Set1", n = 19)

tmap_mode("plot")
tm_shape(community_areas) +
  tm_polygons(col = NA, alpha = 0.5) +
tm_shape(tosf) +
  tm_dots(col = "dbcluster",  palette = colours, style = "cat")
```
Now, what could be related to this? After a very quick Google it appears there locations have a higher percent of the population living in poverty: https://www.visualizingeconomics.com/blog/2007/09/22/new-york-city-poverty-map

Although, this map is from 2007 and poverty itself is not defined. But it appears these clusters identified are related to some underlying variable that we haven't accounted for here.


# Clustering

Wrangle

```{r}
library(sf)

#basic join but gives a new row for each point (e.g. 10 points in borough 1 then 10 rows)
check_example <- community_areas_projected%>%
  st_join(tosf)

# spatial join that counts the points per borough
points_sf_joined <- community_areas_projected%>%
  mutate(n = lengths(st_intersects(., tosf)))%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)


# quick map
tm_shape(points_sf_joined) +
    tm_polygons("density",
        style="jenks",
        palette="PuOr",
        title="Eviction density")

```


Cluster

```{r}
library(spdep)

coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()
  
plot(coordsW,axes=TRUE)

# make neighbours

community_nb <- points_sf_joined %>%
  poly2nb(., queen=T)

summary(community_nb)

#plot them
plot(community_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(points_sf_joined$geometry, add=T)

```


```{r}

# make weight matrix
community_nb.lw <- community_nb %>%
  nb2mat(., style="W")

sum(community_nb.lw)

# make weight list for Moran's I

community_nb.lw <- community_nb %>%
  nb2listw(., style="W")
```


Moran's I

```{r}
I_LWard_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., community_nb.lw)

I_LWard_Global_Density
```
Local Moran’s I is:

* The difference between a value and neighbours * the sum of differences between neighbours and the mean

* Where the the difference between a value and neighbours is divided by the standard deviation (how much values in neighbourhood vary about the mean)

*  Z-score is how many standard deviations a value is away (above or below) from the mean

```{r}
I_LWard_Local_density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., community_nb.lw)%>%
  as_tibble()
```

```{r}
points_sf_joined <- points_sf_joined %>%
  mutate(density_I =as.numeric(I_LWard_Local_density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_density$Z.Ii))
```

Map

```{r}
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

library(RColorBrewer)
MoranColours<- rev(brewer.pal(8, "RdGy"))

tm_shape(points_sf_joined) +
    tm_polygons("density_Iz",
        style="fixed",
        breaks=breaks1,
        palette=MoranColours,
        midpoint=NA,
        title="Local Moran's I, Evicitions in New York")

```



## Interpretation

EXPLAIN (state) what you output shows.. you might do this in conjunction with the analysis so a separate section isn't always needed....For example...Ripley's K shows X, which means X and so now i will progress with DBSCAN because....


## Reflection

This is not the end! Do not stop here...you must ...Critically reflect on the results you have produced.

What is critical reflection - we will cover this in the next few sessions as well, but think about to the intro week lecture how to succeed in your degree. 

DISCUSS

* Evictions can be complicated as it is likely these include **NO FAULT** evictions - where the landlord wants to remove the tenant for no fault. This year New York have to moved to a good cause eviction - meaning there must be a cause to actually evict someone. How might this influence our data?

* Whilst we have looked at point data we haven't considered the number of housing units in each area. 
  * Might we expect more evictions in an area with more housing? 
  * And as our min points or distance is set for the whole study area how will this influence our results? 
  * What other methods allow the distance parameter to vary?....https://cran.r-project.org/web/packages/dbscan/vignettes/hdbscan.html 
  * If we were to do spatial autocorrleation we would need to create some sort of rate, see: https://council.nyc.gov/data/evictions/#:~:text=The%20dataset%20is%20updated%20daily,addresses%20are%20cleaned%2C%20click%20here. 

* Why might the results be important (e.g. the question say....New York City wish to conduct a study that aims to prevent people being evicted)
* What other work might this inform
* E.g. now i have clusters, could i extract the community districts and then look at some other data (e.g. census data) to explore factors that might influence evictions? 
* Could i compare years? re-do this for 2019 - are the clusters in different parts if the city? Why might that be?
* How could i do spatial auto correlation on this? What would that show?
* Have you answered your research question, yes, no probably maybe! 


Notes

**do not just just stop and say there are clusters...** think about what it means or what other analysis you could do
What would the client (New York City) be happy with from the analysis
The exam is timed and you need to tactically score across all criteria - do not spend 4 hours trying to make something perfect. Get the basics down first then make parts better - we will cover this in future weeks too.


FAQs

Q: Can i just do clustering and pass.

A: You can not only pass, but you can do very well. **HOWEVER** this depends on how well you score across the marking scheme...If you were to give me just the code used here (nothing else, no project scope, no explanations, no reflection) you may well fail.

## references 
