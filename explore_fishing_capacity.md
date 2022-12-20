Explore Fishing Capacity Data
================
Julia Blanchard
2022-12-19

- <a href="#access-data" id="toc-access-data">Access data</a>
- <a href="#explore-data" id="toc-explore-data">Explore Data</a>

## Access data

First, read in Rousseau et al. 2023 country-level data from the IMAS
Data Portal:

``` r
library(shiny)
library(plotly)
```

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(DT)
```

    ## 
    ## Attaching package: 'DT'

    ## The following objects are masked from 'package:shiny':
    ## 
    ##     dataTableOutput, renderDataTable

``` r
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.1 
    ## ✔ purrr   0.3.4      
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks plotly::filter(), stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(patchwork)


effort<-read_csv(file="https://data.imas.utas.edu.au/attachments/1241a51d-c8c2-4432-aa68-3d2bae142794/CapacityCountryLevel_Detailed.csv")[,-1]
```

    ## New names:
    ## Rows: 293287 Columns: 24
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (9): Length_Category, Sector, Gear, Region, MethodNV, MethodLOA, Method... dbl
    ## (15): ...1, Year, SAUP, NV, GT, P, NVActive, GTActive, PActive, NomActiv...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

``` r
# convert character to factor for key drop-down variables

effort$Country<-as.factor(effort$Country)
effort$Sector<-as.factor(effort$Sector)
effort$Gear<-as.factor(effort$Gear)
```

## Explore Data

For each country you can check to see the fraction of the time series
that are empirical data and the fraction estimated by GAM. This is
specified as either “Data” or GAM under the column “MethodNV” for each
row of data.

####### Shiny App

<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>

Add to above: total relative error on each plot.

Calculate total relative error across all years: sum (1 - ratio of
predicted/observed) for Number of Vessels, Gross Tonnage, and Power.

``` r
#multimodel mean & sd
rel_effort_error<-effort %>% group_by(Region, Country,Sector,Gear, Length_Category) %>%
  summarise(total_errNV = sum(NVerr), total_errGT = sum(GTerr),total_errP=sum(Perr),mean_errNV = mean(NVerr), mean_errGT = mean(GTerr),mean_errP=mean(Perr))
```

    ## `summarise()` has grouped output by 'Region', 'Country', 'Sector', 'Gear'. You
    ## can override using the `.groups` argument.

``` r
print(rel_effort_error)
```

    ## # A tibble: 5,909 × 11
    ## # Groups:   Region, Country, Sector, Gear [2,553]
    ##    Region   Country Sector Gear  Lengt…¹ total…² total…³ total…⁴ mean_…⁵ mean_…⁶
    ##    <chr>    <fct>   <fct>  <fct> <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 Austral… AUS     I      Dred… 12-24m     5.02   0.614  0.181   0.0738 0.00903
    ##  2 Austral… AUS     I      Dred… 24-50m     5.02   0.237  0.0223  0.0738 0.00348
    ##  3 Austral… AUS     I      Dred… 6-12m      5.02   0.714  0.170   0.0738 0.0105 
    ##  4 Austral… AUS     I      Dred… Less t…    5.02  43.2    5.53    0.0738 0.635  
    ##  5 Austral… AUS     I      Gill… 12-24m     5.02   0.856  0.160   0.0738 0.0126 
    ##  6 Austral… AUS     I      Gill… 6-12m      5.02   1.66   0.117   0.0738 0.0243 
    ##  7 Austral… AUS     I      Gill… Less t…    4.68  40.4    5.00    0.0720 0.622  
    ##  8 Austral… AUS     I      Line… 12-24m    10.0    1.51   0.240   0.0738 0.0111 
    ##  9 Austral… AUS     I      Line… 24-50m     5.02   0.134  0.0388  0.0738 0.00196
    ## 10 Austral… AUS     I      Line… 6-12m     10.0    3.61   0.245   0.0738 0.0265 
    ## # … with 5,899 more rows, 1 more variable: mean_errP <dbl>, and abbreviated
    ## #   variable names ¹​Length_Category, ²​total_errNV, ³​total_errGT, ⁴​total_errP,
    ## #   ⁵​mean_errNV, ⁶​mean_errGT

Create Plots for Mean Relative Error across all years

``` r
filtered_data<-subset(rel_effort_error, Sector %in% "UP") # I, UP, or APW
library(viridis)
```

    ## Loading required package: viridisLite

``` r
p1<-ggplot(filtered_data, aes(x=Gear, y=Country, fill = mean_errNV)) +
  geom_tile(color="white", size=0.1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=6),axis.text.y = element_text(size=3)
        ,strip.text.y = element_blank()) +  #remove facet bar on y
scale_fill_viridis(name="Mean Relative Error") +
#scale_fill_distiller(palette = "YlGnBu") +
  ggtitle("Mean error for Number of Vessels (1-  predicted/observed)") +
  facet_grid(rows = vars(filtered_data$Region),
             cols = vars(filtered_data$Length_Category), scales = "free", space="free_y") 

  ggsave("heatmap_NVerr_UP.tiff",p1,device = "tiff",width=7,height=10)


p2<-ggplot(filtered_data, aes(x=Gear, y=Country, fill = mean_errGT)) +
  geom_tile(color="white", size=0.1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=6),axis.text.y = element_text(size=3)
        ,strip.text.y = element_blank()) +  #remove facet bar on y
scale_fill_viridis(name="Mean Relative Error") +
#scale_fill_distiller(palette = "YlGnBu") +
  ggtitle("Mean error for Gross Tonnage (1-  predicted/observed)") +
  facet_grid(rows = vars(filtered_data$Region),
             cols = vars(filtered_data$Length_Category), scales = "free", space="free_y") 

  ggsave("heatmap_GTerr_UP.tiff",p2,device = "tiff",width=7,height=10)

  
  p3<-ggplot(filtered_data, aes(x=Gear, y=Country, fill = mean_errP)) +
  geom_tile(color="white", size=0.1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=6),axis.text.y = element_text(size=3)
        ,strip.text.y = element_blank()) +  #remove facet bar on y
scale_fill_viridis(name="Mean Relative Error") +
#scale_fill_distiller(palette = "YlGnBu") +
  ggtitle("Mean error for Power (1-  predicted/observed)") +
  facet_grid(rows = vars(filtered_data$Region),
             cols = vars(filtered_data$Length_Category), scales = "free", space="free_y") 

  ggsave("heatmap_Perr_UP.tiff",p3,device = "tiff",width=7,height=10)
```
