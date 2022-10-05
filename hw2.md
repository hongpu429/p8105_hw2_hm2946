p8105_hw2_hm2946.Rmd
================
min
2022-10-04

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readxl)
```

## Question 1

``` r
tran_ent = 
  read_csv(
    "./NYC_Transit_Subway_Entrance_And_Exit_Data.csv",
    col_types = cols(Route8 = "c", Route9 = "c",
                     Route10 = "c", Route11 = "c")
  ) %>% 
  janitor::clean_names() %>% 
  select(
     line, station_name, station_latitude, station_longitude, 
     starts_with("Route"), entry, exit_only, vending, 
     entrance_type, ada) %>% 
  mutate(entry = ifelse(entry == "YES", TRUE, FALSE))

tran_ent
```

    ## # A tibble: 1,868 × 20
    ##    line     station_…¹ stati…² stati…³ route1 route2 route3 route4 route5 route6
    ##    <chr>    <chr>        <dbl>   <dbl> <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
    ##  1 4 Avenue 25th St       40.7   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  2 4 Avenue 25th St       40.7   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  3 4 Avenue 36th St       40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>  
    ##  4 4 Avenue 36th St       40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>  
    ##  5 4 Avenue 36th St       40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>  
    ##  6 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  7 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  8 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ##  9 4 Avenue 45th St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ## 10 4 Avenue 53rd St       40.6   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ## # … with 1,858 more rows, 10 more variables: route7 <chr>, route8 <chr>,
    ## #   route9 <chr>, route10 <chr>, route11 <chr>, entry <lgl>, exit_only <chr>,
    ## #   vending <chr>, entrance_type <chr>, ada <lgl>, and abbreviated variable
    ## #   names ¹​station_name, ²​station_latitude, ³​station_longitude

## First, load the data, and modify the column types;

## Second, clean the data, it contatins line, station_name, station_latitude, station_longitude, Routes, entry, exit_only, vending, entrance_type, ada. The size is 1,868 × 20.

## Last, we convert the entry variable from character to a logical variable, and we know this data is not tidy yet.

## this calculates the number of distinct stations

``` r
tran_ent %>% 
  select(station_name,line) %>% 
  distinct
```

    ## # A tibble: 465 × 2
    ##    station_name             line    
    ##    <chr>                    <chr>   
    ##  1 25th St                  4 Avenue
    ##  2 36th St                  4 Avenue
    ##  3 45th St                  4 Avenue
    ##  4 53rd St                  4 Avenue
    ##  5 59th St                  4 Avenue
    ##  6 77th St                  4 Avenue
    ##  7 86th St                  4 Avenue
    ##  8 95th St                  4 Avenue
    ##  9 9th St                   4 Avenue
    ## 10 Atlantic Av-Barclays Ctr 4 Avenue
    ## # … with 455 more rows

## 465 distinct stations

## this finds ADA compliant stations

``` r
tran_ent %>% 
  filter(ada == TRUE) %>% 
  select(station_name,line) %>% 
  distinct
```

    ## # A tibble: 84 × 2
    ##    station_name                   line           
    ##    <chr>                          <chr>          
    ##  1 Atlantic Av-Barclays Ctr       4 Avenue       
    ##  2 DeKalb Av                      4 Avenue       
    ##  3 Pacific St                     4 Avenue       
    ##  4 Grand Central                  42nd St Shuttle
    ##  5 34th St                        6 Avenue       
    ##  6 47-50th Sts Rockefeller Center 6 Avenue       
    ##  7 Church Av                      6 Avenue       
    ##  8 21st St                        63rd Street    
    ##  9 Lexington Av                   63rd Street    
    ## 10 Roosevelt Island               63rd Street    
    ## # … with 74 more rows

## 84 ADA compliant

## this finds proportion of station entrances / exits without vending allow entrance

``` r
tran_ent %>% 
  filter(vending == "NO") %>% 
  pull(entry) %>% 
  mean
```

    ## [1] 0.3770492

## the proportion is 0.3770492

``` r
tran_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A") %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 60 × 2
    ##    station_name                  line           
    ##    <chr>                         <chr>          
    ##  1 Times Square                  42nd St Shuttle
    ##  2 125th St                      8 Avenue       
    ##  3 145th St                      8 Avenue       
    ##  4 14th St                       8 Avenue       
    ##  5 168th St - Washington Heights 8 Avenue       
    ##  6 175th St                      8 Avenue       
    ##  7 181st St                      8 Avenue       
    ##  8 190th St                      8 Avenue       
    ##  9 34th St                       8 Avenue       
    ## 10 42nd St                       8 Avenue       
    ## # … with 50 more rows

``` r
tran_ent %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A", ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 17 × 2
    ##    station_name                  line            
    ##    <chr>                         <chr>           
    ##  1 14th St                       8 Avenue        
    ##  2 168th St - Washington Heights 8 Avenue        
    ##  3 175th St                      8 Avenue        
    ##  4 34th St                       8 Avenue        
    ##  5 42nd St                       8 Avenue        
    ##  6 59th St                       8 Avenue        
    ##  7 Inwood - 207th St             8 Avenue        
    ##  8 West 4th St                   8 Avenue        
    ##  9 World Trade Center            8 Avenue        
    ## 10 Times Square-42nd St          Broadway        
    ## 11 59th St-Columbus Circle       Broadway-7th Ave
    ## 12 Times Square                  Broadway-7th Ave
    ## 13 8th Av                        Canarsie        
    ## 14 Franklin Av                   Franklin        
    ## 15 Euclid Av                     Fulton          
    ## 16 Franklin Av                   Fulton          
    ## 17 Howard Beach                  Rockaway

## 60 distnct stations serve the A train, and 17 are ada compliant

## Question 2

``` r
mr_trash = 
  read_excel("./Trash Wheel Collection Data.xlsx",
             "Mr. Trash Wheel",
             range = "A2:N549") %>% 
  janitor::clean_names() %>% 
  drop_na(dumpster) %>% 
  mutate(sports_balls = as.integer(round(sports_balls))) %>%
  mutate(year = as.double(year)) %>%
  select(-date, weight = weight_tons,
         volume = volume_cubic_yards)

mr_trash
```

    ## # A tibble: 547 × 13
    ##    dumpster month  year weight volume plastic_…¹ polys…² cigar…³ glass…⁴ groce…⁵
    ##       <dbl> <chr> <dbl>  <dbl>  <dbl>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1        1 May    2014   4.31     18       1450    1820  126000      72     584
    ##  2        2 May    2014   2.74     13       1120    1030   91000      42     496
    ##  3        3 May    2014   3.45     15       2450    3100  105000      50    1080
    ##  4        4 May    2014   3.1      15       2380    2730  100000      52     896
    ##  5        5 May    2014   4.06     18        980     870  120000      72     368
    ##  6        6 May    2014   2.71     13       1430    2140   90000      46     672
    ##  7        7 May    2014   1.91      8        910    1090   56000      32     416
    ##  8        8 May    2014   3.7      16       3580    4310  112000      58    1552
    ##  9        9 June   2014   2.52     14       2400    2790   98000      49     984
    ## 10       10 June   2014   3.76     18       1340    1730  130000      75     448
    ## # … with 537 more rows, 3 more variables: chip_bags <dbl>, sports_balls <int>,
    ## #   homes_powered <dbl>, and abbreviated variable names ¹​plastic_bottles,
    ## #   ²​polystyrene, ³​cigarette_butts, ⁴​glass_bottles, ⁵​grocery_bags

``` r
pro_trash = 
  read_excel("./Trash Wheel Collection Data.xlsx",
             "Professor Trash Wheel",
             range = "A2:M96") %>% 
  janitor::clean_names() %>% 
  drop_na(dumpster) %>% 
  select(-date, weight = weight_tons,
         volume = volume_cubic_yards)

pro_trash
```

    ## # A tibble: 94 × 12
    ##    dumpster month     year weight volume plast…¹ polys…² cigar…³ glass…⁴ groce…⁵
    ##       <dbl> <chr>    <dbl>  <dbl>  <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ##  1        1 January   2017   1.79     15    1950    6080   19700       8    3100
    ##  2        2 January   2017   1.58     15    9540   11230   17600      14    5630
    ##  3        3 February  2017   2.32     18    8350    9210   12000      19    6430
    ##  4        4 February  2017   3.72     15    8590    1030   13000      21    5870
    ##  5        5 February  2017   1.45     15    7830    9950   16000      18    7450
    ##  6        6 March     2017   1.71     15    8210   10340   14000      23    9560
    ##  7        7 April     2017   1.82     15    9830   11020   17000      26   11500
    ##  8        8 April     2017   2.37     15    9240    8760   15000      14    9970
    ##  9        9 May       2017   2.64     15    9540    8810   17000      28   12340
    ## 10       10 May       2017   2.78     15    8230    7800   13000      22   13450
    ## # … with 84 more rows, 2 more variables: chip_bags <dbl>, homes_powered <dbl>,
    ## #   and abbreviated variable names ¹​plastic_bottles, ²​polystyrene,
    ## #   ³​cigarette_butts, ⁴​glass_bottles, ⁵​grocery_bags

``` r
mr_trash = 
  mutate(mr_trash, label = "mr") %>% 
  pivot_longer(
    plastic_bottles:sports_balls,
    names_to = "trash_type",
    values_to = "collect_number")
pro_trash = mutate(pro_trash, label = "pro") %>% 
  pivot_longer(
    plastic_bottles:chip_bags,
    names_to = "trash_type",
    values_to = "collect_number")
com_tidy =  bind_rows(mr_trash, pro_trash) %>% 
  janitor::clean_names() %>% 
  select(label, everything()) 

com_tidy
```

    ## # A tibble: 4,393 × 9
    ##    label dumpster month  year weight volume homes_powered trash_type     colle…¹
    ##    <chr>    <dbl> <chr> <dbl>  <dbl>  <dbl>         <dbl> <chr>            <dbl>
    ##  1 mr           1 May    2014   4.31     18             0 plastic_bottl…    1450
    ##  2 mr           1 May    2014   4.31     18             0 polystyrene       1820
    ##  3 mr           1 May    2014   4.31     18             0 cigarette_but…  126000
    ##  4 mr           1 May    2014   4.31     18             0 glass_bottles       72
    ##  5 mr           1 May    2014   4.31     18             0 grocery_bags       584
    ##  6 mr           1 May    2014   4.31     18             0 chip_bags         1162
    ##  7 mr           1 May    2014   4.31     18             0 sports_balls         7
    ##  8 mr           2 May    2014   2.74     13             0 plastic_bottl…    1120
    ##  9 mr           2 May    2014   2.74     13             0 polystyrene       1030
    ## 10 mr           2 May    2014   2.74     13             0 cigarette_but…   91000
    ## # … with 4,383 more rows, and abbreviated variable name ¹​collect_number

## The size of the combined dataset is 4393\*9, and it has important variables like year and weight. The total weight of trash collected by Professor Trash Wheel is 1140.72, and the total number of sports balls collected by Mr. Trash Wheel in 2020 is 856.

``` r
com_tidy %>% 
  filter(label == "pro") %>% 
  select(weight) %>% 
  sum()
```

    ## [1] 1140.72

``` r
com_tidy %>% 
  filter(label == "mr", year == 2020, trash_type == "sports_balls") %>% 
  select(collect_number) %>% 
  sum()
```

    ## [1] 856
