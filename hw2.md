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

## Qeustion 3

``` r
mon_data = read_csv("./pols-month.csv") %>% 
  janitor::clean_names() %>% 
  separate(., mon, into = c("year","month","day")) %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(month = as.integer(month)) %>% 
  mutate(day = as.integer(day)) %>% 
  mutate(president = ifelse(prez_dem == 1, "dem", "gop")) %>% 
  select(-prez_gop, -prez_dem, -day)
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
mon_data$month = month.abb[mon_data$month]

mon_data
```

    ## # A tibble: 822 × 9
    ##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##    <int> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
    ##  1  1947 Jan        23      51     253      23      45     198 dem      
    ##  2  1947 Feb        23      51     253      23      45     198 dem      
    ##  3  1947 Mar        23      51     253      23      45     198 dem      
    ##  4  1947 Apr        23      51     253      23      45     198 dem      
    ##  5  1947 May        23      51     253      23      45     198 dem      
    ##  6  1947 Jun        23      51     253      23      45     198 dem      
    ##  7  1947 Jul        23      51     253      23      45     198 dem      
    ##  8  1947 Aug        23      51     253      23      45     198 dem      
    ##  9  1947 Sep        23      51     253      23      45     198 dem      
    ## 10  1947 Oct        23      51     253      23      45     198 dem      
    ## # … with 812 more rows

``` r
snp_data = read_csv("./snp.csv") %>% 
  janitor::clean_names() %>% 
  separate(., date, into = c("month","day","year")) %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(year = ifelse(year > 15, year + 1900, year + 2000)) %>%
  mutate(month = as.integer(month)) %>%
  select(year, month, close, -day)
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
snp_data$month = month.abb[snp_data$month]

snp_data
```

    ## # A tibble: 787 × 3
    ##     year month close
    ##    <dbl> <chr> <dbl>
    ##  1  2015 Jul   2080.
    ##  2  2015 Jun   2063.
    ##  3  2015 May   2107.
    ##  4  2015 Apr   2086.
    ##  5  2015 Mar   2068.
    ##  6  2015 Feb   2104.
    ##  7  2015 Jan   1995.
    ##  8  2014 Dec   2059.
    ##  9  2014 Nov   2068.
    ## 10  2014 Oct   2018.
    ## # … with 777 more rows

``` r
une_data = read_csv("./unemployment.csv") %>% 
  pivot_longer(
    Jan:Dec,
    names_to = "month",
    values_to = "unemployment") %>% 
  janitor::clean_names()
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
une_data
```

    ## # A tibble: 816 × 3
    ##     year month unemployment
    ##    <dbl> <chr>        <dbl>
    ##  1  1948 Jan            3.4
    ##  2  1948 Feb            3.8
    ##  3  1948 Mar            4  
    ##  4  1948 Apr            3.9
    ##  5  1948 May            3.5
    ##  6  1948 Jun            3.6
    ##  7  1948 Jul            3.6
    ##  8  1948 Aug            3.9
    ##  9  1948 Sep            3.8
    ## 10  1948 Oct            3.7
    ## # … with 806 more rows

``` r
combined_1 = left_join(snp_data, mon_data, by = c("year","month"))
combined_2 = left_join(combined_1, une_data, by = c("year","month"))

combined_1
```

    ## # A tibble: 787 × 10
    ##     year month close gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##    <dbl> <chr> <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
    ##  1  2015 Jul   2080.      NA      NA      NA      NA      NA      NA <NA>     
    ##  2  2015 Jun   2063.      31      54     246      18      44     188 dem      
    ##  3  2015 May   2107.      31      54     245      18      44     188 dem      
    ##  4  2015 Apr   2086.      31      54     244      18      44     188 dem      
    ##  5  2015 Mar   2068.      31      54     245      18      44     188 dem      
    ##  6  2015 Feb   2104.      31      54     245      18      44     188 dem      
    ##  7  2015 Jan   1995.      31      54     245      18      44     188 dem      
    ##  8  2014 Dec   2059.      29      45     235      21      53     201 dem      
    ##  9  2014 Nov   2068.      29      45     235      21      53     201 dem      
    ## 10  2014 Oct   2018.      29      45     234      21      53     199 dem      
    ## # … with 777 more rows

``` r
combined_2
```

    ## # A tibble: 787 × 11
    ##     year month close gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##    <dbl> <chr> <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
    ##  1  2015 Jul   2080.      NA      NA      NA      NA      NA      NA <NA>     
    ##  2  2015 Jun   2063.      31      54     246      18      44     188 dem      
    ##  3  2015 May   2107.      31      54     245      18      44     188 dem      
    ##  4  2015 Apr   2086.      31      54     244      18      44     188 dem      
    ##  5  2015 Mar   2068.      31      54     245      18      44     188 dem      
    ##  6  2015 Feb   2104.      31      54     245      18      44     188 dem      
    ##  7  2015 Jan   1995.      31      54     245      18      44     188 dem      
    ##  8  2014 Dec   2059.      29      45     235      21      53     201 dem      
    ##  9  2014 Nov   2068.      29      45     235      21      53     201 dem      
    ## 10  2014 Oct   2018.      29      45     234      21      53     199 dem      
    ## # … with 777 more rows, and 1 more variable: unemployment <dbl>

## the first table contains the president variable and many others, the second contains “close”, and the last is about unemployment. We combine all them in the combined_2. The range of year is from 1950 to 2015. We can see how the political party influence the nation’s economic status, like through th unemployment rate.
