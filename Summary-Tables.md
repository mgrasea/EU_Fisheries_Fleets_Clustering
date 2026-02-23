# Summary Tables

This filters the AER 2025 fisheries data to create summary tables of
fleet clusters per species/stock.

## Libraries

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.4.3

``` r
library(writexl)
```

    ## Warning: package 'writexl' was built under R version 4.4.3

## Data

### 1. AER 2025 Data

``` r
data_AER <- read.csv("C:/MER2030/Sem 4 AZTI Thesis/Fisheries Data Analysis/STECF_25 07_Landings_2018-2023.csv")
head(data_AER)
```

    ##   upload_date country_name country_code year supra_reg fishing_tech
    ## 1  24/02/2020      Belgium          BEL 2018       NAO          TBB
    ## 2  24/02/2020      Belgium          BEL 2018       NAO          TBB
    ## 3  24/02/2020      Belgium          BEL 2018       NAO          TBB
    ## 4  24/02/2020      Belgium          BEL 2018       NAO          TBB
    ## 5  24/02/2020      Belgium          BEL 2018       NAO          DTS
    ## 6  24/02/2020      Belgium          BEL 2018       NAO          TBB
    ##   vessel_length geo_indicator      cluster_name               fs_name
    ## 1        VL2440           NGI                     BEL NAO TBB2440 NGI
    ## 2        VL2440           NGI                     BEL NAO TBB2440 NGI
    ## 3        VL2440           NGI                     BEL NAO TBB2440 NGI
    ## 4        VL2440           NGI                     BEL NAO TBB2440 NGI
    ## 5        VL2440           NGI AREA27 DTS VL2440 BEL NAO DTS2440 NGI *
    ## 6        VL2440           NGI                     BEL NAO TBB2440 NGI
    ##   variable_group     variable_name variable_code    value unit  species_name
    ## 1       Landings Value of landings   totvallandg 16840.36 euro         Brill
    ## 2       Landings Value of landings   totvallandg 52633.80 euro   Common sole
    ## 3       Landings Value of landings   totvallandg     2.46 euro Raja rays nei
    ## 4       Landings Value of landings   totvallandg  5456.36 euro     John dory
    ## 5       Landings Value of landings   totvallandg   958.32 euro   Red gurnard
    ## 6       Landings Value of landings   totvallandg   176.77 euro  Grey gurnard
    ##   species_code sub_reg fromtable framework template_name gear fishery activity
    ## 1          BLL  27.8.b map_fsfao       map     map_fsfao           NA         
    ## 2          SOL  27.7.j map_fsfao       map     map_fsfao           NA         
    ## 3          SKA  27.7.j map_fsfao       map     map_fsfao           NA         
    ## 4          JOD  27.7.h map_fsfao       map     map_fsfao           NA         
    ## 5          GUR  27.7.e map_fsfao       map     map_fsfao           NA         
    ## 6          GUG  27.7.a map_fsfao       map     map_fsfao           NA

- Set missing data to NA

``` r
data_AER <- data_AER %>%
  mutate(across(where(is.character), ~ na_if(trimws(.), "")))  # "" or "   " -> NA

na_counts <- colSums(is.na(data_AER))
na_counts
```

    ##    upload_date   country_name   country_code           year      supra_reg 
    ##              0              0              0              0              0 
    ##   fishing_tech  vessel_length  geo_indicator   cluster_name        fs_name 
    ##              0              0          33880         451918              0 
    ## variable_group  variable_name  variable_code          value           unit 
    ##              0              0              0            521              0 
    ##   species_name   species_code        sub_reg      fromtable      framework 
    ##            158              0              0              0              0 
    ##  template_name           gear        fishery       activity 
    ##              0         718920         731158         720012

- Information about data

- Data summary

``` r
summary(data_AER)
```

    ##  upload_date        country_name       country_code            year     
    ##  Length:731158      Length:731158      Length:731158      Min.   :2018  
    ##  Class :character   Class :character   Class :character   1st Qu.:2019  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :2021  
    ##                                                           Mean   :2021  
    ##                                                           3rd Qu.:2022  
    ##                                                           Max.   :2024  
    ##                                                                         
    ##   supra_reg         fishing_tech       vessel_length      geo_indicator     
    ##  Length:731158      Length:731158      Length:731158      Length:731158     
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  cluster_name         fs_name          variable_group     variable_name     
    ##  Length:731158      Length:731158      Length:731158      Length:731158     
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  variable_code          value               unit           species_name      
    ##  Length:731158      Min.   :        0   Length:731158      Length:731158     
    ##  Class :character   1st Qu.:       42   Class :character   Class :character  
    ##  Mode  :character   Median :      487   Mode  :character   Mode  :character  
    ##                     Mean   :    88419                                        
    ##                     3rd Qu.:     5939                                        
    ##                     Max.   :153387134                                        
    ##                     NA's   :521                                              
    ##  species_code         sub_reg           fromtable          framework        
    ##  Length:731158      Length:731158      Length:731158      Length:731158     
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  template_name          gear           fishery          activity        
    ##  Length:731158      Length:731158      Mode:logical   Length:731158     
    ##  Class :character   Class :character   NA's:731158    Class :character  
    ##  Mode  :character   Mode  :character                  Mode  :character  
    ##                                                                         
    ##                                                                         
    ##                                                                         
    ## 

### 2. EU TAC Data

``` r
data_TAC <- read_excel("C:/MER2030/Sem 4 AZTI Thesis/Fisheries Data Analysis/EU TACs.xlsx", sheet = "Sheet1")
head(data_TAC)
```

    ## # A tibble: 6 × 13
    ##   TAC_code    common_name scientific_name  species_code year      area  TAC_type
    ##   <chr>       <chr>       <chr>            <chr>        <chr>     <chr> <chr>   
    ## 1 MAC/2A34-N. Mackerel    Scomber scombrus MAC          2025-2026 3a; … Analyti…
    ## 2 MAC/2A34-N. Mackerel    Scomber scombrus MAC          2025-2026 3a; … Analyti…
    ## 3 MAC/2A34-N. Mackerel    Scomber scombrus MAC          2025-2026 3a; … Analyti…
    ## 4 MAC/2A34-N. Mackerel    Scomber scombrus MAC          2025-2026 3a; … Analyti…
    ## 5 MAC/2A34-N. Mackerel    Scomber scombrus MAC          2025-2026 3a; … Analyti…
    ## 6 MAC/2A34-N. Mackerel    Scomber scombrus MAC          2025-2026 3a; … Analyti…
    ## # ℹ 6 more variables: country_name <chr>, country_code <chr>,
    ## #   quota_tonnes <dbl>, special_conditions <chr>, regulation <chr>, table <dbl>

## Atlantic Mackerel (*Scomber scombrus*)

### 1. Filter data

- Removing the “Value of Landings” variable to avoid row duplication
- MAC (Atlantic Mackerel species code)
- Only NAO, removing MBS (Med and Black Sea)
- Only one year, because some segments are present in one year and not
  the others
- **Grouping clusters by supra_reg, sub_reg, fishing_tech, and
  vessel_length**

NOTES: Atlantic Mackerel, *Scomber scombrus*, only has one EU-wide
stock. <a href="https://stockdatabase.ices.dk/ViewStock.aspx?key=4162"
class="uri">ICES Stock</a>

``` r
mac_clusters <- data_AER %>%
  filter(variable_name == "Live weight of landings",
         species_code == "MAC",
         supra_reg == "NAO",
         year == 2023) %>%
  group_by(country_name, supra_reg, sub_reg, fishing_tech, vessel_length) %>%
  summarise(
    n_rows = n(),
    live_weight_sum = sum(value, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(country_name, supra_reg, sub_reg, fishing_tech, vessel_length) 

mac_clusters
```

    ## # A tibble: 481 × 7
    ##    country_name supra_reg sub_reg fishing_tech vessel_length n_rows
    ##    <chr>        <chr>     <chr>   <chr>        <chr>          <int>
    ##  1 Belgium      NAO       27.4.b  DTS          VL2440             1
    ##  2 Belgium      NAO       27.4.b  TBB          VL2440             1
    ##  3 Belgium      NAO       27.4.c  DTS          VL2440             1
    ##  4 Belgium      NAO       27.4.c  TBB          VL1824             1
    ##  5 Belgium      NAO       27.4.c  TBB          VL2440             1
    ##  6 Belgium      NAO       27.7.d  DTS          VL2440             1
    ##  7 Belgium      NAO       27.7.d  TBB          VL2440             1
    ##  8 Belgium      NAO       27.7.e  DTS          VL2440             1
    ##  9 Belgium      NAO       27.7.f  DTS          VL2440             1
    ## 10 Belgium      NAO       27.7.g  TBB          VL2440             1
    ## # ℹ 471 more rows
    ## # ℹ 1 more variable: live_weight_sum <dbl>

### 2. Cluster metrics

- Total number of clusters

``` r
n_mac_clusters <- nrow(mac_clusters)
n_mac_clusters
```

    ## [1] 481

- The number of clusters per year varies

``` r
mac_clusters_per_year <- data_AER %>%
  filter(species_code == "MAC",
         supra_reg == "NAO",
         variable_name == "Live weight of landings") %>%
  group_by(year) %>%
  summarise(
    n_clusters = n_distinct(country_name, supra_reg, sub_reg, fishing_tech, vessel_length),
    .groups = "drop"
  ) %>%
  arrange(year)

mac_clusters_per_year
```

    ## # A tibble: 7 × 2
    ##    year n_clusters
    ##   <int>      <int>
    ## 1  2018        489
    ## 2  2019        538
    ## 3  2020        495
    ## 4  2021        517
    ## 5  2022        502
    ## 6  2023        481
    ## 7  2024        207

- Count the number of countries with representative clusters

``` r
mac_clusters %>%
  distinct(country_name)
```

    ## # A tibble: 10 × 1
    ##    country_name
    ##    <chr>       
    ##  1 Belgium     
    ##  2 Denmark     
    ##  3 France      
    ##  4 Germany     
    ##  5 Ireland     
    ##  6 Netherlands 
    ##  7 Poland      
    ##  8 Portugal    
    ##  9 Spain       
    ## 10 Sweden

- Count the number of fishing gears used to exploit the stock

``` r
mac_clusters %>%
  distinct(fishing_tech)
```

    ## # A tibble: 14 × 1
    ##    fishing_tech
    ##    <chr>       
    ##  1 DTS         
    ##  2 TBB         
    ##  3 DFN         
    ##  4 TM          
    ##  5 FPO         
    ##  6 DRB         
    ##  7 MGP         
    ##  8 HOK         
    ##  9 MGO         
    ## 10 PGP         
    ## 11 PMP         
    ## 12 PGO         
    ## 13 PS          
    ## 14 PG

- Count the number of subregions where the stock is exploited

``` r
mac_clusters %>%
  distinct(sub_reg)
```

    ## # A tibble: 35 × 1
    ##    sub_reg  
    ##    <chr>    
    ##  1 27.4.b   
    ##  2 27.4.c   
    ##  3 27.7.d   
    ##  4 27.7.e   
    ##  5 27.7.f   
    ##  6 27.7.g   
    ##  7 27.3.a.20
    ##  8 27.3.a.21
    ##  9 27.3.b.23
    ## 10 27.3.c.22
    ## # ℹ 25 more rows

#### Total landings per country

- in metric tons

``` r
mac_country_totals <- mac_clusters %>%
  group_by(country_name) %>%
  summarise(
    live_weight_country_total = round(sum(live_weight_sum, na.rm = TRUE)*0.001), #convert to metric tons / tonnes (same as TAC Regulation unit)
    .groups = "drop"
  ) %>%
  arrange(desc(live_weight_country_total))

mac_country_totals
```

    ## # A tibble: 10 × 2
    ##    country_name live_weight_country_total
    ##    <chr>                            <dbl>
    ##  1 Ireland                          51947
    ##  2 Denmark                          27820
    ##  3 Spain                            25242
    ##  4 Netherlands                      20588
    ##  5 Germany                          16821
    ##  6 France                           14838
    ##  7 Sweden                            2267
    ##  8 Portugal                          2155
    ##  9 Poland                             255
    ## 10 Belgium                             68

### 3. Cluster reduction

#### Total TAC per country

- Top 5 countries are: Spain, Ireland, Denmark, Netherlands, Germany

``` r
mac_TAC_totals <- data_TAC %>%
  filter (species_code == "MAC")  %>%
  group_by(country_name) %>%
  summarise(
    TAC_country_total = round(sum(quota_tonnes, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(desc(TAC_country_total))

mac_TAC_totals
```

    ## # A tibble: 15 × 2
    ##    country_name  TAC_country_total
    ##    <chr>                     <dbl>
    ##  1 Spain                    217190
    ##  2 Ireland                   39914
    ##  3 Denmark                   21995
    ##  4 Netherlands               15264
    ##  5 Germany                   10036
    ##  6 France                     7769
    ##  7 Portugal                   4489
    ##  8 Sweden                     3641
    ##  9 Poland                      679
    ## 10 Belgium                     380
    ## 11 Estonia                      80
    ## 12 Latvia                       59
    ## 13 Lithuania                    59
    ## 14 Faroe Islands                 0
    ## 15 Norway                        0

- Filter the top 5 countries list

``` r
top5_mac <- mac_TAC_totals %>%
  slice_max(TAC_country_total, n = 5, with_ties = FALSE) %>%  # pick top 5
  select(country_name)
top5_mac
```

    ## # A tibble: 5 × 1
    ##   country_name
    ##   <chr>       
    ## 1 Spain       
    ## 2 Ireland     
    ## 3 Denmark     
    ## 4 Netherlands 
    ## 5 Germany

#### Clusters of Top 5 countries

- With subregion

``` r
mac_clusters_top5 <- mac_clusters %>%
  semi_join(top5_mac, by = "country_name") %>% # filter to top 5
  arrange(country_name, supra_reg, sub_reg, fishing_tech, vessel_length)
mac_clusters_top5
```

    ## # A tibble: 227 × 7
    ##    country_name supra_reg sub_reg   fishing_tech vessel_length n_rows
    ##    <chr>        <chr>     <chr>     <chr>        <chr>          <int>
    ##  1 Denmark      NAO       27.3.a.20 DFN          VL0010             1
    ##  2 Denmark      NAO       27.3.a.20 DFN          VL1012             1
    ##  3 Denmark      NAO       27.3.a.20 DFN          VL1218             1
    ##  4 Denmark      NAO       27.3.a.20 DFN          VL1824             1
    ##  5 Denmark      NAO       27.3.a.20 DTS          VL0010             1
    ##  6 Denmark      NAO       27.3.a.20 DTS          VL1012             1
    ##  7 Denmark      NAO       27.3.a.20 DTS          VL1218             1
    ##  8 Denmark      NAO       27.3.a.20 DTS          VL1824             1
    ##  9 Denmark      NAO       27.3.a.20 DTS          VL2440             1
    ## 10 Denmark      NAO       27.3.a.20 TBB          VL1218             1
    ## # ℹ 217 more rows
    ## # ℹ 1 more variable: live_weight_sum <dbl>

- Without subregion

``` r
mac_clusters_top5 %>%
  group_by(country_name, supra_reg, fishing_tech, vessel_length) %>%  # <- sub_reg removed
  summarise(
    n_rows = sum(n_rows),
    live_weight_sum = sum(live_weight_sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country_name, supra_reg,fishing_tech, vessel_length)
```

    ## # A tibble: 84 × 6
    ##    country_name supra_reg fishing_tech vessel_length n_rows live_weight_sum
    ##    <chr>        <chr>     <chr>        <chr>          <int>           <dbl>
    ##  1 Denmark      NAO       DFN          VL0008             2            3508
    ##  2 Denmark      NAO       DFN          VL0010             3           11877
    ##  3 Denmark      NAO       DFN          VL0812             4            8866
    ##  4 Denmark      NAO       DFN          VL1012             3            1237
    ##  5 Denmark      NAO       DFN          VL1218             2             419
    ##  6 Denmark      NAO       DFN          VL1824             3             486
    ##  7 Denmark      NAO       DRB          VL0010             1               7
    ##  8 Denmark      NAO       DTS          VL0010             2              79
    ##  9 Denmark      NAO       DTS          VL0812             2              14
    ## 10 Denmark      NAO       DTS          VL1012             4             851
    ## # ℹ 74 more rows

``` r
mac_clusters_top5
```

    ## # A tibble: 227 × 7
    ##    country_name supra_reg sub_reg   fishing_tech vessel_length n_rows
    ##    <chr>        <chr>     <chr>     <chr>        <chr>          <int>
    ##  1 Denmark      NAO       27.3.a.20 DFN          VL0010             1
    ##  2 Denmark      NAO       27.3.a.20 DFN          VL1012             1
    ##  3 Denmark      NAO       27.3.a.20 DFN          VL1218             1
    ##  4 Denmark      NAO       27.3.a.20 DFN          VL1824             1
    ##  5 Denmark      NAO       27.3.a.20 DTS          VL0010             1
    ##  6 Denmark      NAO       27.3.a.20 DTS          VL1012             1
    ##  7 Denmark      NAO       27.3.a.20 DTS          VL1218             1
    ##  8 Denmark      NAO       27.3.a.20 DTS          VL1824             1
    ##  9 Denmark      NAO       27.3.a.20 DTS          VL2440             1
    ## 10 Denmark      NAO       27.3.a.20 TBB          VL1218             1
    ## # ℹ 217 more rows
    ## # ℹ 1 more variable: live_weight_sum <dbl>

#### Merging subregions

- 6, 7, 8a, 8b, 8d and 8e; United Kingdom and international waters of
  5b; international waters of 2a, 12 and 14 (MAC/2CX14-)

``` r
mac_clusters %>%
filter(str_detect(sub_reg, "^27\\.(5|6|7|8)")) %>%
  group_by(country_name, supra_reg, fishing_tech, vessel_length) %>%
  summarise(
    n_rows = n(),
    live_weight_sum = sum(live_weight_sum, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  arrange(country_name, supra_reg, fishing_tech, vessel_length)
```

    ## # A tibble: 92 × 6
    ##    country_name supra_reg fishing_tech vessel_length n_rows live_weight_sum
    ##    <chr>        <chr>     <chr>        <chr>          <int>           <dbl>
    ##  1 Belgium      NAO       DTS          VL2440             3           9549.
    ##  2 Belgium      NAO       TBB          VL2440             2           1966.
    ##  3 Denmark      NAO       TM           VL40XX             5        8453094.
    ##  4 France       NAO       DFN          VL0010             5          72432.
    ##  5 France       NAO       DFN          VL1012             7          42762.
    ##  6 France       NAO       DFN          VL1218             5          12417.
    ##  7 France       NAO       DFN          VL1824             3            945.
    ##  8 France       NAO       DFN          VL2440             3          12405.
    ##  9 France       NAO       DRB          VL0010             3           1833.
    ## 10 France       NAO       DRB          VL1012             3          63847.
    ## # ℹ 82 more rows

- 8c, 9 and 10; Union waters of CECAF 34.1.1 (MAC/8C3411)

``` r
mac_clusters %>%
filter(str_detect(sub_reg, "^27\\.(8c|9|10)")|
       str_detect(sub_reg, "^34\\.(1.1)")) %>%
  group_by(country_name, supra_reg, fishing_tech, vessel_length) %>%
  summarise(
    n_rows = n(),
    live_weight_sum = sum(live_weight_sum, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  arrange(country_name, supra_reg, fishing_tech, vessel_length)
```

    ## # A tibble: 48 × 6
    ##    country_name supra_reg fishing_tech vessel_length n_rows live_weight_sum
    ##    <chr>        <chr>     <chr>        <chr>          <int>           <dbl>
    ##  1 France       NAO       FPO          VL0010             1            45.6
    ##  2 Portugal     NAO       DFN          VL0010             1          2777  
    ##  3 Portugal     NAO       DFN          VL1012             1           839  
    ##  4 Portugal     NAO       DFN          VL1218             1          5315  
    ##  5 Portugal     NAO       DFN          VL1824             1          5404  
    ##  6 Portugal     NAO       DTS          VL0010             1          2469  
    ##  7 Portugal     NAO       DTS          VL1012             1          8049  
    ##  8 Portugal     NAO       DTS          VL1218             1          1911  
    ##  9 Portugal     NAO       DTS          VL2440             1        506461  
    ## 10 Portugal     NAO       FPO          VL0010             1            13  
    ## # ℹ 38 more rows

## European Hake (*Merluccius merluccius*)

### 1. Filter data

Same filtering as the first one - Atlantic Mackerel

``` r
hke_clusters <- data_AER %>%
  filter(variable_name == "Live weight of landings",
         species_code == "HKE",
         supra_reg == "NAO",
         year == 2023) %>%
  group_by(country_name, supra_reg, sub_reg, fishing_tech, vessel_length) %>%
  summarise(
    n_rows = n(),
    live_weight_sum = sum(value, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(country_name, supra_reg, sub_reg, fishing_tech, vessel_length) 

hke_clusters
```

    ## # A tibble: 457 × 7
    ##    country_name supra_reg sub_reg fishing_tech vessel_length n_rows
    ##    <chr>        <chr>     <chr>   <chr>        <chr>          <int>
    ##  1 Belgium      NAO       27.4.a  DTS          VL2440             1
    ##  2 Belgium      NAO       27.4.b  DTS          VL2440             1
    ##  3 Belgium      NAO       27.4.b  TBB          VL1824             1
    ##  4 Belgium      NAO       27.4.b  TBB          VL2440             1
    ##  5 Belgium      NAO       27.4.c  DTS          VL2440             1
    ##  6 Belgium      NAO       27.4.c  TBB          VL2440             1
    ##  7 Belgium      NAO       27.7.a  DTS          VL2440             1
    ##  8 Belgium      NAO       27.7.a  TBB          VL2440             1
    ##  9 Belgium      NAO       27.7.d  DTS          VL2440             1
    ## 10 Belgium      NAO       27.7.d  TBB          VL2440             1
    ## # ℹ 447 more rows
    ## # ℹ 1 more variable: live_weight_sum <dbl>

### 2. Cluster metrics

- Total number of clusters

``` r
nrow(hke_clusters)
```

    ## [1] 457

- The number of clusters per year varies

``` r
hke_clusters_per_year <- data_AER %>%
  filter(species_code == "HKE",
         supra_reg == "NAO",
         variable_name == "Live weight of landings") %>%
  group_by(year) %>%
  summarise(
    n_clusters = n_distinct(country_name, supra_reg, sub_reg, fishing_tech, vessel_length),
    .groups = "drop"
  ) %>%
  arrange(year)

hke_clusters_per_year
```

    ## # A tibble: 7 × 2
    ##    year n_clusters
    ##   <int>      <int>
    ## 1  2018        512
    ## 2  2019        520
    ## 3  2020        486
    ## 4  2021        503
    ## 5  2022        481
    ## 6  2023        457
    ## 7  2024        162

- Count the number of countries with representative clusters

``` r
hke_clusters %>%
  distinct(country_name)
```

    ## # A tibble: 9 × 1
    ##   country_name
    ##   <chr>       
    ## 1 Belgium     
    ## 2 Denmark     
    ## 3 France      
    ## 4 Germany     
    ## 5 Ireland     
    ## 6 Netherlands 
    ## 7 Portugal    
    ## 8 Spain       
    ## 9 Sweden

- Count the number of fishing gears used to exploit the stock

``` r
hke_clusters %>%
  distinct(fishing_tech)
```

    ## # A tibble: 13 × 1
    ##    fishing_tech
    ##    <chr>       
    ##  1 DTS         
    ##  2 TBB         
    ##  3 DFN         
    ##  4 DRB         
    ##  5 FPO         
    ##  6 TM          
    ##  7 HOK         
    ##  8 MGP         
    ##  9 PGP         
    ## 10 PMP         
    ## 11 PS          
    ## 12 MGO         
    ## 13 PG

- Count the number of subregions where the stock is exploited

``` r
hke_clusters %>%
  distinct(sub_reg)
```

    ## # A tibble: 31 × 1
    ##    sub_reg
    ##    <chr>  
    ##  1 27.4.a 
    ##  2 27.4.b 
    ##  3 27.4.c 
    ##  4 27.7.a 
    ##  5 27.7.d 
    ##  6 27.7.e 
    ##  7 27.7.f 
    ##  8 27.7.g 
    ##  9 27.7.h 
    ## 10 27.7.j 
    ## # ℹ 21 more rows

#### Total landings per country

- in metric tons

``` r
hke_country_totals <- hke_clusters %>%
  group_by(country_name) %>%
  summarise(
    live_weight_country_total = round(sum(live_weight_sum, na.rm = TRUE)*0.001), #convert to metric tons / tonnes (same as TAC Regulation unit)
    .groups = "drop"
  ) %>%
  arrange(desc(live_weight_country_total))

hke_country_totals
```

    ## # A tibble: 9 × 2
    ##   country_name live_weight_country_total
    ##   <chr>                            <dbl>
    ## 1 Spain                            26837
    ## 2 France                           25646
    ## 3 Ireland                           3435
    ## 4 Denmark                           2973
    ## 5 Portugal                          1592
    ## 6 Germany                            327
    ## 7 Netherlands                        164
    ## 8 Belgium                            136
    ## 9 Sweden                              93

### 3. Cluster reduction

#### Total TAC per country

- Top 5 countries are: France, Spain, Portugal, Denmark, Ireland

``` r
hke_TAC_totals <- data_TAC %>%
  filter (species_code == "HKE")  %>%
  group_by(country_name) %>%
  summarise(
    TAC_country_total = round(sum(quota_tonnes, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(desc(TAC_country_total))

hke_TAC_totals
```

    ## # A tibble: 9 × 2
    ##   country_name TAC_country_total
    ##   <chr>                    <dbl>
    ## 1 France                   30514
    ## 2 Spain                    26867
    ## 3 Portugal                  5111
    ## 4 Denmark                   3611
    ## 5 Ireland                   1730
    ## 6 Netherlands                350
    ## 7 Belgium                    329
    ## 8 Germany                    227
    ## 9 Sweden                     137

- Filter the top 5 countries list

``` r
top5_hke <- hke_TAC_totals %>%
  slice_max(TAC_country_total, n = 5, with_ties = FALSE) %>%  # pick top 5
  select(country_name)
top5_hke
```

    ## # A tibble: 5 × 1
    ##   country_name
    ##   <chr>       
    ## 1 France      
    ## 2 Spain       
    ## 3 Portugal    
    ## 4 Denmark     
    ## 5 Ireland

#### Clusters of Top 5 countries

- With subregion

``` r
hke_clusters_top5 <- hke_clusters %>%
  semi_join(top5_hke, by = "country_name") %>% # filter to top 5
  arrange(country_name, supra_reg, sub_reg, fishing_tech, vessel_length)
hke_clusters_top5
```

    ## # A tibble: 389 × 7
    ##    country_name supra_reg sub_reg   fishing_tech vessel_length n_rows
    ##    <chr>        <chr>     <chr>     <chr>        <chr>          <int>
    ##  1 Denmark      NAO       27.3.a.20 DFN          VL0010             1
    ##  2 Denmark      NAO       27.3.a.20 DFN          VL1012             1
    ##  3 Denmark      NAO       27.3.a.20 DFN          VL1218             1
    ##  4 Denmark      NAO       27.3.a.20 DFN          VL1824             1
    ##  5 Denmark      NAO       27.3.a.20 DRB          VL0010             1
    ##  6 Denmark      NAO       27.3.a.20 DTS          VL0010             1
    ##  7 Denmark      NAO       27.3.a.20 DTS          VL0812             1
    ##  8 Denmark      NAO       27.3.a.20 DTS          VL1012             1
    ##  9 Denmark      NAO       27.3.a.20 DTS          VL1218             1
    ## 10 Denmark      NAO       27.3.a.20 DTS          VL1824             1
    ## # ℹ 379 more rows
    ## # ℹ 1 more variable: live_weight_sum <dbl>

- Without subregion

``` r
hke_clusters_top5 %>%
  group_by(country_name, supra_reg, fishing_tech, vessel_length) %>%  # <- sub_reg removed
  summarise(
    n_rows = sum(n_rows),
    live_weight_sum = sum(live_weight_sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country_name, supra_reg,fishing_tech, vessel_length)
```

    ## # A tibble: 130 × 6
    ##    country_name supra_reg fishing_tech vessel_length n_rows live_weight_sum
    ##    <chr>        <chr>     <chr>        <chr>          <int>           <dbl>
    ##  1 Denmark      NAO       DFN          VL0010             3          26751 
    ##  2 Denmark      NAO       DFN          VL0812             2              4 
    ##  3 Denmark      NAO       DFN          VL1012             2          29434 
    ##  4 Denmark      NAO       DFN          VL1218             3         327091 
    ##  5 Denmark      NAO       DFN          VL1824             4         216460 
    ##  6 Denmark      NAO       DRB          VL0010             2             91 
    ##  7 Denmark      NAO       DTS          VL0010             3           1858 
    ##  8 Denmark      NAO       DTS          VL0812             2             55 
    ##  9 Denmark      NAO       DTS          VL1012             3           3451 
    ## 10 Denmark      NAO       DTS          VL1218             4         128270.
    ## # ℹ 120 more rows

``` r
hke_clusters_top5
```

    ## # A tibble: 389 × 7
    ##    country_name supra_reg sub_reg   fishing_tech vessel_length n_rows
    ##    <chr>        <chr>     <chr>     <chr>        <chr>          <int>
    ##  1 Denmark      NAO       27.3.a.20 DFN          VL0010             1
    ##  2 Denmark      NAO       27.3.a.20 DFN          VL1012             1
    ##  3 Denmark      NAO       27.3.a.20 DFN          VL1218             1
    ##  4 Denmark      NAO       27.3.a.20 DFN          VL1824             1
    ##  5 Denmark      NAO       27.3.a.20 DRB          VL0010             1
    ##  6 Denmark      NAO       27.3.a.20 DTS          VL0010             1
    ##  7 Denmark      NAO       27.3.a.20 DTS          VL0812             1
    ##  8 Denmark      NAO       27.3.a.20 DTS          VL1012             1
    ##  9 Denmark      NAO       27.3.a.20 DTS          VL1218             1
    ## 10 Denmark      NAO       27.3.a.20 DTS          VL1824             1
    ## # ℹ 379 more rows
    ## # ℹ 1 more variable: live_weight_sum <dbl>

## Horse Mackerel (*Trachurus trachurus*)

### 1. Filter data

NOTE: in the AER 2025 Data, JAX=Jack and Horse Mackerel, but not
including CJM = Chilean jack mackerel, this is also separate in the TAC
Reg 2025/202,\].

``` r
jax_clusters <- data_AER %>%
  filter(variable_name == "Live weight of landings",
         species_code == "JAX",
         supra_reg == "NAO",
         year == 2023) %>%
  group_by(country_name, supra_reg, sub_reg, fishing_tech, vessel_length) %>%
  summarise(
    n_rows = n(),
    live_weight_sum = sum(value, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(country_name, supra_reg, sub_reg, fishing_tech, vessel_length) 

jax_clusters
```

    ## # A tibble: 64 × 7
    ##    country_name supra_reg sub_reg fishing_tech vessel_length n_rows
    ##    <chr>        <chr>     <chr>   <chr>        <chr>          <int>
    ##  1 Belgium      NAO       27.4.b  DTS          VL2440             1
    ##  2 Belgium      NAO       27.4.c  DTS          VL2440             1
    ##  3 Belgium      NAO       27.4.c  TBB          VL1824             1
    ##  4 Belgium      NAO       27.4.c  TBB          VL2440             1
    ##  5 Belgium      NAO       27.7.d  DTS          VL2440             1
    ##  6 Belgium      NAO       27.7.d  TBB          VL1824             1
    ##  7 Belgium      NAO       27.7.d  TBB          VL2440             1
    ##  8 Belgium      NAO       27.7.e  TBB          VL2440             1
    ##  9 Belgium      NAO       27.7.f  DTS          VL2440             1
    ## 10 Belgium      NAO       27.7.f  TBB          VL2440             1
    ## # ℹ 54 more rows
    ## # ℹ 1 more variable: live_weight_sum <dbl>

### 2. Cluster metrics

- Total number of clusters

``` r
nrow(jax_clusters)
```

    ## [1] 64

- The number of clusters per year varies

``` r
jax_clusters_per_year <- data_AER %>%
  filter(species_code == "JAX",
         supra_reg == "NAO",
         variable_name == "Live weight of landings") %>%
  group_by(year) %>%
  summarise(
    n_clusters = n_distinct(country_name, supra_reg, sub_reg, fishing_tech, vessel_length),
    .groups = "drop"
  ) %>%
  arrange(year)

jax_clusters_per_year
```

    ## # A tibble: 7 × 2
    ##    year n_clusters
    ##   <int>      <int>
    ## 1  2018        105
    ## 2  2019        113
    ## 3  2020        104
    ## 4  2021         99
    ## 5  2022         77
    ## 6  2023         64
    ## 7  2024         31

- Count the number of countries with representative clusters

``` r
jax_clusters %>%
  distinct(country_name)
```

    ## # A tibble: 7 × 1
    ##   country_name
    ##   <chr>       
    ## 1 Belgium     
    ## 2 Denmark     
    ## 3 France      
    ## 4 Germany     
    ## 5 Ireland     
    ## 6 Portugal    
    ## 7 Spain

- Count the number of fishing gears used to exploit the stock

``` r
jax_clusters %>%
  distinct(fishing_tech)
```

    ## # A tibble: 9 × 1
    ##   fishing_tech
    ##   <chr>       
    ## 1 DTS         
    ## 2 TBB         
    ## 3 DFN         
    ## 4 TM          
    ## 5 FPO         
    ## 6 DRB         
    ## 7 HOK         
    ## 8 PS          
    ## 9 PMP

- Count the number of subregions where the stock is exploited

``` r
jax_clusters %>%
  distinct(sub_reg)
```

    ## # A tibble: 17 × 1
    ##    sub_reg
    ##    <chr>  
    ##  1 27.4.b 
    ##  2 27.4.c 
    ##  3 27.7.d 
    ##  4 27.7.e 
    ##  5 27.7.f 
    ##  6 27.7.g 
    ##  7 27.8.a 
    ##  8 27.3.a 
    ##  9 27.4.a 
    ## 10 27.6.a 
    ## 11 27.7.b 
    ## 12 27.7.j 
    ## 13 27.8.b 
    ## 14 27.9.a 
    ## 15 27.7.c 
    ## 16 27.7.k 
    ## 17 27.8.c

#### Total landings per country

- in metric tons

``` r
jax_country_totals <- jax_clusters %>%
  group_by(country_name) %>%
  summarise(
    live_weight_country_total = round(sum(live_weight_sum, na.rm = TRUE)*0.001), #convert to metric tons / tonnes (same as TAC Regulation unit)
    .groups = "drop"
  ) %>%
  arrange(desc(live_weight_country_total))

jax_country_totals
```

    ## # A tibble: 7 × 2
    ##   country_name live_weight_country_total
    ##   <chr>                            <dbl>
    ## 1 Ireland                           1163
    ## 2 Spain                              556
    ## 3 Belgium                             25
    ## 4 Germany                              4
    ## 5 Portugal                             1
    ## 6 Denmark                              0
    ## 7 France                               0

### 3. Cluster reduction

#### Total TAC per country

- Top 5 countries are: Portugal, Spain, Netherlands, Ireland, Denmark

``` r
jax_TAC_totals <- data_TAC %>%
  filter (species_code == "JAX")  %>%
  group_by(country_name) %>%
  summarise(
    TAC_country_total = round(sum(quota_tonnes, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(desc(TAC_country_total))

jax_TAC_totals
```

    ## # A tibble: 9 × 2
    ##   country_name TAC_country_total
    ##   <chr>                    <dbl>
    ## 1 Portugal                 43544
    ## 2 Spain                    30194
    ## 3 Netherlands              19921
    ## 4 Ireland                  16422
    ## 5 Denmark                   6572
    ## 6 Germany                   4949
    ## 7 France                    2709
    ## 8 Sweden                     750
    ## 9 Belgium                      1

- Filter the top 5 countries list

``` r
top5_jax <- jax_TAC_totals %>%
  slice_max(TAC_country_total, n = 5, with_ties = FALSE) %>%  # pick top 5
  select(country_name)
top5_jax
```

    ## # A tibble: 5 × 1
    ##   country_name
    ##   <chr>       
    ## 1 Portugal    
    ## 2 Spain       
    ## 3 Netherlands 
    ## 4 Ireland     
    ## 5 Denmark

#### Clusters of Top 5 countries

- With subregion

``` r
jax_clusters_top5 <- jax_clusters %>%
  semi_join(top5_jax, by = "country_name") %>% # filter to top 5
  arrange(country_name, supra_reg, sub_reg, fishing_tech, vessel_length)
hke_clusters_top5
```

    ## # A tibble: 389 × 7
    ##    country_name supra_reg sub_reg   fishing_tech vessel_length n_rows
    ##    <chr>        <chr>     <chr>     <chr>        <chr>          <int>
    ##  1 Denmark      NAO       27.3.a.20 DFN          VL0010             1
    ##  2 Denmark      NAO       27.3.a.20 DFN          VL1012             1
    ##  3 Denmark      NAO       27.3.a.20 DFN          VL1218             1
    ##  4 Denmark      NAO       27.3.a.20 DFN          VL1824             1
    ##  5 Denmark      NAO       27.3.a.20 DRB          VL0010             1
    ##  6 Denmark      NAO       27.3.a.20 DTS          VL0010             1
    ##  7 Denmark      NAO       27.3.a.20 DTS          VL0812             1
    ##  8 Denmark      NAO       27.3.a.20 DTS          VL1012             1
    ##  9 Denmark      NAO       27.3.a.20 DTS          VL1218             1
    ## 10 Denmark      NAO       27.3.a.20 DTS          VL1824             1
    ## # ℹ 379 more rows
    ## # ℹ 1 more variable: live_weight_sum <dbl>

- Without subregion

``` r
jax_clusters_top5 %>%
  group_by(country_name, supra_reg, fishing_tech, vessel_length) %>%  # <- sub_reg removed
  summarise(
    n_rows = sum(n_rows),
    live_weight_sum = sum(live_weight_sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country_name, supra_reg,fishing_tech, vessel_length)
```

    ## # A tibble: 25 × 6
    ##    country_name supra_reg fishing_tech vessel_length n_rows live_weight_sum
    ##    <chr>        <chr>     <chr>        <chr>          <int>           <dbl>
    ##  1 Denmark      NAO       DTS          VL2440             1             15 
    ##  2 Ireland      NAO       DFN          VL0010             1             69 
    ##  3 Ireland      NAO       DFN          VL1824             2            619.
    ##  4 Ireland      NAO       DTS          VL0010             1             98 
    ##  5 Ireland      NAO       DTS          VL1218             1            786.
    ##  6 Ireland      NAO       DTS          VL1824             3           1300.
    ##  7 Ireland      NAO       DTS          VL2440             2          16405 
    ##  8 Ireland      NAO       FPO          VL1218             1             62 
    ##  9 Ireland      NAO       TM           VL2440             5         440239 
    ## 10 Ireland      NAO       TM           VL40XX             3         703011 
    ## # ℹ 15 more rows

``` r
jax_clusters_top5
```

    ## # A tibble: 47 × 7
    ##    country_name supra_reg sub_reg fishing_tech vessel_length n_rows
    ##    <chr>        <chr>     <chr>   <chr>        <chr>          <int>
    ##  1 Denmark      NAO       27.4.c  DTS          VL2440             1
    ##  2 Ireland      NAO       27.6.a  DTS          VL2440             1
    ##  3 Ireland      NAO       27.6.a  TM           VL2440             1
    ##  4 Ireland      NAO       27.6.a  TM           VL40XX             1
    ##  5 Ireland      NAO       27.7.b  DTS          VL1824             1
    ##  6 Ireland      NAO       27.7.b  TM           VL2440             1
    ##  7 Ireland      NAO       27.7.b  TM           VL40XX             1
    ##  8 Ireland      NAO       27.7.g  DFN          VL1824             1
    ##  9 Ireland      NAO       27.7.g  DTS          VL1824             1
    ## 10 Ireland      NAO       27.7.g  TM           VL2440             1
    ## # ℹ 37 more rows
    ## # ℹ 1 more variable: live_weight_sum <dbl>

## Albacore (*Thunnus alalunga*)

### 1. Filter data

Same filtering as the first one - Atlantic Mackerel

``` r
alb_clusters <- data_AER %>%
  filter(variable_name == "Live weight of landings",
         species_code == "ALB",
         supra_reg == "NAO",
         year == 2023) %>%
  group_by(country_name, supra_reg, sub_reg, fishing_tech, vessel_length) %>%
  summarise(
    n_rows = n(),
    live_weight_sum = sum(value, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(country_name, supra_reg, sub_reg, fishing_tech, vessel_length) 

alb_clusters
```

    ## # A tibble: 220 × 7
    ##    country_name supra_reg sub_reg fishing_tech vessel_length n_rows
    ##    <chr>        <chr>     <chr>   <chr>        <chr>          <int>
    ##  1 France       NAO       27.7.e  HOK          VL0010             1
    ##  2 France       NAO       27.7.h  DTS          VL1218             1
    ##  3 France       NAO       27.7.h  DTS          VL1824             1
    ##  4 France       NAO       27.7.h  DTS          VL2440             1
    ##  5 France       NAO       27.7.h  MGP          VL1824             1
    ##  6 France       NAO       27.7.h  TM           VL1218             1
    ##  7 France       NAO       27.7.h  TM           VL1824             1
    ##  8 France       NAO       27.7.h  TM           VL2440             1
    ##  9 France       NAO       27.7.j  DTS          VL1218             1
    ## 10 France       NAO       27.7.j  DTS          VL1824             1
    ## # ℹ 210 more rows
    ## # ℹ 1 more variable: live_weight_sum <dbl>

### 2. Cluster metrics

- Total number of clusters

``` r
nrow(alb_clusters)
```

    ## [1] 220

- The number of clusters per year varies

``` r
alb_clusters_per_year <- data_AER %>%
  filter(species_code == "ALB",
         supra_reg == "NAO",
         variable_name == "Live weight of landings") %>%
  group_by(year) %>%
  summarise(
    n_clusters = n_distinct(country_name, supra_reg, sub_reg, fishing_tech, vessel_length),
    .groups = "drop"
  ) %>%
  arrange(year)

alb_clusters_per_year
```

    ## # A tibble: 7 × 2
    ##    year n_clusters
    ##   <int>      <int>
    ## 1  2018        224
    ## 2  2019        198
    ## 3  2020        239
    ## 4  2021        215
    ## 5  2022        203
    ## 6  2023        220
    ## 7  2024         17

- Count the number of countries with representative clusters

``` r
alb_clusters %>%
  distinct(country_name)
```

    ## # A tibble: 4 × 1
    ##   country_name
    ##   <chr>       
    ## 1 France      
    ## 2 Ireland     
    ## 3 Portugal    
    ## 4 Spain

- Count the number of fishing gears used to exploit the stock

``` r
alb_clusters %>%
  distinct(fishing_tech)
```

    ## # A tibble: 10 × 1
    ##    fishing_tech
    ##    <chr>       
    ##  1 HOK         
    ##  2 DTS         
    ##  3 MGP         
    ##  4 TM          
    ##  5 DFN         
    ##  6 FPO         
    ##  7 PGP         
    ##  8 MGO         
    ##  9 PMP         
    ## 10 PS

- Count the number of subregions where the stock is exploited

``` r
alb_clusters %>%
  distinct(sub_reg)
```

    ## # A tibble: 21 × 1
    ##    sub_reg
    ##    <chr>  
    ##  1 27.7.e 
    ##  2 27.7.h 
    ##  3 27.7.j 
    ##  4 27.8.a 
    ##  5 27.8.b 
    ##  6 27.8.c 
    ##  7 27.8.d 
    ##  8 27.7.k 
    ##  9 27.10.a
    ## 10 27.9.a 
    ## # ℹ 11 more rows

#### Total landings per country

- in metric tons

``` r
alb_country_totals <- alb_clusters %>%
  group_by(country_name) %>%
  summarise(
    live_weight_country_total = round(sum(live_weight_sum, na.rm = TRUE)*0.001), #convert to metric tons / tonnes (same as TAC Regulation unit)
    .groups = "drop"
  ) %>%
  arrange(desc(live_weight_country_total))

alb_country_totals
```

    ## # A tibble: 4 × 2
    ##   country_name live_weight_country_total
    ##   <chr>                            <dbl>
    ## 1 Spain                            17290
    ## 2 Ireland                           3035
    ## 3 France                            2867
    ## 4 Portugal                           210

### 3. Cluster reduction

only 4 countries

- Without subregion

``` r
alb_clusters %>%
  group_by(country_name, supra_reg, fishing_tech, vessel_length) %>%  # <- sub_reg removed
  summarise(
    n_rows = sum(n_rows),
    live_weight_sum = sum(live_weight_sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(country_name, supra_reg,fishing_tech, vessel_length)
```

    ## # A tibble: 50 × 6
    ##    country_name supra_reg fishing_tech vessel_length n_rows live_weight_sum
    ##    <chr>        <chr>     <chr>        <chr>          <int>           <dbl>
    ##  1 France       NAO       DFN          VL0010             1           681. 
    ##  2 France       NAO       DFN          VL1012             2           885. 
    ##  3 France       NAO       DFN          VL1218             4         23248  
    ##  4 France       NAO       DFN          VL1824             4         10734. 
    ##  5 France       NAO       DTS          VL1012             1         18275. 
    ##  6 France       NAO       DTS          VL1218             6        192728. 
    ##  7 France       NAO       DTS          VL1824             6        797619. 
    ##  8 France       NAO       DTS          VL2440             6        534986. 
    ##  9 France       NAO       FPO          VL0010             1            36.8
    ## 10 France       NAO       FPO          VL1012             1          5961. 
    ## # ℹ 40 more rows

``` r
alb_clusters
```

    ## # A tibble: 220 × 7
    ##    country_name supra_reg sub_reg fishing_tech vessel_length n_rows
    ##    <chr>        <chr>     <chr>   <chr>        <chr>          <int>
    ##  1 France       NAO       27.7.e  HOK          VL0010             1
    ##  2 France       NAO       27.7.h  DTS          VL1218             1
    ##  3 France       NAO       27.7.h  DTS          VL1824             1
    ##  4 France       NAO       27.7.h  DTS          VL2440             1
    ##  5 France       NAO       27.7.h  MGP          VL1824             1
    ##  6 France       NAO       27.7.h  TM           VL1218             1
    ##  7 France       NAO       27.7.h  TM           VL1824             1
    ##  8 France       NAO       27.7.h  TM           VL2440             1
    ##  9 France       NAO       27.7.j  DTS          VL1218             1
    ## 10 France       NAO       27.7.j  DTS          VL1824             1
    ## # ℹ 210 more rows
    ## # ℹ 1 more variable: live_weight_sum <dbl>

## Combined database

``` r
# 1) Species metadata
species_meta <- tibble::tribble(
  ~species_code, ~common_name,         ~scientific_name,
  "MAC",         "Atlantic Mackerel",  "Scomber scombrus",
  "HKE",         "European Hake",      "Merluccius merluccius",
  "JAX",         "Horse Mackerel",     "Trachurus trachurus",
  "ALB",         "Albacore",           "Thunnus alalunga"
)

# 2) Combine all clusters + add species info in one step
all_clusters <- bind_rows(
  mac_clusters_top5 %>% mutate(species_code = "MAC"),
  hke_clusters_top5 %>% mutate(species_code = "HKE"),
  jax_clusters_top5 %>% mutate(species_code = "JAX"),
  alb_clusters %>% mutate(species_code = "ALB")
) %>%
  left_join(species_meta, by = "species_code") %>%
  select(species_code, common_name, scientific_name, everything())

all_clusters
```

    ## # A tibble: 883 × 10
    ##    species_code common_name       scientific_name country_name supra_reg sub_reg
    ##    <chr>        <chr>             <chr>           <chr>        <chr>     <chr>  
    ##  1 MAC          Atlantic Mackerel Scomber scombr… Denmark      NAO       27.3.a…
    ##  2 MAC          Atlantic Mackerel Scomber scombr… Denmark      NAO       27.3.a…
    ##  3 MAC          Atlantic Mackerel Scomber scombr… Denmark      NAO       27.3.a…
    ##  4 MAC          Atlantic Mackerel Scomber scombr… Denmark      NAO       27.3.a…
    ##  5 MAC          Atlantic Mackerel Scomber scombr… Denmark      NAO       27.3.a…
    ##  6 MAC          Atlantic Mackerel Scomber scombr… Denmark      NAO       27.3.a…
    ##  7 MAC          Atlantic Mackerel Scomber scombr… Denmark      NAO       27.3.a…
    ##  8 MAC          Atlantic Mackerel Scomber scombr… Denmark      NAO       27.3.a…
    ##  9 MAC          Atlantic Mackerel Scomber scombr… Denmark      NAO       27.3.a…
    ## 10 MAC          Atlantic Mackerel Scomber scombr… Denmark      NAO       27.3.a…
    ## # ℹ 873 more rows
    ## # ℹ 4 more variables: fishing_tech <chr>, vessel_length <chr>, n_rows <int>,
    ## #   live_weight_sum <dbl>

``` r
# 3) Export single sheet
write_xlsx(all_clusters, "Fleet_Clusters_All_Species.xlsx")
```
