Lab 06 - Ugly charts and Simpson’s paradox
================
Tsion
02/26/26

### Load packages and data

``` r
library(tidyverse) 
library(mosaicData) 
```

### Instructional staff employment trends

``` r
staff <- read_csv("data/instructional-staff.csv")
```

    ## Rows: 5 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): faculty_type
    ## dbl (11): 1975, 1989, 1993, 1995, 1999, 2001, 2003, 2005, 2007, 2009, 2011
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
glimpse(staff)
```

    ## Rows: 5
    ## Columns: 12
    ## $ faculty_type <chr> "Full-Time Tenured Faculty", "Full-Time Tenure-Track Facu…
    ## $ `1975`       <dbl> 29.0, 16.1, 10.3, 24.0, 20.5
    ## $ `1989`       <dbl> 27.6, 11.4, 14.1, 30.4, 16.5
    ## $ `1993`       <dbl> 25.0, 10.2, 13.6, 33.1, 18.1
    ## $ `1995`       <dbl> 24.8, 9.6, 13.6, 33.2, 18.8
    ## $ `1999`       <dbl> 21.8, 8.9, 15.2, 35.5, 18.7
    ## $ `2001`       <dbl> 20.3, 9.2, 15.5, 36.0, 19.0
    ## $ `2003`       <dbl> 19.3, 8.8, 15.0, 37.0, 20.0
    ## $ `2005`       <dbl> 17.8, 8.2, 14.8, 39.3, 19.9
    ## $ `2007`       <dbl> 17.2, 8.0, 14.9, 40.5, 19.5
    ## $ `2009`       <dbl> 16.8, 7.6, 15.1, 41.1, 19.4
    ## $ `2011`       <dbl> 16.7, 7.4, 15.4, 41.3, 19.3

Each row is a faculty type, and each year is stored in a separate
column.

### Exercise 1

``` r
staff_long <- staff %>%
  pivot_longer(cols = -faculty_type, names_to = "year") %>%
  mutate(value = as.numeric(value))
staff_long
```

    ## # A tibble: 55 × 3
    ##    faculty_type              year  value
    ##    <chr>                     <chr> <dbl>
    ##  1 Full-Time Tenured Faculty 1975   29  
    ##  2 Full-Time Tenured Faculty 1989   27.6
    ##  3 Full-Time Tenured Faculty 1993   25  
    ##  4 Full-Time Tenured Faculty 1995   24.8
    ##  5 Full-Time Tenured Faculty 1999   21.8
    ##  6 Full-Time Tenured Faculty 2001   20.3
    ##  7 Full-Time Tenured Faculty 2003   19.3
    ##  8 Full-Time Tenured Faculty 2005   17.8
    ##  9 Full-Time Tenured Faculty 2007   17.2
    ## 10 Full-Time Tenured Faculty 2009   16.8
    ## # ℹ 45 more rows

``` r
staff_long %>%
  ggplot(aes(
    x = year,
    y = value,
    group = faculty_type,
    color = faculty_type
  )) +
  geom_line(linewidth = 1) +
  labs(
    title = "Instructional Staff Employment Trends",
    x = "Year",
    y = "Percentage of instructional staff",
    color = "Faculty type"
  ) +
  theme_minimal()
```

![](lab-06_files/figure-gfm/Make%20a%20basic%20line%20graph%20of%20faculty%20type%20over%20time-1.png)<!-- -->

The line plot shows how the percentage of different types of
instructional staff changed over time. The improved plot is easier to
understand than the original because the years are on the x-axis,
percentages are on the y-axis, and each faculty type is shown with a
separate colored line.

### Exercise 2

In my opinion, to make the story clearer, I would emphasize Part-Time
Faculty because this group appears to increase over time compared with
some other faculty types. I would use a stronger color or thicker line
for Part-Time Faculty and make the other lines more muted? I think this
would help the viewer to focus more on the main trend instead of trying
to compare every line equally.

``` r
staff_long %>%
  mutate(
    highlight = if_else(faculty_type == "Part-Time Faculty", 
                        "Part-Time Faculty", 
                        "Other faculty types")
  ) %>%
  ggplot(aes(
    x = year,
    y = value,
    group = faculty_type
  )) +
  geom_line(aes(color = highlight, linewidth = highlight)) +
  scale_linewidth_manual(values = c("Part-Time Faculty" = 1.4,
                                    "Other faculty types" = 0.6)) +
  labs(
    title = "Part-Time Faculty Became a Larger Share of Instructional Staff",
    subtitle = "Instructional staff employment trends from 1975 to 2011",
    x = "Year",
    y = "Percentage of instructional staff",
    color = "Group",
    linewidth = "Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )
```

![](lab-06_files/figure-gfm/Improve%20the%20plot%20by%20highlighting%20Part-Time%20Faculty-1.png)<!-- -->

### Fisheries

### Exercise 3

The original fisheries plots are a bit difficult to understand as they
stand. The 3D pie charts are hard to compare because the
three-dimensional effect distorts the size of the slices. There are alos
too many countries shown at once, which makes the visualization
cluttered. To improve the visualization, I decided to use a horizontal
bar chart. I will only show the top countries by total fisheries
production so the plot is not overcrowded.

``` r
fisheries <- read_csv("data/fisheries.csv")
```

    ## Rows: 216 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): country
    ## dbl (3): capture, aquaculture, total
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
top_countries <- fisheries %>%
  slice_max(total, n = 20) %>%
  pull(country)
```

``` r
fisheries_long <- fisheries %>%
  filter(country %in% top_countries) %>%
  pivot_longer(
    cols = c(capture, aquaculture),
    names_to = "type",
    values_to = "tons"
  ) %>%
  mutate(
    type = recode(type,
                  "capture" = "Capture fishing",
                  "aquaculture" = "Aquaculture")
  )

fisheries_long
```

    ## # A tibble: 40 × 4
    ##    country       total type                tons
    ##    <chr>         <dbl> <chr>              <dbl>
    ##  1 Bangladesh  3878324 Capture fishing  1674770
    ##  2 Bangladesh  3878324 Aquaculture      2203554
    ##  3 Brazil      1286230 Capture fishing   705000
    ##  4 Brazil      1286230 Aquaculture       581230
    ##  5 Chile       2879355 Capture fishing  1829238
    ##  6 Chile       2879355 Aquaculture      1050117
    ##  7 China      81500000 Capture fishing 17800000
    ##  8 China      81500000 Aquaculture     63700000
    ##  9 Egypt       1706274 Capture fishing   335614
    ## 10 Egypt       1706274 Aquaculture      1370660
    ## # ℹ 30 more rows

``` r
fisheries_long %>%
  ggplot(aes(
    x = tons,
    y = fct_reorder(country, tons, .fun = sum),
    fill = type
  )) +
  geom_col() +
  labs(
    title = "Top 20 Countries by Fisheries Production",
    subtitle = "Production is separated into capture fishing and aquaculture",
    x = "Production in tons",
    y = "Country",
    fill = "Production type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )
```

![](lab-06_files/figure-gfm/Make%20an%20improved%20fisheries%20bar%20chart%20visualization-1.png)<!-- -->

I think this improved plot is easier to read because it avoids 3D pie
charts, limits the number of countries, and uses a horizontal layout so
country names are readable. It also makes the distinction between
capture fishing and aquaculture clearer.

### Stretch Yourself with Smokers in Whickham

``` r
library(tidyverse)
library(mosaicData)
data(Whickham)
head(Whickham)
```

    ##   outcome smoker age
    ## 1   Alive    Yes  23
    ## 2   Alive    Yes  18
    ## 3    Dead    Yes  71
    ## 4   Alive     No  67
    ## 5   Alive     No  64
    ## 6   Alive    Yes  38

``` r
glimpse(Whickham)
```

    ## Rows: 1,314
    ## Columns: 3
    ## $ outcome <fct> Alive, Alive, Dead, Alive, Alive, Alive, Alive, Dead, Alive, A…
    ## $ smoker  <fct> Yes, Yes, Yes, No, No, Yes, Yes, No, No, No, No, Yes, No, Yes,…
    ## $ age     <int> 23, 18, 71, 67, 64, 38, 45, 76, 28, 27, 28, 34, 20, 72, 48, 45…

### Exercise 4

These data come from an observational study. Participants were not
randomly assigned to smoke or not smoke. Instead, their smoking status
was observed, and their health outcome was recorded later.

### Exercise 5

``` r
nrow(Whickham)
```

    ## [1] 1314

There are 1314 observations in the dataset. Each observation represents
one participant in the Whickham study.

### Exercise 6

``` r
glimpse(Whickham)
```

    ## Rows: 1,314
    ## Columns: 3
    ## $ outcome <fct> Alive, Alive, Dead, Alive, Alive, Alive, Alive, Dead, Alive, A…
    ## $ smoker  <fct> Yes, Yes, Yes, No, No, Yes, Yes, No, No, No, No, Yes, No, Yes,…
    ## $ age     <int> 23, 18, 71, 67, 64, 38, 45, 76, 28, 27, 28, 34, 20, 72, 48, 45…

There are three variables in the dataset. ‘Outcome’, categorical
variable showing whether the participant was alive or dead; ‘smoker’,
categorical variable showing whether the participant smoked; and ‘age’,
quantitative variable showing the participant’s age.

``` r
# Visualize health outcome
ggplot(Whickham, aes(x = outcome, fill = outcome)) +
  geom_bar() +
  labs(
    title = "Distribution of Health Outcomes",
    x = "Outcome",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

![](lab-06_files/figure-gfm/visualize%20each%20outcome-1.png)<!-- -->

``` r
# Visualize smoking status
ggplot(Whickham, aes(x = smoker, fill = smoker)) +
  geom_bar() +
  labs(
    title = "Distribution of Smoking Status",
    x = "Smoker",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

![](lab-06_files/figure-gfm/visualize%20each%20outcome-2.png)<!-- -->

``` r
# Visualize age
ggplot(Whickham, aes(x = age)) +
  geom_histogram(binwidth = 5, color = "white") +
  labs(
    title = "Distribution of Age",
    x = "Age",
    y = "Count"
  ) +
  theme_minimal()
```

![](lab-06_files/figure-gfm/visualize%20each%20outcome-3.png)<!-- -->

### Exercise 7

I would expect smokers to have worse health outcomes than non-smokers. I
think a higher proportion of smokers to be dead compared with
non-smokers.

### Exercise 8

``` r
Whickham %>%
  count(smoker, outcome)
```

    ##   smoker outcome   n
    ## 1     No   Alive 502
    ## 2     No    Dead 230
    ## 3    Yes   Alive 443
    ## 4    Yes    Dead 139

``` r
Whickham %>%
  count(smoker, outcome) %>%
  group_by(smoker) %>%
  mutate(prop = n / sum(n))
```

    ## # A tibble: 4 × 4
    ## # Groups:   smoker [2]
    ##   smoker outcome     n  prop
    ##   <fct>  <fct>   <int> <dbl>
    ## 1 No     Alive     502 0.686
    ## 2 No     Dead      230 0.314
    ## 3 Yes    Alive     443 0.761
    ## 4 Yes    Dead      139 0.239

``` r
Whickham %>%
  count(smoker, outcome) %>%
  group_by(smoker) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(
    x = smoker,
    y = prop,
    fill = outcome
  )) +
  geom_col(position = "fill") +
  labs(
    title = "Health Outcome by Smoking Status",
    x = "Smoking status",
    y = "Proportion",
    fill = "Outcome"
  ) +
  theme_minimal()
```

![](lab-06_files/figure-gfm/Visualize%20health%20outcome%20by%20smoking%20status-1.png)<!-- -->

At first, the plot may make it look like smokers have better outcomes
than non-smokers. This is surprising because I expected smokers to have
worse health outcomes. I think there may be another variable affecting
the relationship.

### Exercise 9

``` r
Whickham <- Whickham %>%
  mutate(
    age_cat = case_when(
      age <= 44 ~ "18-44",
      age > 44 & age <= 64 ~ "45-64",
      age > 64 ~ "65+"
    )
  )

# Check the new variable
Whickham %>%
  count(age_cat)
```

    ##   age_cat   n
    ## 1   18-44 624
    ## 2   45-64 447
    ## 3     65+ 243

### Exercse 10

``` r
Whickham %>%
  count(smoker, age_cat, outcome)
```

    ##    smoker age_cat outcome   n
    ## 1      No   18-44   Alive 327
    ## 2      No   18-44    Dead  12
    ## 3      No   45-64   Alive 147
    ## 4      No   45-64    Dead  53
    ## 5      No     65+   Alive  28
    ## 6      No     65+    Dead 165
    ## 7     Yes   18-44   Alive 270
    ## 8     Yes   18-44    Dead  15
    ## 9     Yes   45-64   Alive 167
    ## 10    Yes   45-64    Dead  80
    ## 11    Yes     65+   Alive   6
    ## 12    Yes     65+    Dead  44

``` r
Whickham %>%
  count(smoker, age_cat, outcome) %>%
  group_by(smoker, age_cat) %>%
  mutate(prop = n / sum(n))
```

    ## # A tibble: 12 × 5
    ## # Groups:   smoker, age_cat [6]
    ##    smoker age_cat outcome     n   prop
    ##    <fct>  <chr>   <fct>   <int>  <dbl>
    ##  1 No     18-44   Alive     327 0.965 
    ##  2 No     18-44   Dead       12 0.0354
    ##  3 No     45-64   Alive     147 0.735 
    ##  4 No     45-64   Dead       53 0.265 
    ##  5 No     65+     Alive      28 0.145 
    ##  6 No     65+     Dead      165 0.855 
    ##  7 Yes    18-44   Alive     270 0.947 
    ##  8 Yes    18-44   Dead       15 0.0526
    ##  9 Yes    45-64   Alive     167 0.676 
    ## 10 Yes    45-64   Dead       80 0.324 
    ## 11 Yes    65+     Alive       6 0.12  
    ## 12 Yes    65+     Dead       44 0.88

``` r
Whickham %>%
  count(smoker, age_cat, outcome) %>%
  group_by(smoker, age_cat) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(
    x = smoker,
    y = prop,
    fill = outcome
  )) +
  geom_col(position = "fill") +
  facet_wrap(~ age_cat) +
  labs(
    title = "Health Outcome by Smoking Status and Age Group",
    x = "Smoking status",
    y = "Proportion",
    fill = "Outcome"
  ) +
  theme_minimal()
```

![](lab-06_files/figure-gfm/Recreate%20the%20smoking%20and%20outcome%20visualization,%20faceted%20by%20age%20category-1.png)<!-- -->

### Challenge Graph

``` r
# Prepare data for a challenge visualization
whickham_plot_data <- Whickham %>%
  count(age_cat, smoker, outcome) %>%
  group_by(age_cat, smoker) %>%
  mutate(prop = n / sum(n))

# Create a faceted proportion plot by age group
ggplot(whickham_plot_data, aes(
  x = smoker,
  y = prop,
  fill = outcome
)) +
  geom_col(position = "fill") +
  facet_wrap(~ age_cat) +
  labs(
    title = "Smoking and Health Outcomes Differ Across Age Groups",
    subtitle = "Age explains why the overall relationship is misleading",
    x = "Smoking status",
    y = "Proportion",
    fill = "Outcome"
  ) +
  theme_minimal()
```

![](lab-06_files/figure-gfm/try%20to%20recreate%20challenge%20graph-1.png)<!-- -->
