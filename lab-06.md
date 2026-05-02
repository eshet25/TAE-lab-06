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
