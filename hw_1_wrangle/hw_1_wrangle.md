hw\_1\_wrangle.Rmd
================
Kammy Chiu
February 14, 2017

### Motivating Question

I'm interested in looking at how motor voter registration in Oregon has influenced the number of new registrations affiliated with different parties in 2016. I'm particularly interested in learning which party gained the most number of new registrations over the course of 2016. My first plot will compare such trends across parties.

In order to further gauge the impact of auto-registration on the number of registrations, I'd also like to see whether the number of auto-registrations surpassed the number of regular registrations at any point in 2016. My second plot will compare the number of auto-registrations vs regular registrations.

Note that I am limited to data from 2016 since the Motor Voter Registration Act only came into effect in Jan 2016.

``` r
#load data
vote_reg <- read_csv("http://bit.ly/2kG37yJ")
```

    ## Parsed with column specification:
    ## cols(
    ##   VOTER_ID = col_integer(),
    ##   BIRTH_DATE = col_character(),
    ##   CONFIDENTIAL = col_character(),
    ##   EFF_REGN_DATE = col_character(),
    ##   STATUS = col_character(),
    ##   PARTY_CODE = col_character(),
    ##   COUNTY = col_character()
    ## )

``` r
Or_MV <- read_csv("http://bit.ly/2lCadlB")
```

    ## Parsed with column specification:
    ## cols(
    ##   VOTER_ID = col_integer(),
    ##   DESCRIPTION = col_character(),
    ##   COUNTY = col_character()
    ## )

``` r
#clean data: filter out inactive individuals and confidential records
vote_joined <- filter(vote_reg, STATUS == "A", is.na(CONFIDENTIAL)) %>%
#join data sets
  left_join(Or_MV, by = c("VOTER_ID" = "VOTER_ID")) %>%
  #filter out duplicate rows
  group_by(VOTER_ID) %>%
    slice(1L)

#create month, year and age column
vote_joined <- vote_joined %>%
  ungroup() %>%
  mutate(reg_date = mdy(EFF_REGN_DATE)) %>%
  mutate(reg_year = year(reg_date)) %>%
  mutate(reg_month = month(reg_date, label = TRUE))

#rename motor voters in both data sets
vote_joined$description[vote_joined$DESCRIPTION == "MVPhase2"] <- "Motor Voter"
```

    ## Warning: Unknown or uninitialised column: 'description'.

``` r
vote_joined$description[vote_joined$DESCRIPTION == "Motor Voter"] <- "Motor Voter"

#create 2016 data set
vote_2016 <- select(vote_joined, VOTER_ID, reg_year, reg_month, PARTY_CODE, description) %>%
  filter(reg_year == 2016, PARTY_CODE=="DEM" | PARTY_CODE=="REP" | PARTY_CODE=="NAV" | PARTY_CODE=="IND") %>%
  group_by(reg_month, PARTY_CODE, description) %>%
  summarize(cnt = n())

MV_2016 <- filter(vote_2016, is.na(description) == FALSE)
```

``` r
#first plot
ggplot(MV_2016, aes(x = reg_month, y = cnt, col = PARTY_CODE)) +
  scale_y_log10() +
  geom_line(aes(group = PARTY_CODE)) +
  xlab("Month in 2016") +
  ylab("log (Number of Motor Voter Registrations)") +
  labs(col = "Party Affiliation") +
  ggtitle("Number of Motor Voter Registrations by Party in 2016")
```

![](hw_1_wrangle_files/figure-markdown_github/voter%20participation-1.png)

This plot clearly shows that the Oregon Motor Voter (OMV) Act increased the number of registrations the most for the non-affiliated voter (NAV) group across all months in 2016. This might have something to with the process of collecting data on party affiliation. After a motor voter registers at the DMV, the voter receives a postcard that asks for their party affiliation. If the voter does not respond to the postcard in time, the voter's party affiliation defaults on NAV. Given that there are costs (e.g. time) associated with returning the post card, voters who are in fact affiliated with a certain party may not be incentivized to declare their party affiliation, which means that they get "incorrectly" lumped into the NAV category.

The biggest winner (in receiving the most number of MV registrations) is the Democratic Party (DEM), closely followed by the Republican Party (REP). The similarity in trends between the DEM and REP lines the suggest that data on party affiliation may be collected and processed in certain months of the year.

``` r
#second plot
ggplot(vote_2016, aes(x = reg_month, y = cnt, col = description)) +
  scale_y_log10() +
  geom_line(aes(group = description)) +
  xlab("Month in 2016") +
  ylab("log (Number of Voter Registrations)") +
  labs(col = "Voter Registration Type") +
  ggtitle("Number of Voter Registrations by Party in 2016") +
  facet_wrap( ~ PARTY_CODE, ncol = 1)
```

![](hw_1_wrangle_files/figure-markdown_github/unnamed-chunk-1-1.png)

The number of non-MV registrations is consistently higher than the number of MV registrations across all parties, except for the NAV group. It's interesting that the trends in the number of MV registrations for Jun-Dec follow that of the number of non-MV registrations closely across all three parties. That might have to do with campaigning efforts leading up to the presidential election that promoted MV-registrations, or it might again have to do with the timing of which data on party affiliation are collected and processed.

### Identifying Types of Variables

First Plot:

1.  Month in 2016 (categorical)

2.  Number of Motor Voter Registrations (numerical - log-transformed)

3.  Party Affiliation (categorical)

Second Plot:

1.  Month in 2016 (categorical)

2.  Number of Motor Voter Registrations (numerical - log-transformed)

3.  Party Affiliation (categorical)

4.  Type of registration - Motor Voter vs Non-motor voter (categorical)

### Explaining Visual Cues

Position: I decided to map the months of 2016 on to x-values for both plots because time is usually displayed on the x-axis in a time-series plot. I mapped the number of motor voter registrations on to y-values, then log-transformed the y-values such that a large range can be displayed without small values being compressed at the bottom of the graphs. Although we lose resolution on the actual counts of motor voter registrations, the log transformation allows us to compare line plots across parties and find out who gained the most comparatively.

Note: An increase in 1 tick mark in the y-direction denotes an increase in 1 order of magnitude of vote registrations.

Color: Mapping party affiliation on to color in my first plot allows me to distinguish and compare trends across different parties in the same plot. Mapping registration type on to color allows me to compare the number of auto-registrations with the number of regular registrations at different points in 2016.

Small multiples: For my second plot, I faceted the plots according to party affiliation to compare trends between registration type within each party.

### Geometry

I used line plots instead of scatter plots because lines allow for easier comparisons across party affiliations in my first plot, and across voter types in my second plot. The line plot uses two visual cues, position and direction. Position is the easiest visual cue for people to register. Direction allows people to eyeball the relationship between the two variables. I adhered to Tufte's rule of matching the dimensions of the visual cues to that of my variables.

### Additional Tweaks

1.  I added a title, axis labels and a label for the color key to explain the mappings between variables and visual cues for each plot. Ideally, I would like to have the labels under Voter Registration Type specified as well.

2.  I decided to plot the total number of motor voter registrations per month against time, instead of total number of motor voter registrations per day against time to avoid over-plotting.

3.  For the second plot, I stacked the four plots on top of on another in a single column so that the axes are readable on a page.
