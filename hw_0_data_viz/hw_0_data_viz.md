Visualizing Oregon Vote History
================
Kammy Chiu
Feb 9, 2017

``` r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
#load data
vote_particip <- read_csv("http://bit.ly/2l1bIxs")
```

    ## Parsed with column specification:
    ## cols(
    ##   VOTER_ID = col_integer(),
    ##   FIRST_NAME = col_character(),
    ##   LAST_NAME = col_character(),
    ##   BIRTH_DATE = col_date(format = ""),
    ##   PARTY_CODE = col_character(),
    ##   COUNTY = col_character(),
    ##   p_vote = col_double()
    ## )

### Motivating Question

From Pol260, I learnt that Tea Partiers, typically retired citizens, provide the Republican Party with a consistent voter-base across elections of different scales. I'm interested in seeing whether we can identify consistent Oregon voters, i.e. voters with a high participation rate, by their age within subsets of different political affiliations. I am particularly interested in such relationships within non-affiliated voters and voters affiliated with the Democratic, Republican, or Independent Party. I suspect that older voters affiliated with the Republican Party would have higher participation rates compared to other three groups.

``` r
#tidy data
vote_particip <- vote_particip %>% mutate(birth_year =lubridate::year(vote_particip$BIRTH_DATE)) %>%
  mutate(age = 2017-birth_year)

#create first plot (within-trends)
vote_particip %>% 
  filter(PARTY_CODE=="DEM" | PARTY_CODE=="REP" | PARTY_CODE=="NAV" | PARTY_CODE=="IND") %>%
  group_by(age, PARTY_CODE) %>%
  summarize(med_p = median(p_vote, na.rm = TRUE),
            cnt = n()) %>%
  ggplot(aes(x = age, y = med_p, col = cnt)) +
  geom_point() +
  xlab("Voter Age") +
  ylab("Median Voter Participation") +
  labs(col = "Count of Voter Type(*) in Sample (Number of Voters)") +
  ggtitle("Voter Participation vs Voter Birth Year") +
  facet_wrap( ~ PARTY_CODE, ncol = 1)
```

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](hw_0_data_viz_files/figure-markdown_github/voter%20participation-1.png)

``` r
#create second plot (across-trends)
vote_particip %>% 
  filter(PARTY_CODE=="DEM" | PARTY_CODE=="REP" | PARTY_CODE=="NAV" | PARTY_CODE=="IND") %>%
  group_by(age, PARTY_CODE) %>%
  summarize(med_p = median(p_vote, na.rm = TRUE),
            cnt = n()) %>%
  ggplot(aes(x = age, y = med_p, col = PARTY_CODE)) +
  geom_point() +
  xlab("Voter Age") +
  ylab("Median Voter Participation") +
  labs(col = "Count of Voter Type(*) in Sample (Number of Voters)") +
  ggtitle("Voter Participation vs Voter Birth Year")
```

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](hw_0_data_viz_files/figure-markdown_github/voter%20participation-2.png)

### Variables

1.  Age (categorical)

2.  Median Voter Participation Rate (numerical)

3.  Party Affiliation (categorical)

4.  Count of each voter type in the sample (numeric). (\*) Voter types are subsets of the sample that contain voters with a particular combination of birth date and party affiliation. For example, voters affiliated with the Republican Party born in 1950 would be counted under the same voter type.

### Visual Cues

Position: I decided to map Birth Year on to x-values because explanatory variables are usually in the x-axis by convention. Similarly, I decided to map Median Voter Participation on to y-values because dependent variables are usually in the y-axis by convention.

Small multiples: I decided to facet the plots by Party Affiliation so that I can compare voter participation across age groups within each party.

Color: After noticing that my plots for smaller third-parties contained more erratic points, I decided to map the count of each voter type to color to inform the viewer of the representativeness of the sample. The lighter-colored points in the plots suggest that young voters who are either non-affiliated voters or Democratic voters were more frequently sampled than the rest of the population.

### Geometry

I decided to use points for my scatter plot because I'm interested in looking at the relationship between two variables, birth year and the median vote participation. The scatter plot uses two visual cues, position and direction. Position is the easiest visual cue for people to register. Direction allows people to eyeball the relationship between the two variables. As discussed in the previous section on visual cues, I adhered to Tufte's rule of matching the dimensions of the visual cues to that of my variables.

### Additional Tweaks

1.  I added a title, axis labels and a label for the saturation key to explain the mappings between variables and visual cues. I added an asterisk next to Voter Type to explain what I mean in the text below. Ideally that information would be in a legend under the graph, but I haven't figured out how to do that.

2.  I decided to plot the median statistic for voter participation rates against birth years instead of voter participation rates against birth dates to avoid over-plotting.

3.  I stacked the four plots on top of on another in a single column so that the x-axis wouldn't be squished.

### Results

From my graphic, I can see that there is a positive correlation between voter age and the median voter participation across all four party affiliations. That is, the older a voter, the more likely they are to vote. Although I hypothesized earlier that older voters affiliated with the Republican Party would have higher participation rates compared to other three groups, what's immediately observable from these plots is that non-affiliated voters born after 1985 have a median voter participation rate of close to 0, considerably lower than the voter participation rates in the other three plots within the same birth-year range. Non-affiliated voters across all birth-years also appear to have a lower median voter participation rate than the other groups. Mapping party affiliation on to color and layering these four plots on top of each other would make comparisons across groups easier. However, faceting allowed me to map the count of each voter type on to color, which revealed that Democrats born after year 1945 and non-affiliated voters born after 1975 are much more prevalent within the sample. Moreover, seeing the count of each voter type informed my decision in kicking out third-party plots with more erratic median voter participation values across age groups, as our sample is not big enough to provide reliable median values.

One improvement that can be easily made is to map voter age instead of voter birth year on to x-positions, as it is more intuitive for people to perceive an increasing age as we move to the right of the x-axis. It would also be interesting to see a regression with party affiliation and age as explanatory variables and voter participation as the dependent variable to identify which voter type votes the most consistently.
