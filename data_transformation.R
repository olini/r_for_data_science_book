library(nycflights13)
library(tidyverse)

# comparison approximation
sqrt(2)^2 == 2
near(sqrt(2)^2, 2)

# dplyr filter
filter(flights, month==1, day==1)
jan1 <- filter(flights, month==1, day==1)
(dec25 <- filter(flights, month==12, day==25))

filter(flights, month==11 | month==12)
nov_dec <- filter(flights, month %in% c(11, 12))

## NAs
df <- tibble(x=c(1, NA, 3))
filter(df, x>1)
filter(df, x>1 | is.na(x))

# 5.2.4 Exercises
# Find all flights that
## Had an arrival delay of two or more hours
filter(flights, arr_delay >= 120)
## Flew to Houston (IAH or HOU)
filter(flights, dest %in% c('IAH', 'HOU'))
## Were operated by United, American, or Delta
filter(flights, carrier %in% c('AA', 'DL', 'UA'))
## Departed in summer (July, August, and September)
filter(flights, month %in% 7:9)
filter(flights, between(month, 7, 9))
## Arrived more than two hours late, but didn’t leave late
filter(flights, arr_delay > 120, dep_delay <= 0)
## Were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, dep_delay >= 60, dep_delay - arr_delay > 30)
## Departed between midnight and 6am (inclusive)
summary(flights$dep_time)
filter(flights, dep_time <= 600 | dep_time == 2400)

# How many flights have a missing dep_time? What other variables are missing? 
# What might these rows represent?
filter(flights, is.na(dep_time))
summary(flights)

NA^0
NA | TRUE
FALSE & NA
NA * 0
Inf * 0

# dplyr arrange
# 5.3.1 Exercises
# How could you use arrange() to sort all missing values to the start? 
# (Hint: use is.na())
arrange(flights, dep_time) %>% tail() # gets tail of tibble
arrange(flights, desc(is.na(dep_time)), dep_time)

# Sort flights to find the most delayed flights. 
arrange(flights, desc(dep_delay))
# Find the flights that left earliest.
arrange(flights, dep_delay)

# Sort flights to find the fastest (highest speed) flights.
flights$distance / flights$air_time
arrange(flights, desc(distance / air_time))

# Which flights travelled the farthest? Which travelled the shortest?
arrange(flights, desc(distance))
arrange(flights, distance)


# dplyr select
select(flights, year, month, day)
select(flights, distance, air_time, distance/air_time)
select(flights, year:day)
select(flights, -(year:day))
select(flights, -c(year, month, day))

rename(flights, tail_num = tailnum)

select(flights, time_hour, air_time, everything())

select(flights, year, year, month, day)
select(flights, any_of(c('year', 'month', 'day')))

select(flights, contains('TIME'))
select(flights, contains('TIME', ignore.case=FALSE))

# dplyr mutate
flights_small <- select(flights,
                        year:day,
                        ends_with('delay'),
                        distance,
                        air_time)
mutate(flights_small,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60)

mutate(flights_small,
       gain = dep_delay - arr_delay,
       hours = air_time/60,
       gain_per_hour = gain/hours)

# transmute keeps only the new columns created
transmute(flights_small,
          gain = dep_delay - arr_delay,
          hours = air_time/60,
          gain_per_hour = gain / hours)

transmute(flights,
          distance,
          sum(distance),
          mean(distance),
          distance / sum(distance),
          distance - mean(distance))

transmute(flights,
          dep_time,
          hour = dep_time %/% 100, # integer division
          minute = dep_time %% 100 # remainder
)

# 5.5.2 Exercises
# Currently dep_time and sched_dep_time are convenient to look at, but hard to 
# compute with because they’re not really continuous numbers. Convert them to 
# a more convenient representation of number of minutes since midnight.
transmute(flights,
          dep_time,
          hours = dep_time %/% 100,
          minutes = dep_time %% 100,
          total_min = (minutes + (hours*60)) %% 1440 # remainder of 1440 to
                                                     # solve midnight as 2400
          )

# dplyr summarise
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

# example analysis without pipe
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dest != 'HNL')
ggplot(delay, mapping = aes(x = dist, y = delay)) +
        geom_point(aes(size = count), alpha = 1/3) +
        geom_smooth(se = FALSE)

# example analysis with pipe
delays <- flights %>%
        group_by(dest) %>%
        summarise(
                count = n(),
                dist = mean(distance, na.rm = TRUE),
                delay = mean(arr_delay, na.rm = TRUE)
        ) %>%
        filter(count > 20, dest != 'HNL')
ggplot(delays, mapping = aes(x = dist, y = delay)) +
        geom_point(aes(size = count), alpha = 1/3) +
        geom_smooth(se = FALSE)


# if we dont use the paramter na.rm = FALSE, NA values are considered
flights %>%
        group_by(year, month, day) %>%
        summarise(mean = mean(dep_delay))

# another way of solving this besides na.rm is excluding the data before
not_cancelled <- flights %>%
        filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>%
        group_by(year, month, day) %>%
        summarise(mean = mean(dep_delay))

delays <- not_cancelled %>%
        group_by(tailnum) %>%
        summarise(
                delay = mean(arr_delay)
        )

ggplot(delays, mapping = aes(x=delay)) +
        geom_freqpoly(binwidth=10)

delays <- not_cancelled %>%
        group_by(tailnum) %>%
        summarise(
                delay = mean(arr_delay),
                n = n()
        )
ggplot(delays, mapping = aes(x = n, y = delay)) +
        geom_point(alpha = 1/10)

delays %>%
        filter(n > 25) %>%
        ggplot(mapping = aes(x = n, y = delay)) +
        geom_point(alpha = 1/10)


batting <- as_tibble(Lahman::Batting)
batters <- batting %>%
        group_by(playerID) %>%
        summarise(
                ba = sum(H, na.rm = TRUE)/ sum(AB, na.rm = TRUE),
                ab = sum(AB, na.rm = TRUE)
        )
batters %>%
        filter(ab > 100) %>%
        ggplot(mapping = aes(x = ab, y = ba)) +
        geom_point(alpha = 1/5) +
        geom_smooth(se = FALSE)

batters %>%
        arrange(desc(ba))

not_cancelled %>%
        group_by(year, month, day) %>%
        summarise(
                avg_delay1 = mean(arr_delay),
                avg_delay2 = mean(arr_delay[arr_delay > 0])
        )

not_cancelled %>%
        group_by(dest) %>%
        summarise(distance_sd = sd(distance)) %>%
        arrange(desc(distance_sd))

not_cancelled %>%
        group_by(year, month, day) %>%
        summarise(
                first = min(dep_time),
                last = max(dep_time),
                first_q = quantile(dep_time, 0.25),
                second_q = quantile(dep_time, 0.5),
                third_q = quantile(dep_time, 0.75),
                median = median(dep_time),
                iqr = IQR(dep_time),
                aux = third_q - first_q
        )

not_cancelled %>%
        group_by(year, month, day) %>%
        summarise(
                first_dep = first(dep_time),
                last_dep = last(dep_time)
        )

not_cancelled %>%
        group_by(year, month, day) %>%
        transmute(dep_time, r = min_rank(desc(dep_time))) %>%
        filter(r %in% range(r))

not_cancelled %>%
        group_by(dest) %>%
        summarise(carriers = n_distinct(carrier)) %>%
        arrange(desc(carriers))

not_cancelled %>%
        count(dest)

# How many flights left before 5am? (these usually indicate delayed
# flights from the previous day)
not_cancelled %>%
        group_by(year, month, day) %>%
        summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>%
        group_by(year, month, day) %>%
        summarise(hour_prop = mean(arr_delay > 60))

daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year <- summarise(per_month, flights = sum(flights)))

daily %>%
        ungroup() %>%
        summarise(flights = n())

# 5.6.7. Exercises
# 2. Come up with another approach that will give you the same output as 
# not_cancelled %>% count(dest) and 
# not_cancelled %>% count(tailnum, wt = distance) (without using count()).
not_cancelled %>%
        group_by(dest) %>%
        summarise(n = n())

# 4. Look at the number of cancelled flights per day. Is there a pattern? 
# Is the proportion of cancelled flights related to the average delay?
flights %>%
        mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>%
        group_by(year, month, day) %>%
        summarise(cancelled = sum(cancelled), total_flights = n()) %>%
        ggplot(mapping = aes(x = total_flights, y = cancelled)) +
        geom_point()

flights %>%
        mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>%
        group_by(year, month, day) %>%
        summarise(
                cancelled = mean(cancelled), 
                avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
        ggplot(mapping = aes(x = avg_dep_delay, y = cancelled)) +
        geom_point()

# 5. Which carrier has the worst delays? Challenge: can you disentangle 
# the effects of bad airports vs. bad carriers? Why/why not? 
# (Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n()))
flights %>%
  group_by(carrier) %>%
  summarise(max = max(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(max))


not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(arr_delay, rank = rank(arr_delay)) %>%
  arrange(year, month, day, desc(rank))

not_cancelled %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

popular_dest <- flights %>%
  group_by(dest) %>%
  filter(n() > 365)
popular_dest
