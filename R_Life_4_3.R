
# Week 4 -- INTRODUCING DPLYR 

# Dplyr is a powerful art package that transforms and summarizes tabular data with rows and columns.
library(dplyr)

msleep <- read.csv("msleep_ggplot2.csv")
head(msleep)

# The two most basic functions are select() and filter() which selects columns and filters rows, respectively.
# Select a set of columns: the name and the sleep_total columns.
sleepData <- select(msleep, name, sleep_total)
head(sleepData)

# To select all the columns except a specific column, use the "-" (subtraction) operator (also known as negative indexing)
head(select(msleep, -name))

# To select a range of columns by name, use the ":" (colon) operator
head(select(msleep, name:order))

# To select all columns that start with the character string "sl", use the function starts_with()
head(select(msleep, starts_with("sl")))

# Some additional options to select columns based on a specific criteria include

ends_with() = Select columns that end with a character string
contains() = Select columns that contain a character string
matches() = Select columns that match a regular expression
one_of() = Select columns names that are from a group of names

# Filter the rows for mammals that sleep a total of more than 16 hours.

filter(msleep, sleep_total >= 16)

# Filter the rows for mammals that sleep a total of more than 16 hours and have a body weight of greater than 1 kilogram.

filter(msleep, sleep_total >= 16, bodywt >= 1)

# Pipe operator: %>%
# dplyr imports this operator from another package (magrittr). This operator allows you to pipe the output from one function
# to the input of another function. Instead of nesting functions (reading from the inside to the outside), the idea of of
# piping is to read the functions from left to right.

# Here's an example you have seen:

head(select(msleep, name, sleep_total))
# Now in this case, we will pipe the msleep data frame to the function that will select two columns (name and sleep_total)
# and then pipe the new data frame to the function head() which will return the head of the new data frame.

msleep %>% 
    select(name, sleep_total) %>% 
    head
# You will soon see how useful the pipe operator is when we start to combine many functions.

###

# To arrange (or re-order) rows by a particular column such as the taxonomic order, list the name of the column you want to
# arrange the rows by

msleep %>% arrange(order) %>% head

# Now, we will select three columns from msleep, arrange the rows by the taxonomic order and then arrange the rows by
# sleep_total. Finally show the head of the final data frame

msleep %>% 
    select(name, order, sleep_total) %>%
    arrange(order, sleep_total) %>% 
    head

# Same as above, except here we filter the rows for mammals that sleep for 16 or more hours instead of showing the head of
# the final data frame

msleep %>% 
    select(name, order, sleep_total) %>%
    arrange(order, sleep_total) %>% 
    filter(sleep_total >= 16)

# Something slightly more complicated: same as above, except arrange the rows in the sleep_total column in a descending 
# order. For this, use the function desc()

msleep %>% 
    select(name, order, sleep_total) %>%
    arrange(order, desc(sleep_total)) %>% 
    filter(sleep_total >= 16)

# The mutate() function will add new columns to the data frame. Create a new column called rem_proportion which is the ratio
# of rem sleep to total amount of sleep.

msleep %>% 
    mutate(rem_proportion = sleep_rem / sleep_total) %>%
    head

# You can create many new columns using mutate (separated by commas). Here we add a second column called bodywt_grams which
# is the bodywt column in grams.

msleep %>% 
    mutate(rem_proportion = sleep_rem / sleep_total, 
           bodywt_grams = bodywt * 1000) %>%
    head

# The summarise() function will create summary statistics for a given column in the data frame such as finding the mean. For
# example, to compute the average number of hours of sleep, apply the mean() function to the column sleep_total and call the
# summary value avg_sleep.

msleep %>% 
    summarise(avg_sleep = mean(sleep_total))


# There are many other summary statistics you could consider such sd(), min(), max(), median(), sum(), n() (returns the
# length of vector), first() (returns first value in vector), last() (returns last value in vector) and n_distinct() (number
# of distinct values in vector).

msleep %>% 
    summarise(avg_sleep = mean(sleep_total), 
              min_sleep = min(sleep_total),
              max_sleep = max(sleep_total),
              total = n())

# The group_by() verb is an important function in dplyr. As we mentioned before it's related to concept of "split-apply
# -combine". We literally want to split the data frame by some variable (e.g. taxonomic order), apply a function to the
# individual data frames and then combine the output.

# Inside the function group_by, we tell it what column to group the data frames by or split the data frames by. And here,
# we're splitting by the taxonomic order.
msleep %>% 
    group_by(order) %>%
    summarise(avg_sleep = mean(sleep_total), 
              min_sleep = min(sleep_total), 
              max_sleep = max(sleep_total),
              total = n())

#########

# Using dplyr and the pipe command %>%, and perform the following steps:
msleep <- read.csv("msleep_ggplot2.csv")
names(msleep)

# Add a column of the proportion of REM sleep to total sleep
msleep %>% 
    mutate(rem_proportion = sleep_rem / sleep_total) %>%
    
# Group the animals by their taxonomic order
    group_by(order) %>%
    
# Summarise by the median REM proportion
    summarise(median_sleep = median(rem_proportion)) %>%
    
# Arrange by the median REM proportion
    arrange(median_sleep) %>%
    
# Take the head() of this to see just the orders with smallest median REM proportion
    head

# What is the median REM proportion of the order with the smallest median REM proportion?
0.09433962


