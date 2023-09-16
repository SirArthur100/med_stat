# for statistics or machine learning, most of the time, we use structured data
# most often, this data is collected into tabular format

# we can import our own data into R and work with it,
# however for testing purposes, we can use some built-in datasets of R
# to see the list of available built-in datasets, we can run the following function:

data()

# we can declare variables
# lets assign the built-in mtcars dataset to a newly created variable

test_table <- mtcars

# lets take a look at the structure of this test_table

str(test_table)

# to check only the names of indexes and columns,
# we can use the following functions:

rownames(test_table)
colnames(test_table)

# now make some quick descriptive statistics on the dataset

summary(test_table)

# to select one column from our test_table, 
# we can use the following syntax:

test_table$wt

# lets check the structure of the selected column:

str(test_table$wt)

# this is called a numeric vector, which means
# that we have many numbers chained together in a list
# next we would like to select a specific element from this list,
# lets say we want to select the fifth element (:D) from this list of numbers
# for this we use the square bracket [] syntax:

test_table$wt[5]
