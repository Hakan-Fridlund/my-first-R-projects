"""
Various Lesson assignments, basic variables,vectors, operators
"""

x <- c(10.4, 5.6, 3.1, 6.4, 21.7)  # create a vector
assign("x", c(10.4, 5.6, 3.1, 6.4, 21.7)) # create a vector with assign function

1/x  # divide 1 with vector x and prints the values, does not change what is in x

y <- c(x, 0, x)   #y create a vector that adds vector x with 0 and x again

n <- 10  # creates n and gets value 10 

temp <- x > 13   # temp creates logicals (booleans) for vector X, TRUE if over 13 otherwise FALSE
temp

y <- x[!is.na(x)]  # y gets x without NotAvailable numbers

c("x","y")[rep(c(1,2,2,1), times=4)] #produces a character vector of length 16 consisting of "x", "y", "y", "x" repeated four times.

y <- x[-(1:5)] #gives y all but the first five elements of x. the minus negates the expression 

fruit <- c(5, 10, 1, 20)
names(fruit) <- c("orange", "banana", "apple", "peach")  #alfanumeric namnes
lunch <- fruit[c("apple","orange")]
lunch

x[is.na(x)] <- 0 #replaces any missing values in x by zeros

# as.something() for either coercion from one mode to another ex as.integer() as.double() as.character()

scan() #similar to input, unlimited input until double enter


x <- c(10, 5, 100, 2, 65, 13)   # create a vector and checking class
x
class(x)

y <- list("Janne", 10, 1.1, "Titti", TRUE) #creates a list with or without names for the elements
y <- list(namn = "Janne", ålder = 45)   
y
class(y)

z <- data.frame(                                 #creates a dataframe
  name = c("Janne", "Titti"),
  age = c(42, 35)
)
z
class(z) 
str(z)       # structure

