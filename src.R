s <- "Hello World!"  # globalni promenna
s = "string"        # lokalni
print(s)
# comment
if (TRUE) {
    print("True")
}

# Combine multiple values into Vector, List
color_vector <- c('red','green',"yellow")
list1 <- list(c(2,5,3),21.3,sin)

# Data Frames
BMI <- data.frame(
   gender = c("Male", "Male","Female"),
   height = c(152, 171.5, 165),
   weight = c(81,93, 78),
   Age = c(42,38,26)
)

# MATH FUNCTIONS
factorial(5)
choose(3, 1)  # (n|k)

# string join
paste(..., sep = " ")
