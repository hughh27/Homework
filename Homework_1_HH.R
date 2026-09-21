#Problem 1
student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section    <- c("A", "B", "A", "B", "A", "B")
quiz1      <- c(82, 91, 76, 88, 95, 69)
quiz2      <- c(85, 89, 80, 92, 94, 74)
passed     <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

#Part A
section <- factor(section, levels = c("A", "B"))

# Five vectors
students <- data.frame(
  student_id = student_id,
  section    = section,
  quiz1      = quiz1,
  quiz2      = quiz2,
  passed     = passed,
  stringsAsFactors = FALSE
)

# studs rows, quiz columns
score_matrix <- matrix(
  c(quiz1, quiz2),
  nrow = length(student_id),
  ncol = 2,
  dimnames = list(student_id, c("quiz1", "quiz2"))
)

# Course name, student data frame, cutoffs vector
course_record <- list(
  course  = "R Programming",
  scores  = students,
  cutoffs = c(pass = 70, excellent = 90)
)

# Inspect
typeof(section)
class(section)
length(section)
str(section)

typeof(course_record)
class(course_record)
length(course_record)
str(course_record)

typeof(students)
class(students)
length(students)
str(students)
dim(students)

typeof(score_matrix)
class(score_matrix)
length(score_matrix)
str(score_matrix)
dim(score_matrix)

# A vector holds one type of data. A factor is a vector for categories,
# stored as integers with labels attached. A matrix is 2d and one type
# throughout. A data frame is 2d but each column can be different types.
# A list is the loosest structure, holding anything, any size.

#Part B
# S04's second quiz score
score_matrix["S04", "quiz2"]

score_matrix[1:2, , drop = FALSE]

course_record["course"]
course_record[["course"]]
course_record$course

# [ keeps the same container type (a list here), even for one item.
# [[ and $ pull out the actual item itself. [[ can take a variable name,
# $ only works with a name typed directly.

#Part C
students$average <- rowMeans(students[, c("quiz1", "quiz2")])

students$excellent <- students$average >= 90

section_a_high <- students[students$section == "A" & students$average >= 80, ]

section_a_high_subset <- section_a_high[, c("student_id", "section", "average")]
section_a_high_subset

student_averages <- setNames(students$average, students$student_id)
student_averages

# Vectorized means the function works on the whole column at once, not one
# value at a time. rowMeans() does this automatically.

#Problem 2
csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

#Part A
measurements <- read.csv(text = csv_text, na.strings = c("", "NA"), stringsAsFactors = FALSE)

# inspect
head(measurements)
str(measurements)
dim(measurements)
names(measurements)

colSums(is.na(measurements))

measurements_complete <- measurements[complete.cases(measurements), ]
measurements_complete

removed_ids <- measurements$sample_id[!complete.cases(measurements)]
removed_ids

# x == NA doesn't work because NA means unknown. Comparing to an unknown
# value gives you another unknown, not TRUE or FALSE.

#Part B
measurements$site   <- factor(measurements$site)
measurements$status <- factor(measurements$status)
levels(measurements$site)
levels(measurements$status)

# Fahrenheit conversion
measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

measurements$ph_below_7 <- measurements$ph < 7

ns_ok <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North", "South") &
    measurements$status == "ok",
]

ns_ok_subset <- ns_ok[, c("sample_id", "site", "temp_c", "temp_f", "ph")]
ns_ok_subset

mean_temp_overall <- mean(measurements$temp_c, na.rm = TRUE)
mean_temp_overall

mean_temp_south <- mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)
mean_temp_south

#Part C
A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

elementwise_product <- A * B
matrix_product      <- A %*% B

elementwise_product
matrix_product

dim(elementwise_product)
dim(matrix_product)

# Both results are 2x2. A * B multiplies matching positions and needs
# the matrices to be the same size. A %*% B is real matrix multiplication,
# taking a row from A times a column from B and adding it up, which needs
# A's columns to match B's rows. Same shape here, different math.

#Problem 3
student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

#Part A
grade_one <- function(score, a_min = 90, b_min = 80, c_min = 70, d_min = 60) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}

# test cases
grade_one(NA)   # NA
grade_one(90)   # "A"
grade_one(80)   # "B"
grade_one(85)   # "B"
grade_one(74)   # "C"

#Part B
grades <- rep(NA_character_, length(scores))

for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}

names(grades) <- student_id
grades

# i is the position in the vector, going from 1 up to the number of scores.

#Part C-1
summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  result <- c(
    total   = length(x),
    missing = sum(is.na(x)),
    mean    = round(mean(x, na.rm = na.rm), digits),
    sd      = round(sd(x, na.rm = na.rm), digits),
    min     = round(min(x, na.rm = na.rm), digits),
    max     = round(max(x, na.rm = na.rm), digits)
  )
  return(result)
}

# call with defaults
summarize_scores(scores)

# call with named arguments
summarize_scores(x = scores, na.rm = TRUE, digits = 2)

#Part C-2
plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}

plot_scores(
  scores,
  type = "b",
  pch  = 19,
  xlab = "Position",
  ylab = "Score",
  main = "Student Scores"
)

# prediction: the plot should show 8 points going from position 1 to 8,
# connected by lines since type = "b". point 3 will have a gap since
# scores[3] is NA, so no dot or line segment shows up there. scores
# jump around a lot, high at position 1 (95), low at position 7 (59)

# plot_scores takes x and plots it against its own position (1 to 8).
# the ... just grabs whatever extra stuff you pass in, like type, pch,
# xlab, ylab, main, and hands it straight to plot(), so you can change
# how it looks without touching the function itself

