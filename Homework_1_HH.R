#Problem 1
student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section    <- c("A", "B", "A", "B", "A", "B")
quiz1      <- c(82, 91, 76, 88, 95, 69)
quiz2      <- c(85, 89, 80, 92, 94, 74)
passed     <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

#Part A
section <- factor(section, levels = c("A", "B"))

students <- data.frame(
  student_id = student_id,
  section    = section,
  quiz1      = quiz1,
  quiz2      = quiz2,
  passed     = passed,
  stringsAsFactors = FALSE
)

score_matrix <- matrix(
  c(quiz1, quiz2),
  nrow = length(student_id),
  ncol = 2,
  dimnames = list(student_id, c("quiz1", "quiz2"))
)

course_record <- list(
  course  = "R Programming",
  scores  = students,
  cutoffs = c(pass = 70, excellent = 90)
)

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

# a vector is just one type of thing in a row. a factor is basically a vector
# but for categories, it stores numbers behind the scenes with labels on top.
# a matrix is 2d and still one type. a data frame is 2d too but each column
# can be its own type. a list can hold pretty much anything and any size

#Part B
score_matrix["S04", "quiz2"]
score_matrix[1:2, , drop = FALSE]

course_record["course"]
course_record[["course"]]
course_record$course

# [ gives you back the same kind of object, so its still a list even with one
# thing in it. [[ and $ actually give you the thing itself. only difference
# between those two is [[ can use a variable, $ needs the actual name typed out

#Part C
students$average <- rowMeans(students[, c("quiz1", "quiz2")])
students$excellent <- students$average >= 90

section_a_high <- students[students$section == "A" & students$average >= 80, ]
section_a_high_subset <- section_a_high[, c("student_id", "section", "average")]
section_a_high_subset

student_averages <- setNames(students$average, students$student_id)
student_averages

# its vectorized because rowMeans() just runs on the whole column in one go
# instead of me having to loop through each student one at a time

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

head(measurements)
str(measurements)
dim(measurements)
names(measurements)

colSums(is.na(measurements))

measurements_complete <- measurements[complete.cases(measurements), ]
measurements_complete

removed_ids <- measurements$sample_id[!complete.cases(measurements)]
removed_ids

# x == NA doesnt work because NA is unknown, so comparing it to anything just gives
# you another unknown back, not true or false. have to use is.na() instead

#Part B
measurements$site   <- factor(measurements$site)
measurements$status <- factor(measurements$status)
levels(measurements$site)
levels(measurements$status)

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

# both come out 2x2. A * B just multiplies the same spot in each matrix so
# they have to be the same size to start. A %*% B is actual matrix mult where
# you take a row and a column and multiply/add them, needs cols of A to match
# rows of B. same size answer here but totally different math going on

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

grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

#Part B
grades <- rep(NA_character_, length(scores))

for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}

names(grades) <- student_id
grades

# i is just which spot in the vector the loop is on right now, goes 1 through
# however many scores there are

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

summarize_scores(scores)
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

# prediction: should be 8 points positions 1-8 connected by lines. point 3
# will be missing/gap since scores[3] is NA. scores go up and down a lot,
# highest at position 1 (95) lowest at position 7 (59)

# the ... just takes whatever extra args i pass (type, pch, xlab etc) and
# sends them straight into plot(), so plot_scores doesnt need to know about
# all of those options itself

