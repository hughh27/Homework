install.packages("tidyverse")
sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)
sales_clean <- sales_raw |> mutate(item_clean = str_to_lower(str_squish(item_raw)),
                                   amount = parse_number(amount_raw),
                                   discount_rate = parse_number(discount_raw)/100,
                                   net_amount = amount * (1 - discount_rate)
                                   )
sales_clean <- sales_clean |> mutate(size = case_when(
  is.na(net_amount) ~ "unrecorded" , 
  net_amount >= 20 ~ "large",
  net_amount < 20 ~ "small"
))

sales_clean |> select(sale_id, item_clean, amount,
discount_rate, net_amount, size)

sales_codes <- sales_clean |> mutate(code_clean = str_to_upper(str_trim(code_raw)),
                                    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$") )
invalid_codes <- sales_codes |> filter(!valid_code) |> select(sale_id, code_raw, code_clean)
invalid_codes

valid_code_parts <- sales_codes |> filter(valid_code) |> 
  separate_wider_delim(code_clean, delim = "-" , names = c("channel", "number"),
  cols_remove = FALSE) |>
  select(sale_id,code_clean,channel,number )
valid_code_parts

sale_tags <- sales_clean |> select(sale_id,tags) |> separate_longer_delim(tags, delim = ";")

sale_tags

#separate_wider_delim splits a text column into new columns while separate_longer_delim 
#puts it into new rows

#Part C

customer_summary <- sales_clean |> summarise(sales_n = n(), known_n = sum(!is.na(net_amount)),
                              large_n = sum(net_amount >= 20, na.rm = TRUE),
                                            large_share = if_else(known_n == 0, NA_real_, large_n / known_n),
                                            .by = customer_id) |> arrange(customer_id)
                              
        
customer_summary                              

# The numerator is large_n, the number of sales with a known net amount of at least 20.
# The denominator is known_n, the number of sales whose net amount is not NA.
# If a customer has no known amounts (known_n = 0), large_share is NA_real_.

sales_factors <- sales_clean |> mutate(item_factor = fct_infreq(factor(item_clean)),
item_group = fct_lump_n(factor(item_clean), n = 2, other_level = "Other"))

# Levels
levels(sales_factors$item_factor)
levels(sales_factors$item_group)

# Category counts
sales_factors |> count(item_factor)
sales_factors |> count(item_group)

#Changing level order (fct_infreq()) only changes the order the categories are listed in, 
#so the values and the number of categories stay the same, while combining categories 
#(fct_lump_n()) changes the values by merging the less common items into "Other", 
#which reduces the number of categories from 4 to 3.

#Problem 2
reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)
customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)
 
#Part A
reports_labeled <- reports_raw |> arrange(form,line) |> group_by(form) |> 
  fill(customer_id, .direction = "down") |> ungroup()

reports_labeled

reports_clean <- reports_labeled |> mutate(primary_clean = na_if(primary_amount, -999), 
                                           amount = coalesce(primary_clean, backup_amount))

'The backup is only used when the primary amount is missing, and a recorded primary, 
including zero, is a real measurement rather than a missing value. Since coalesce() 
only replaces NA, it keeps the primary of 0 in form B, line 2 instead of using the backup
of 99, which would wrongly change that reports amount.'

reports_complete <- reports_clean |> select(customer_id, round, amount) |> 
  mutate(recorded_row = TRUE) |> complete(customer_id, round = 1:3, fill = list(recorded_row = FALSE))

reports_complete
reports_clean |>
  summarise(rows = n(), missing_amounts = sum(is.na(amount)))

reports_complete |>
  summarise(
    original_rows   = sum(recorded_row),
    added_rows      = sum(!recorded_row),
    missing_amounts = sum(is.na(amount))
  )
#An explicit missing amount is C3 in round 1: the row exists, but its amount is NA. 
#An implicit missing value is C1 in round 2: before completion there is no row for that 
#customer and round at all, so the missing value isn't shown until complete() adds the row.

#Part B
customers |> count(customer_id) |> filter(n>1)

customers |> filter(is.na(customer_id))

reports_clean |>
  count(customer_id, round) |>
  filter(n > 1)

customers_marked <- customers |> mutate(customer_found = TRUE  )
reports_joined <- reports_clean |>
  left_join(customers_marked,
            join_by(customer_id),
            relationship = "many-to-one",
            na_matches = "never") |>
  mutate(customer_found = replace_na(customer_found, FALSE))

reports_joined |> select(customer_id, round, amount, region, customer_found)

reports_clean |> anti_join(customers, join_by(customer_id), na_matches = "never") |> distinct(customer_id)

customers |> anti_join(reports_clean, join_by(customer_id), na_matches = "never") |>  distinct(customer_id)

#A missing region alone doesn't say why it's missing. customer_found shows that C3 has a 
#customer record whose region was simply not recorded (TRUE), while C4 has no customer 
#record at all (FALSE), which matches the anti_join() result listing C4 as found only in 
#reports_clean.

#Part C

#First part customer_bad is not a existing table??

#With relationship = "many-to-one", the join would stop with an error because a customer ID 
#appears more than once in customers_bad. Allowing "many-to-many" does not fix the conflicting 
#customer information; it only lets the join run, which duplicates that customer's reports and 
#attaches both versions of their details, so the conflict still has to be resolved in the 
#customer table first.

reports_wide <- reports_complete |> select(customer_id,round,amount) |> pivot_wider(names_from = round, values_from = amount,
                                                                    names_prefix = "round_")

reports_wide


#Problem 3
#PartA
two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  # Sample summaries
  nx  <- length(x)
  ny  <- length(y)
  xbar <- mean(x)
  ybar <- mean(y)
  sx2 <- var(x)
  sy2 <- var(y)
  
  # Choose the method with if / else
  if (equal_var) {
    method <- "Pooled"
    sp2 <- ((nx - 1) * sx2 + (ny - 1) * sy2) / (nx + ny - 2)
    se  <- sqrt(sp2 * (1 / nx + 1 / ny))
    df  <- nx + ny - 2
  } else {
    method <- "Welch"
    se <- sqrt(sx2 / nx + sy2 / ny)
    df <- (sx2 / nx + sy2 / ny)^2 /
      ((sx2 / nx)^2 / (nx - 1) + (sy2 / ny)^2 / (ny - 1))
  }
  
  # t-statistic and two-sided p-value
  t_stat  <- (xbar - ybar) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  
  list(method  = method,
       t_stat  = t_stat,
       df      = df,
       p_value = p_value,
       reject  = p_value < alpha)
}
x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

welch  <- two_sample_t(x_check, y_check)                    # default arguments
pooled <- two_sample_t(x_check, y_check, equal_var = TRUE)
welch
pooled

# Verification with t.test() (outside the function)
tt_welch  <- t.test(x_check, y_check, var.equal = FALSE)
tt_pooled <- t.test(x_check, y_check, var.equal = TRUE)

comparison <- data.frame(
  method  = c("Welch", "Pooled"),
  t_diff  = c(abs(welch$t_stat  - tt_welch$statistic),
              abs(pooled$t_stat - tt_pooled$statistic)),
  df_diff = c(abs(welch$df  - tt_welch$parameter),
              abs(pooled$df - tt_pooled$parameter)),
  p_diff  = c(abs(welch$p_value  - tt_welch$p.value),
              abs(pooled$p_value - tt_pooled$p.value))
)
comparison$all_below_1e8 <- comparison$t_diff < 1e-8 &
  comparison$df_diff < 1e-8 &
  comparison$p_diff < 1e-8
comparison

#Part B
B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null", "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)


n_rows <- nrow(scenarios) * B * 2
simulation_results <- data.frame(
  scenario  = character(n_rows),
  replicate = integer(n_rows),
  method    = character(n_rows),
  t_stat    = numeric(n_rows),
  df        = numeric(n_rows),
  p_value   = numeric(n_rows),
  reject    = logical(n_rows)
)


set.seed(2026)

row <- 0
for (i in seq_len(nrow(scenarios))) {          # loop over scenarios
  for (b in 1:B) {                             # loop over repetitions
    x <- rnorm(n_x, mean = scenarios$mean_x[i], sd = scenarios$sd_x[i])
    y <- rnorm(n_y, mean = scenarios$mean_y[i], sd = scenarios$sd_y[i])
    
    for (equal in c(FALSE, TRUE)) {            # both methods, same samples
      res <- two_sample_t(x, y, equal_var = equal, alpha = alpha)
      row <- row + 1
      simulation_results[row, ] <- list(scenarios$scenario[i], b, res$method,
                                        res$t_stat, res$df, res$p_value, res$reject)
    }
  }
}

head(simulation_results)
nrow(simulation_results)
simulation_summary <- simulation_results |>
  summarise(
    mean_t         = mean(t_stat),
    sd_t           = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  ) |>
  mutate(
    mean_t         = round(mean_t, 3),
    sd_t           = round(sd_t, 3),
    rejection_rate = round(rejection_rate, 3)
  )

simulation_summary
ggplot(simulation_summary, aes(x = scenario, y = rejection_rate, fill = method)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  labs(y = "Rejection rate", title = "Rejection rates by scenario and method")

#When the means are equal, Welch's rejection rates (0.044 with equal variances and 
#0.061 with unequal variances) and the pooled test's rate with equal variances (0.038) 
#are all close to 0.05, within the simulation error of about ±0.014 for 1000 repetitions,
#but the pooled test's rate jumps to 0.139 when variances are unequal, so its Type I error 
#is badly inflated. When the means differ by one unit, both tests have high power with equal 
#variances (0.956 for Welch and 0.957 for pooled), while power drops with unequal variances
#to 0.282 for Welch and 0.497 for pooled. The pooled test's higher rejection rate in the 
#\unequal-variance shift is not trustworthy, because the same test already rejected 13.9% of 
#the time when there was no difference at all, so part of its apparent power comes from false 
#rejections. Higher power alone is therefore not enough to judge a test, since power is only
#meaningful once the Type I error is controlled near α, which makes Welch the safer choice when
#the variances may differ.
