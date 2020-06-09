test_that("mutate_cond can successfully mutate by row", {
  ex_df <- data.frame(id = c("A", "B"), value = c(5, 10))
  ex_df_test <- ex_df %>% mutate_cond(id == "A", value = 15)
  
  # Original dataframe should be untouched
  expect_equal(ex_df$value, c(5, 10))
  # New df should be mutated
  expect_equal(ex_df_test$value, c(15, 10))
})

test_that("If mutate_cond can't find a matching row, it returns the same df",{
  ex_df <- data.frame(id = c("A", "B"), value = c(5, 10))
  ex_df_test <- ex_df %>% mutate_cond(id == "C", value = 15)
  expect_equal(ex_df, ex_df_test)
})