test_that("notin works as expected", {
  my_arr <- c("apple", "orange")
  
  expect_equal("apple" %notin% my_arr, FALSE)
  expect_equal("grape" %notin% my_arr, TRUE)
})

test_that("inline_div width works as expected", {
  expect_equal(
    inline_div(width = 1), 
    div(style='display: inline-block; vertical-align:top; width:100%;')
  )
  
  expect_equal(
    inline_div(width = .1), 
    div(style='display: inline-block; vertical-align:top; width:10%;')
  )
})

test_that("details parameters work as expected", {
  expect_equal(
    details(summary = "my summary", content = "mycontent"),
    HTML("<details>
        <summary style='font-size:100%;'>my summary</summary>mycontent</details>")
  )
  
  expect_equal(
    details(summary = "my summary", content = "mycontent", summary_multiplier = 2),
    HTML("<details>
        <summary style='font-size:200%;'>my summary</summary>mycontent</details>")
  )
})
