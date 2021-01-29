context("data table buttons")

library(data.table)

set.seed(2020)

n = 10

df = data.table(no = 1:n, 
                x = runif(n),
                y = runif(n), 
                pixels = runif(n), 
                growth = runif(n), 
                relx = runif(n), 
                rely = runif(n))

out = wiad:::displayDataTable(df = df, 
                              id1 = 1, 
                              id2 = 2)

test_that("test displaydata function", {
  expect_length(out, n = 8)
})
