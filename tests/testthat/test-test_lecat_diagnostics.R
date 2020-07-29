test_that("Diagnostic generation works", {

  lecat_result <- data.frame(
    Type = c('Creatures', 'Creatures', 'Creatures', 'Language', 'Language', 'Language'),
    Category = c('People', 'People', 'People', 'Common words','Common words','Common words'),
    Query = c('wonder', 'keen', 'wish', 'the', 'from', 'and'),
    Column_examined = c('stimuli','stimuli','stimuli','stimuli','stimuli','stimuli'),
    V1 = c(0,0,0,0,2,0),
    V2 = c(1,1,0,0,1,0),
    V3 = c(0,1,0,0,1,0),
    V4 = c(0,0,1,0,0,1)
  )

  diagnostics <- lecat::create_unique_total_diagnostics(lecat_result)

  expected_diagnostics <- data.frame(
    Type = c("Creatures(4,3)", "Language(5,4)"),
    Category = c("People(4,3)","Common words(5,4)"),
    Queries = c(" wonder(1,1) keen(2,2) wish(1,1)"," the(0,0) from(4,3) and(1,1)"),
    Column_examined = c('stimuli', 'stimuli')
  )

  testthat::expect_identical(diagnostics, expected_diagnostics)
})
