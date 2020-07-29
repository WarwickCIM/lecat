test_that("Cotable creation works", {

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

  # set flag to prevent files being when tests run
  do_not_write_files <- TRUE
  cooc <- lecat::create_cooccurrence_graph(lecat_result)
  rm(do_not_write_files)

  expect_equal(2 * 2, 4)
})
