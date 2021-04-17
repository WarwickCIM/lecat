test_that("LE-CAT analysis works", {
  # Create test corpus, lexicon and lookup table
  # Quote taken from Hitchhikers Guide to the Galaxy
  corpus <- data.frame(
    id = c(1,2,3,4),
    stimuli = c(
      'Bypasses are devices that allow some people to dash from point A to point B very fast while other people dash from point B to point A very fast.',
      'People living at point C, being a point directly in between, are often given to wonder what’s so great about point A that so many people from point B are so keen to get there,',
      'and what’s so great about point B that so many people from point A are so keen to get there.',
      'They often wish that people would just once and for all work out where they wanted to be.'
    )
  )

  lexicon <- data.frame(
    Type = c('Creatures', 'Language'),
    Category= c('People', 'Common words'),
    Query = c('wonder', 'the'),
    Query1 = c('keen', 'from'),
    Query2 = c('wish', 'and')
  )

  lookup_table <- data.frame(
    Type = c('Creatures', 'Language'),
    Column = c('stimuli', 'stimuli')
  )

  lexicon <- lecat::parse_lexicon(lexicon)

  lecat_result <- run_lecat_analysis(lexicon = lexicon, corpus = corpus, searches = lookup_table, id = 'id')

  expected_lecat_result <- data.frame(
    Type = c('Creatures', 'Creatures', 'Creatures', 'Language', 'Language', 'Language'),
    Category = c('People', 'People', 'People', 'Common words','Common words','Common words'),
    Query = c('wonder', 'keen', 'wish', 'the', 'from', 'and'),
    Column_examined = c('stimuli','stimuli','stimuli','stimuli','stimuli','stimuli'),
    V1 = c(0,0,0,0,2,0),
    V2 = c(1,1,0,0,1,0),
    V3 = c(0,1,0,0,1,0),
    V4 = c(0,0,1,0,0,1)
  )

  testthat::expect_identical(lecat_result, expected_lecat_result)
})
