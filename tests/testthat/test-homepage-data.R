homepage_fixture <- function() {
  data.frame(
    trait = c("SLA", "SLA", "  ", NA, "Vcmax"),
    species_id = c(1, 1, 2, NA, 3),
    site_id = c(10, 10, 11, NA, 20),
    citation_id = c(100, 100, 101, NA, 200),
    scientificname = c("Alpha plant", "Alpha plant", "Beta plant", NA, "Gamma plant"),
    stringsAsFactors = FALSE
  )
}

test_that("homepage metrics count observations and linked entities", {
  metrics <- betydata:::homepage_metrics(homepage_fixture())

  expect_equal(metrics$value, c(5, 2, 3, 3, 3))
})

test_that("duplicating an observation affects only the observation total", {
  data <- homepage_fixture()
  duplicated_data <- rbind(data, data[1, ])

  original <- betydata:::homepage_metrics(data)$value
  duplicated <- betydata:::homepage_metrics(duplicated_data)$value

  expect_equal(duplicated, original + c(1, 0, 0, 0, 0))
})

test_that("display fields do not change homepage metrics", {
  data <- homepage_fixture()
  original <- betydata:::homepage_metrics(data)$value

  data$scientificname <- paste("renamed", seq_len(nrow(data)))
  expect_equal(betydata:::homepage_metrics(data)$value, original)
})

test_that("top traits exclude blank values and sort ties deterministically", {
  top_traits <- betydata:::homepage_top_traits(homepage_fixture())

  expect_equal(top_traits$trait, c("SLA", "Vcmax"))
  expect_equal(top_traits$observations, c(2L, 1L))
})

test_that("top species use species IDs and a single scientific name per ID", {
  top_species <- betydata:::homepage_top_species(homepage_fixture())

  expect_equal(top_species$species_id, c(1, 2, 3))
  expect_equal(top_species$species, c("Alpha plant", "Beta plant", "Gamma plant"))
  expect_equal(top_species$observations, c(2L, 1L, 1L))
  expect_false(any(is.na(top_species$species)))
  expect_false(any(trimws(top_species$species) == ""))
})

test_that("top species reject conflicting names for one species ID", {
  data <- homepage_fixture()
  data$scientificname[2] <- "Different alpha plant"

  expect_error(
    betydata:::homepage_top_species(data),
    "at most one non-blank scientificname"
  )
})

test_that("homepage helpers agree with direct traitsview calculations", {
  data("traitsview", package = "betydata")
  metrics <- betydata:::homepage_metrics(traitsview)
  non_blank <- function(x) !is.na(x) & (!is.character(x) | nzchar(trimws(x)))

  expect_equal(metrics$value, c(
    nrow(traitsview),
    length(unique(traitsview$trait[non_blank(traitsview$trait)])),
    length(unique(traitsview$species_id[!is.na(traitsview$species_id)])),
    length(unique(traitsview$site_id[!is.na(traitsview$site_id)])),
    length(unique(traitsview$citation_id[!is.na(traitsview$citation_id)]))
  ))
  expect_false(any(is.na(betydata:::homepage_top_species(traitsview)$species_id)))
})
