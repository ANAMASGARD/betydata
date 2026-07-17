# Homepage summary helpers used by the Quarto site.

homepage_non_blank <- function(x) {
  !is.na(x) & (!is.character(x) | nzchar(trimws(x)))
}

homepage_metrics <- function(data) {
  data.frame(
    metric = c(
      "Observations",
      "Measured variables",
      "Represented species",
      "Contributing sites",
      "Data sources"
    ),
    value = c(
      nrow(data),
      length(unique(data$trait[homepage_non_blank(data$trait)])),
      length(unique(data$species_id[!is.na(data$species_id)])),
      length(unique(data$site_id[!is.na(data$site_id)])),
      length(unique(data$citation_id[!is.na(data$citation_id)]))
    ),
    definition = c(
      "Trait and yield records in the primary dataset",
      "Distinct variables with recorded observations",
      "Distinct species linked to observation records",
      "Distinct research sites linked to observation records",
      "Distinct cited sources linked to observation records"
    ),
    stringsAsFactors = FALSE
  )
}

homepage_top_traits <- function(data, n = 10L) {
  traits <- data$trait[homepage_non_blank(data$trait)]
  counts <- as.data.frame(table(traits), stringsAsFactors = FALSE)
  names(counts) <- c("trait", "observations")
  counts$observations <- as.integer(counts$observations)
  counts <- counts[order(-counts$observations, counts$trait), , drop = FALSE]
  utils::head(counts, n)
}

homepage_top_species <- function(data, n = 10L) {
  valid_species <- !is.na(data$species_id) & homepage_non_blank(data$scientificname)
  data <- data[valid_species, c("species_id", "scientificname"), drop = FALSE]
  data$scientificname <- trimws(data$scientificname)
  data$scientificname[!homepage_non_blank(data$scientificname)] <- NA_character_

  species_ids <- unique(data$species_id)
  summary <- lapply(species_ids, function(species_id) {
    rows <- data[data$species_id == species_id, , drop = FALSE]
    names <- unique(rows$scientificname[!is.na(rows$scientificname)])

    if (length(names) > 1L) {
      stop(
        "Each species_id must have at most one non-blank scientificname.",
        call. = FALSE
      )
    }

    data.frame(
      species_id = species_id,
      species = if (length(names) == 1L) names else NA_character_,
      observations = nrow(rows),
      stringsAsFactors = FALSE
    )
  })

  summary <- do.call(rbind, summary)
  summary <- summary[order(-summary$observations, summary$species, summary$species_id), , drop = FALSE]
  rownames(summary) <- NULL
  utils::head(summary, n)
}
