###############################################################################
# 00_provision_phylopic.R -- ONE-OFF provisioning. Run manually, not by the build.
#
# Resolves PhyloPic silhouettes for the Figure 1 panel B taxa, saves each to a
# local cache, and records UUID / contributor / licence for every taxon
# attempted -- including the ones that could not be resolved.
#
#   cache   : R/figures/fig1/phylopic_cache/<key>.rds
#   credits : output/figures/phylopic_credits.csv
#
# The figure build (03_panel_b.R) reads ONLY from the cache and never touches
# the network. This script is the only thing here that does.
#
# Where PhyloPic has no suitable Antarctic taxon the entry is recorded with
# status = "unresolved" and panel B leaves a labelled gap at that body mass.
# Nothing is approximated with a wrong taxon -- Kieran substitutes his own
# silhouettes later, and the gap is where they go.
#
# Re-running is safe and incremental: cached taxa are skipped unless FORCE=TRUE.
###############################################################################

suppressPackageStartupMessages({
  library(rphylopic)
})

FORCE     <- FALSE
CACHE_DIR <- "R/figures/fig1/phylopic_cache"
CREDITS   <- "output/figures/phylopic_credits.csv"
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(CREDITS), recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Taxa to resolve.
#
# key        model group / band this silhouette annotates in panel B
# search     candidate PhyloPic search terms, tried in order (most specific
#            Antarctic taxon first, then the containing clade)
# label      text used if the silhouette cannot be resolved
# ---------------------------------------------------------------------------
TAXA <- list(
  list(key = "antarctic krill",  label = "Antarctic krill",
       search = c("Euphausia superba", "Euphausiacea")),
  list(key = "salps",            label = "Salps",
       search = c("Salpa", "Salpidae", "Thaliacea")),
  list(key = "mesopelagic fishes", label = "Mesopelagic fishes",
       search = c("Electrona antarctica", "Myctophidae", "Myctophiformes")),
  list(key = "shelf and coastal fishes", label = "Shelf & coastal fishes",
       search = c("Trematomus", "Nototheniidae", "Notothenioidei")),
  list(key = "toothfishes",      label = "Toothfishes",
       search = c("Dissostichus mawsoni", "Dissostichus", "Nototheniidae")),
  list(key = "squids",           label = "Squids",
       search = c("Psychroteuthis glacialis", "Teuthida", "Decapodiformes")),
  list(key = "flying birds",     label = "Flying birds",
       search = c("Macronectes giganteus", "Procellariidae", "Procellariiformes")),
  list(key = "small divers",     label = "Penguins",
       search = c("Pygoscelis adeliae", "Pygoscelis", "Spheniscidae")),
  list(key = "leopard seals",    label = "Leopard seal",
       search = c("Hydrurga leptonyx", "Phocidae")),
  list(key = "large divers",     label = "Elephant seal",
       search = c("Mirounga leonina", "Mirounga", "Phocidae")),
  list(key = "minke whales",     label = "Minke whales",
       search = c("Balaenoptera bonaerensis", "Balaenoptera acutorostrata")),
  list(key = "orca",             label = "Orca",
       search = c("Orcinus orca", "Orcinus")),
  list(key = "sperm whales",     label = "Sperm whales",
       search = c("Physeter macrocephalus", "Physeter")),
  list(key = "baleen whales",    label = "Large baleen whales",
       search = c("Balaenoptera musculus", "Balaenoptera physalus", "Balaenopteridae"))
)

# ---------------------------------------------------------------------------
# Resolve one taxon: first search term that returns a uuid wins.
# ---------------------------------------------------------------------------
# Licence ranking. Science is a commercial publisher, so NonCommercial images
# are unusable and are rejected outright rather than silently shipped. Among
# usable licences, prefer the least encumbered.
licence_rank <- function(abbr) {
  if (is.na(abbr)) return(99L)
  if (grepl("NC", abbr, fixed = TRUE)) return(-1L)          # unusable
  if (grepl("^(CC0|Public Domain)", abbr)) return(1L)
  if (grepl("^CC BY-SA", abbr)) return(3L)                  # share-alike: usable, flag it
  if (grepl("^CC BY", abbr)) return(2L)
  99L
}

resolve_one <- function(entry) {
  cache_file <- file.path(CACHE_DIR, paste0(gsub("[^a-z0-9]+", "_", entry$key), ".rds"))

  if (file.exists(cache_file) && !FORCE) {
    cached <- readRDS(cache_file)
    cat(sprintf("  [cached]   %-26s %s\n", entry$key, cached$uuid))
    return(cached$credit)
  }

  # Search terms most-specific first; within a term, consider several candidate
  # images and take the best-licensed one rather than whichever is returned first.
  for (nm in entry$search) {
    # get_uuid() warns when a taxon has fewer images than requested; a warning
    # must not discard the results, and n must fall back so single-image taxa
    # (most of the whales) still resolve.
    grab <- function(k) tryCatch(
      suppressWarnings(rphylopic::get_uuid(name = nm, n = k)),
      error = function(e) NULL)
    uuids <- grab(8)
    if (is.null(uuids) || !length(uuids) || all(is.na(uuids))) uuids <- grab(1)
    if (is.null(uuids) || !length(uuids) || all(is.na(uuids))) next
    uuids <- unique(stats::na.omit(as.character(uuids)))

    ranked <- vapply(uuids, function(u) {
      a <- tryCatch(rphylopic::get_attribution(uuid = u), error = function(e) NULL)
      r <- tryCatch(a$images[[u]]$license_abbr, error = function(e) NULL)
      licence_rank(if (is.null(r) || !length(r)) NA_character_ else as.character(r)[1])
    }, integer(1))

    usable <- uuids[ranked > 0]
    if (!length(usable)) {
      cat(sprintf("  [NC only]  %-26s %s -- all candidates NonCommercial, skipping term\n",
                  entry$key, nm))
      next
    }
    uuid <- usable[which.min(ranked[ranked > 0])]

    img <- tryCatch(rphylopic::get_phylopic(uuid = uuid),
                    error = function(e) NULL)
    if (is.null(img)) next

    # rphylopic 1.7.0 returns list(images = list(<uuid> = list(attribution,
    # contributor, license, license_abbr, created, ...)))
    att <- tryCatch(rphylopic::get_attribution(uuid = uuid),
                    error = function(e) NULL)
    rec <- tryCatch(att$images[[uuid]], error = function(e) NULL)
    fld <- function(p) {
      v <- tryCatch(rec[[p]], error = function(e) NULL)
      if (is.null(v) || !length(v) || !nzchar(as.character(v)[1])) NA_character_
      else as.character(v)[1]
    }

    credit <- data.frame(
      key = entry$key, label = entry$label, status = "resolved",
      matched_name = nm, uuid = uuid,
      artist       = fld("attribution"),    # who drew it
      contributor  = fld("contributor"),    # who uploaded it
      licence      = fld("license_abbr"),
      licence_url  = fld("license"),
      created      = fld("created"),
      source = sprintf("https://www.phylopic.org/images/%s", uuid),
      retrieved = format(Sys.Date()), stringsAsFactors = FALSE)

    saveRDS(list(uuid = uuid, img = img, credit = credit), cache_file)
    cat(sprintf("  [resolved] %-26s %-28s %s\n", entry$key, nm, uuid))
    return(credit)
  }

  cat(sprintf("  [MISSING]  %-26s tried: %s\n", entry$key,
              paste(entry$search, collapse = ", ")))
  data.frame(
    key = entry$key, label = entry$label, status = "unresolved",
    matched_name = NA_character_, uuid = NA_character_,
    artist = NA_character_, contributor = NA_character_,
    licence = NA_character_, licence_url = NA_character_, created = NA_character_,
    source = NA_character_, retrieved = format(Sys.Date()), stringsAsFactors = FALSE)
}

cat("=== PhyloPic provisioning for Figure 1 panel B ===\n")
cat("cache:", CACHE_DIR, "\n\n")
credits <- do.call(rbind, lapply(TAXA, resolve_one))

credits$licence_note <- vapply(credits$licence, function(l) {
  if (is.na(l)) return("no silhouette")
  if (grepl("NC", l, fixed = TRUE)) return("UNUSABLE - NonCommercial")
  if (grepl("^CC BY-SA", l)) return("usable - share-alike, attribute")
  if (grepl("^CC BY", l)) return("usable - attribute")
  "usable - no restriction"
}, character(1))

write.csv(credits, CREDITS, row.names = FALSE)
cat(sprintf("\nWrote %s (%d rows: %d resolved, %d unresolved)\n",
            CREDITS, nrow(credits), sum(credits$status == "resolved"),
            sum(credits$status == "unresolved")))
if (any(credits$status == "unresolved"))
  cat("Unresolved (panel B leaves a labelled gap):\n  ",
      paste(credits$key[credits$status == "unresolved"], collapse = "\n  "), "\n")

cat("\nLicence summary:\n")
print(table(credits$licence_note))
bad <- credits[grepl("UNUSABLE", credits$licence_note), ]
if (nrow(bad))
  cat("\n!! NonCommercial images still selected for:",
      paste(bad$key, collapse = ", "),
      "\n   Science is a commercial publisher -- these must be replaced.\n")
sa <- credits[grepl("share-alike", credits$licence_note), ]
if (nrow(sa))
  cat("\nShare-alike (CC BY-SA) images, usable but must be attributed:",
      paste(sa$key, collapse = ", "), "\n")
