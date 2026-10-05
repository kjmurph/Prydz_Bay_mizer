# A1_lineage.R -- read-only lineage reconstruction of the saved MizerParams objects
#
# Usage: Rscript --vanilla R/model_construction/A1_lineage.R [<repo> <out_dir>]
#   (defaults: ASSESS_REPO or "."; ASSESS_OUT or Output_large_files/model_construction)
#
# Compares the STORED slots (attributes()) of every saved MizerParams under
# <repo>/params and <repo>. Nothing is upgraded before comparison, so nothing
# mizer 3.1.0 adds or recomputes can show up as an edit; a slot present on one
# side only is reported as a version artefact. Writes only into <out_dir>.

args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args) >= 1) args[1] else Sys.getenv("ASSESS_REPO", ".")
out  <- if (length(args) >= 2) args[2] else Sys.getenv("ASSESS_OUT", "Output_large_files/model_construction")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(mizer))
hashfun <- rlang::hash

files <- c(list.files(file.path(repo, "params"), pattern = "\\.(rds|RDS)$",
                      full.names = TRUE),
           list.files(repo, pattern = "\\.(rds|RDS)$", full.names = TRUE))
files <- files[!grepl(" - Copy", files)]

skip_keys <- c("class", "time_created", "time_modified", "mizer_version",
               "metadata")

# One hash per field: data-frame columns, named-list elements (two levels
# deep inside other_params, where therMizer keeps its arrays), else the slot.
fingerprint <- function(a) {
  fp <- character(0)
  for (nm in setdiff(names(a), skip_keys)) {
    v <- a[[nm]]
    if (is.data.frame(v)) {
      for (cn in names(v)) fp[paste0(nm, "$", cn)] <- hashfun(v[[cn]])
    } else if (is.list(v) && length(v) > 0 && !is.null(names(v))) {
      for (en in names(v)) {
        ve <- v[[en]]
        if (nm == "other_params" && is.list(ve) && !is.data.frame(ve) &&
            length(ve) > 0 && !is.null(names(ve))) {
          for (en2 in names(ve)) {
            fp[paste0(nm, "$", en, "$", en2)] <- hashfun(ve[[en2]])
          }
        } else {
          fp[paste0(nm, "$", en)] <- hashfun(ve)
        }
      }
    } else {
      fp[nm] <- hashfun(v)
    }
  }
  fp
}

# Fetch a field by fingerprint key
get_field <- function(a, key) {
  parts <- strsplit(key, "$", fixed = TRUE)[[1]]
  v <- a[[parts[1]]]
  for (p in parts[-1]) v <- v[[p]]
  v
}

as_time <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(as.POSIXct(NA))
  as.POSIXct(x)
}

meta <- list(); fps <- list(); objs <- list()
for (f in files) {
  x <- tryCatch(readRDS(f), error = function(e) NULL)
  if (!is(x, "MizerParams")) next
  a <- attributes(x)
  key <- sub(paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", repo), "/?"),
             "", f)
  sp <- a$species_params
  meta[[key]] <- data.frame(
    file = key,
    md5 = unname(tools::md5sum(f)),
    bytes = file.size(f),
    mizer = if (!is.null(a$mizer_version)) as.character(a$mizer_version) else NA,
    created = as_time(a$time_created),
    modified = as_time(a$time_modified),
    nsp = nrow(sp), nw = length(a$w), nwfull = length(a$w_full),
    encounter = if (!is.null(a$rates_funcs$Encounter)) a$rates_funcs$Encounter else NA,
    resdyn = if (!is.null(a$resource_dynamics)) a$resource_dynamics else NA,
    other = paste(sort(names(a$other_params$other)), collapse = ";"),
    stringsAsFactors = FALSE)
  if (nrow(sp) == 19 && length(a$w) == 100 && length(a$w_full) == 142) {
    fps[[key]] <- fingerprint(a)
    objs[[key]] <- a
  }
}
meta <- do.call(rbind, meta); rownames(meta) <- NULL
meta <- meta[order(meta$created, meta$modified), ]
write.csv(meta, file.path(out, "objects.csv"), row.names = FALSE)

# ---- distances among the 19-group objects --------------------------------
ord <- meta$file[meta$file %in% names(fps)]
n <- length(ord)
Dm <- matrix(NA_integer_, n, n, dimnames = list(ord, ord))
Vm <- Dm
for (i in seq_len(n)) for (j in seq_len(n)) {
  fi <- fps[[ord[i]]]; fj <- fps[[ord[j]]]
  common <- intersect(names(fi), names(fj))
  Dm[i, j] <- sum(fi[common] != fj[common])
  Vm[i, j] <- length(union(names(fi), names(fj))) - length(common)
}
write.csv(Dm, file.path(out, "distance_fields.csv"))

mod <- setNames(meta$modified, meta$file)
par_rows <- list()
for (i in seq_len(n)) {
  child <- ord[i]
  earlier <- ord[mod[ord] < mod[child]]
  if (!length(earlier)) next
  d <- Dm[child, earlier, drop = FALSE][1, ]
  names(d) <- earlier
  o <- order(d, -as.numeric(mod[earlier]))
  best <- earlier[o[1]]
  runner <- if (length(o) > 1) earlier[o[2:min(4, length(o))]] else character(0)
  par_rows[[child]] <- data.frame(
    child = child, parent = best, n_fields = d[best],
    version_only = Vm[child, best],
    runners_up = if (length(runner)) paste0(runner, " (", d[runner], ")",
                                            collapse = "; ") else "",
    stringsAsFactors = FALSE)
}
parents <- do.call(rbind, par_rows); rownames(parents) <- NULL
write.csv(parents, file.path(out, "parents.csv"), row.names = FALSE)

# ---- detailed diffs along each chosen parent -> child edge ----------------
relmax <- function(x, y) {
  x <- as.numeric(x); y <- as.numeric(y)
  if (length(x) != length(y)) return(NA_real_)
  ok <- !(is.na(x) & is.na(y))
  if (!any(ok)) return(0)
  if (any(is.na(x[ok]) != is.na(y[ok]))) return(Inf)
  ok <- ok & !is.na(x) & !is.na(y)
  den <- pmax(abs(x[ok]), abs(y[ok]))
  num <- abs(x[ok] - y[ok])
  r <- ifelse(den == 0, 0, num / den)
  r[!is.finite(x[ok]) | !is.finite(y[ok])] <-
    ifelse(x[ok] == y[ok], 0, Inf)[!is.finite(x[ok]) | !is.finite(y[ok])]
  max(r)
}
short <- function(v) {
  s <- if (is.numeric(v)) format(signif(v, 7)) else as.character(v)
  s <- paste(s, collapse = ",")
  if (nchar(s) > 60) paste0(substr(s, 1, 57), "...") else s
}

edge_rows <- list(); sp_rows <- list(); gear_rows <- list(); int_rows <- list()
for (k in seq_len(nrow(parents))) {
  ch <- parents$child[k]; pa <- parents$parent[k]
  fc <- fps[[ch]]; fpp <- fps[[pa]]
  keys <- union(names(fc), names(fpp))
  for (key in keys) {
    inc <- key %in% names(fc); inp <- key %in% names(fpp)
    if (inc && inp && fc[key] == fpp[key]) next
    if (!(inc && inp)) {
      edge_rows[[length(edge_rows) + 1]] <- data.frame(
        child = ch, parent = pa, key = key,
        kind = if (inc) "added" else "removed", n_diff = NA, max_rel = NA,
        detail = "", stringsAsFactors = FALSE)
      next
    }
    vc <- get_field(objs[[ch]], key); vp <- get_field(objs[[pa]], key)
    kind <- "changed"; nd <- NA; mr <- NA; detail <- ""
    if (is.numeric(vc) && is.numeric(vp)) {
      if (length(vc) == length(vp) && identical(dim(vc), dim(vp))) {
        same <- (vc == vp) | (is.na(vc) & is.na(vp))
        same[is.na(same)] <- FALSE
        nd <- sum(!same); mr <- relmax(vc, vp)
        if (nd == 0) kind <- "storage only"
      } else {
        kind <- "shape"
        detail <- paste0(paste(dim(vp) %||% length(vp), collapse = "x"), " -> ",
                         paste(dim(vc) %||% length(vc), collapse = "x"))
      }
    } else {
      detail <- paste0(short(vp), " -> ", short(vc))
    }
    edge_rows[[length(edge_rows) + 1]] <- data.frame(
      child = ch, parent = pa, key = key, kind = kind, n_diff = nd,
      max_rel = mr, detail = detail, stringsAsFactors = FALSE)

    # per-species and per-gear detail for the parameter tables
    if (startsWith(key, "species_params$") && length(vc) == length(vp)) {
      spn <- objs[[ch]]$species_params$species
      col <- sub("species_params$", "", key, fixed = TRUE)
      for (s in seq_along(vc)) {
        a1 <- vp[s]; a2 <- vc[s]
        if (identical(a1, a2) || (is.na(a1) && is.na(a2))) next
        if (!is.na(a1) && !is.na(a2) && is.numeric(a1) && a1 == a2) next
        sp_rows[[length(sp_rows) + 1]] <- data.frame(
          child = ch, parent = pa, column = col, species = spn[s],
          old = if (is.numeric(a1)) signif(a1, 10) else as.character(a1),
          new = if (is.numeric(a2)) signif(a2, 10) else as.character(a2),
          stringsAsFactors = FALSE)
      }
    }
    if (startsWith(key, "gear_params$") && length(vc) == length(vp)) {
      gp <- objs[[ch]]$gear_params
      col <- sub("gear_params$", "", key, fixed = TRUE)
      for (s in seq_along(vc)) {
        a1 <- vp[s]; a2 <- vc[s]
        if (identical(a1, a2) || (is.na(a1) && is.na(a2))) next
        if (!is.na(a1) && !is.na(a2) && is.numeric(a1) && a1 == a2) next
        gear_rows[[length(gear_rows) + 1]] <- data.frame(
          child = ch, parent = pa, column = col,
          row = paste(gp$species[s], gp$gear[s], sep = " / "),
          old = if (is.numeric(a1)) signif(a1, 10) else as.character(a1),
          new = if (is.numeric(a2)) signif(a2, 10) else as.character(a2),
          stringsAsFactors = FALSE)
      }
    }
    if (key == "interaction" && identical(dim(vc), dim(vp))) {
      w <- which(vc != vp, arr.ind = TRUE)
      if (nrow(w) > 0 && nrow(w) <= 60) {
        for (r in seq_len(nrow(w))) {
          int_rows[[length(int_rows) + 1]] <- data.frame(
            child = ch, parent = pa,
            predator = rownames(vc)[w[r, 1]], prey = colnames(vc)[w[r, 2]],
            old = vp[w[r, 1], w[r, 2]], new = vc[w[r, 1], w[r, 2]],
            stringsAsFactors = FALSE)
        }
      }
    }
  }
}
bind <- function(l) if (length(l)) do.call(rbind, l) else data.frame()
write.csv(bind(edge_rows), file.path(out, "lineage_edges.csv"), row.names = FALSE)
write.csv(bind(sp_rows), file.path(out, "species_param_changes.csv"), row.names = FALSE)
write.csv(bind(gear_rows), file.path(out, "gear_param_changes.csv"), row.names = FALSE)
write.csv(bind(int_rows), file.path(out, "interaction_changes.csv"), row.names = FALSE)

options(width = 220)
cat("\n== objects (", nrow(meta), " MizerParams;", n, "on the 19-group grid) ==\n")
print(meta[, c("file", "mizer", "created", "modified", "nsp", "encounter")],
      right = FALSE)
cat("\n== nearest earlier object (fields differing) ==\n")
print(parents[, c("child", "parent", "n_fields", "version_only")], right = FALSE)
