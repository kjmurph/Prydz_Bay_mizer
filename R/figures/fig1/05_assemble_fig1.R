###############################################################################
# 05_assemble_fig1.R -- compose and export Figure 1
#
# Layout:
#   A spans the full width
#   B and C side by side beneath, roughly equal width
#
# Run from the repository root:
#   Rscript R/figures/fig1/05_assemble_fig1.R
#
# Outputs (output/figures/): fig1.pdf (vector, primary), fig1.svg (Illustrator),
# fig1.png (600 dpi, drafts).
#
# This is the only script in the fig1 set that writes files. The panel scripts
# are side-effect free and are sourced, not run.
###############################################################################

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

here <- "R/figures/fig1"
source(file.path(here, "01_theme_fig1.R"))
source(file.path(here, "02_panel_a.R"))
source(file.path(here, "03_panel_b.R"))
source(file.path(here, "04_panel_c.R"))

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

message("Building panels ...")
pA <- panel_a()
pB <- panel_b()
pC <- panel_c("lambda")

# Panel C is itself a patchwork of two sub-plots. Left bare, tag_levels = "A"
# descends into it and tags the sub-plots (C) and (D), and guides = "collect"
# hoists its scenario key into the shared legend. wrap_elements() makes it one
# opaque element: a single (C) tag, and its scenario key stays inside the panel
# where the spec wants it.
bottom <- (pB | wrap_elements(full = pC)) + plot_layout(widths = c(1, 1))

# The taxon legend is NOT collected to figure level. Panel B uses identity
# scales and contributes no guide of its own, so the taxon legend is entirely
# panel A's -- collecting it only moved it into a full-width band at the foot of
# the figure, where it clipped panel C's x-axis title (panel C stacks two
# sub-plots and so reaches lower than panel B). Leaving it inside panel A gives
# the same single shared legend, sitting next to the data it describes, and
# removes the collision entirely. Panel B's colours are keyed to it by
# PANEL_A_LEGEND_TAXA, and panel C keeps its own scenario key inside its panel.
fig1 <- (pA / bottom) +
  plot_layout(heights = c(0.85, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = BASE_SIZE_PT + 2, face = "bold"))

# ---------------------------------------------------------------------------
# Export at exactly 18.4 cm width. Height is set below the 22.7 cm ceiling.
# ---------------------------------------------------------------------------
W <- FIG1_WIDTH_CM
H <- 18.4
if (H > FIG1_MAXHEIGHT)
  stop(sprintf("Height %.1f cm exceeds the %.1f cm ceiling.", H, FIG1_MAXHEIGHT))

pdf_path <- file.path(OUT_DIR, "fig1.pdf")
svg_path <- file.path(OUT_DIR, "fig1.svg")
png_path <- file.path(OUT_DIR, "fig1.png")

message("Exporting ...")
ggsave(pdf_path, fig1, width = W, height = H, units = "cm", device = cairo_pdf)
ggsave(svg_path, fig1, width = W, height = H, units = "cm",
       device = svglite::svglite)
ggsave(png_path, fig1, width = W, height = H, units = "cm", dpi = 600,
       bg = "white")

for (f in c(pdf_path, svg_path, png_path))
  message(sprintf("  %-28s %8.2f MB", f, file.size(f) / 1e6))

# ---------------------------------------------------------------------------
# Verify the PDF rather than assume: fonts must be embedded.
# ---------------------------------------------------------------------------
message("\nChecking embedded fonts in ", pdf_path, " ...")
emb <- tryCatch(suppressWarnings(system2("pdffonts", pdf_path,
                                         stdout = TRUE, stderr = TRUE)),
                error = function(e) NULL)
if (!is.null(emb) && length(emb) > 2) {
  cat(paste(emb, collapse = "\n"), "\n")
  body <- emb[-(1:2)]
  if (any(grepl("\\bno\\b", substr(body, 40, 60))))
    warning("At least one font is NOT embedded -- check before submission.")
} else {
  message("  pdffonts not available. Verify embedding manually before ",
          "submission (Acrobat: File > Properties > Fonts).")
}

message("\nDone. Width ", W, " cm, height ", H, " cm.")
