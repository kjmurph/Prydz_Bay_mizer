# Figure 1 implementation spec

Prydz Bay manuscript, Science submission. Panels A, B, C.

This document specifies what to build. It does not specify where things live in the
repository, because that must be discovered rather than assumed.

---

## 1. Ground rules

Read these before writing any code.

1. **Do not fabricate data.** Every number, ribbon, and point in this figure must
   trace to an object in the repository or a saved ensemble output. If you cannot
   find the object you need, stop and ask. Do not substitute simulated or
   placeholder data to make the script run.
2. **Do not invent object or column names.** Inspect the actual ensemble outputs
   and use the real names.
3. **Do not modify the ensemble.** This is a plotting task. Nothing in this work
   should rewrite, refilter, or regenerate ensemble members.
4. **Ask before deviating.** If the spec conflicts with what the data actually
   supports, raise it rather than silently adapting.
5. **One script per panel, plus one assembly script.** Do not build a single
   monolithic file.

---

## 2. Repository discovery

Before implementing, establish and report back:

- Where the accepted ensemble (n = 2,111) is stored, and its format.
- Where the RMSE-fitted subset (n = 212) is stored, or how membership is flagged.
- Whether paired exploited and unexploited runs are stored as separate objects or
  as a single object with a scenario column, and what the pairing key is.
- Which object holds the community size spectrum output used for panel B.
- The existing panel A script and the objects it consumes.
- The existing ggplot theme functions built for the Nature-portfolio submission.
- Available time coverage and the terminal year of the ensemble runs.

Report these before proceeding.

---

## 3. Ensemble terminology

Use this vocabulary consistently in code, comments, and the figure legend. The
distinction matters and must not be blurred.

| Term | n | Definition |
|---|---|---|
| Accepted ensemble | 2,111 | Members passing rejection criteria: negative size-spectrum slope, biomass stability criteria |
| Fitted ensemble | 212 | Top 10% of the accepted ensemble by RMSE of modelled catches against observed fishery and whaling catches |

**Panel C uses the fitted ensemble (n = 212).** State the derivation chain in the
legend so a reader can reconstruct it.

Each fitted member has a paired unexploited counterfactual run, identical in
parameters and climate forcing, differing only in that fishing and whaling
mortality are absent. The pairing is the analytical basis of the panel.

---

## 4. Deliverables

```
R/figures/fig1/
  01_theme_fig1.R        shared theme, palette, taxon colour mapping
  02_panel_a.R           returns a ggplot object
  03_panel_b.R           returns a ggplot object
  04_panel_c.R           returns a ggplot object (patchwork of two sub-plots)
  05_assemble_fig1.R     composes and exports
output/figures/
  fig1.pdf               vector, primary deliverable
  fig1.svg               vector, for any downstream Illustrator work
  fig1.png               600 dpi, for drafts and circulation
```

Each panel script must expose a function (`panel_a()`, `panel_b()`, `panel_c()`)
returning a ggplot or patchwork object. No side effects, no printing, no writing
files from panel scripts.

---

## 5. Shared style layer (`01_theme_fig1.R`)

### Journal format

- Target width: 18.4 cm (three columns). Design to that width.
- Maximum height 22.7 cm. Aim for roughly 16 to 18 cm.
- Background white throughout. No dark theme.
- Sans-serif, Helvetica or Arial. Set via `systemfonts`, with `ragg` for raster
  device output.
- Base text size 7 pt. Nothing below 6 pt after final scaling. Verify by measuring
  the exported file, not by eye in the RStudio pane.
- Panel tags uppercase bold: (A), (B), (C). Use
  `patchwork::plot_annotation(tag_levels = "A")`, not manual annotation.

Build `theme_science()` by adapting the existing Nature-portfolio theme. Keep the
old theme intact, do not overwrite it.

### Palette

Use a colourblind-safe categorical palette (Okabe-Ito is the default choice). Define
a single named vector mapping taxon to colour, and use it in both panel A and panel
B so that a taxon is the same colour everywhere in the figure. This consistency is
the strongest feature of the current draft and must be preserved.

Define separately:

- `col_exploited` and `col_unexploited` for panel C. These are scenario colours,
  not taxon colours, and must be visually distinct from every taxon colour. Suggest
  a warm hue for exploited and neutral grey for unexploited.
- `col_paired` for the within-pair difference distribution, distinct from both.

Do not rely on colour alone. Pair the exploited and unexploited ribbons with
different line types on their median lines.

---

## 6. Panel A: observed catch time series

**Scope: minimal change. Do not redesign this panel.**

Kieran has an existing figure and wants it left substantially as is for now.

Permitted changes only:

1. Apply `theme_science()` in place of the Nature-portfolio theme.
2. Apply the shared taxon palette.
3. Remove any panel-internal title. The legend carries the description.
4. Convert the era dividers from dashed vertical lines to translucent shaded bands
   if this can be done without disturbing the existing geometry. If it looks worse,
   revert to the dashed lines and say so.
5. Ensure the legend is shared with panel B rather than drawn twice. Collect it at
   the figure level via `patchwork` with `guides = "collect"`.

Keep it as a single panel with all species on one set of axes. Do not split it, do
not add a broken axis, do not switch to a log scale.

---

## 7. Panel B: community size spectrum

**Plot the actual constructed size spectrum.** No artistic smoothing, no idealised
straight line standing in for real output.

### Content

1. Abundance (or biomass density, match whatever the manuscript uses elsewhere)
   against body mass, both on log10 axes.
2. The realised community spectrum from the model output, drawn as the primary
   data layer. Retain its structure. The species humps are informative and should
   not be smoothed away.
3. The fitted community slope lambda overlaid as a single straight line, with the
   fitted value annotated. Use the same slope estimation method the manuscript uses
   for the later slope time series. Do not introduce a second, inconsistent method
   here. Confirm which function that is before implementing.
4. Optionally, a second fitted line for a comparison state if one is readily
   available, showing the steepening under exploitation. Only include this if it
   comes from real output. If it requires fabrication, omit it.

### Silhouettes

Use `rphylopic`. Place silhouettes at each taxon's actual body mass position on the
x-axis, so the placement is data-driven rather than decorative.

- `rphylopic::get_uuid()` to resolve taxa, `geom_phylopic()` to place them with
  `x`, `y`, and `size` mapped to data coordinates.
- Colour each silhouette with the shared taxon palette so panels A and B agree.
- **Record provenance.** For every silhouette used, write UUID, contributor, and
  licence to `output/figures/phylopic_credits.csv`. Science will require this
  cleared, and reconstructing it later is painful.
- Where `rphylopic` has no suitable Antarctic taxon, leave a labelled gap and note
  it in the credits file. Kieran has his own silhouettes and will substitute them
  later. Do not approximate with a wrong taxon.

Keep silhouettes small enough not to obscure the data. They annotate the spectrum,
they do not replace the points.

---

## 8. Panel C: the counterfactual contrast

This is the panel doing the most work. Its purpose is to prepare the reader for
the exploited versus unexploited contrast that carries the main results. The reader
should finish this panel already understanding what a paired counterfactual is and
why the comparison is well posed, so that the later results figures need no
methodological preamble.

Build it as two stacked sub-plots combined with `patchwork`, sharing a common
vertical alignment. Tag the whole thing (C).

### 8.1 Upper sub-plot: ensemble trajectories

For each of the 212 fitted members and its paired unexploited counterfactual, plot
the response variable through time.

- Two ribbon sets: exploited and unexploited.
- Each shows the ensemble median as a line, with 50% and 90% quantile ribbons
  computed across members at each time step.
- Before the onset of exploitation the two are identical by construction. Do not
  force this cosmetically. If the plotted lines diverge before the first catch
  year, that is a bug worth reporting.
- Mark the whaling and krill fishery onsets consistent with panel A, using the same
  visual convention.
- Annotate the terminal-year gap between the two medians with a bracket, labelled
  to match the manuscript's "ecological deficit" framing. Use
  `ggforce::geom_mark_bracket()` or a manual segment with arrowheads at both ends.

Parameterise the response variable:

```r
panel_c(metric = c("biomass", "lambda"))
```

Produce the biomass version first. Confirm the layout with Kieran before extending
to lambda. If both are wanted in the final figure, they become two rows sharing an
x-axis, and the overall figure height needs rechecking against the 22.7 cm limit.

### 8.2 Lower sub-plot: spread and signal-to-noise

This is the part that justifies the panel's space, and it must be constructed
carefully because a level and a difference are not the same quantity.

**Put all three distributions on one axis by expressing everything in log units
relative to the unexploited ensemble median at the terminal year.**

For each fitted member `i` at the terminal year, with `E_i` the exploited value and
`U_i` the paired unexploited value, and `m = median(log(U))`:

| Row | Quantity plotted | Expected appearance |
|---|---|---|
| Exploited members | `log(E_i) - m` | Wide, offset below zero |
| Unexploited members | `log(U_i) - m` | Wide, centred on zero by construction |
| Within-pair difference | `log(E_i) - log(U_i)` | Narrow, offset below zero by a similar amount |

All three are in log-ratio units and are legitimately comparable in width. Draw a
dashed reference line at zero.

Render with `ggdist`. `stat_halfeye()` or `stat_slabinterval()` both work; choose
whichever reads better at 7 pt. Colour rows one and two with the scenario colours,
row three with `col_paired`.

The visual argument is that rows one and two overlap substantially, while row three
is narrow and clearly displaced from zero. Between-member parameter uncertainty
cancels within pairs. This pre-empts the obvious reviewer objection, which is that
the ensembles overlap so no signal can be claimed.

Compute and annotate a signal-to-noise ratio:

- Paired: `median(log(E_i) - log(U_i)) / sd(log(E_i) - log(U_i))`
- Unpaired, for contrast: the same numerator over the pooled SD of the marginals.

Report both to the console. Annotate the paired value on the plot. Do not
hard-code either number, compute them from the data at build time.

### 8.3 What not to do

Do not build a flowchart. No boxes, no arrows describing the pipeline, no
"2,111 sims to top 10%" schematic. The derivation chain belongs in the legend and
the methods. The panel shows the counterfactual, it does not diagram the procedure.

---

## 9. Assembly (`05_assemble_fig1.R`)

Layout, using `patchwork`:

```
A spans the full width
B and C side by side beneath, roughly equal width
```

Requirements:

- `plot_annotation(tag_levels = "A")` for uppercase bold tags.
- `guides = "collect"` so the taxon legend appears once, shared by A and B.
- Panel C carries its own scenario legend, placed inside the panel if space allows.
- Consistent margins. No panel should have visibly more whitespace than another.

---

## 10. Export

Use `svglite` for SVG and `ragg` or `cairo_pdf` for the other formats. Export at
exactly 18.4 cm width.

```r
ggsave("output/figures/fig1.pdf", fig1, width = 18.4, height = <h>, units = "cm", device = cairo_pdf)
ggsave("output/figures/fig1.svg", fig1, width = 18.4, height = <h>, units = "cm", device = svglite::svglite)
ggsave("output/figures/fig1.png", fig1, width = 18.4, height = <h>, units = "cm", dpi = 600)
```

Fonts must be embedded in the PDF. Verify, do not assume.

---

## 11. Acceptance checklist

Work through this before reporting completion.

- [ ] Every plotted value traces to real repository data. Nothing simulated.
- [ ] Panel C uses the fitted ensemble, n = 212, and this is verified in code.
- [ ] Exploited and unexploited trajectories are identical prior to the first
      catch year.
- [ ] Taxon colours are identical between panels A and B.
- [ ] Panel tags are uppercase bold (A), (B), (C).
- [ ] Exported at 18.4 cm width, total height within 22.7 cm.
- [ ] Smallest text in the exported PDF measures at least 6 pt.
- [ ] Figure is legible when printed at actual size on white paper. Print it.
- [ ] Readable in greyscale, or every colour distinction is redundantly encoded by
      line type or position.
- [ ] `phylopic_credits.csv` exists and covers every silhouette used.
- [ ] Signal-to-noise values are computed at build time, not hard-coded.
- [ ] Panel A geometry is substantively unchanged from the existing version.

---

## 12. Questions to raise before finishing

1. Which slope estimation function is canonical for lambda, so panel B matches the
   later slope time series?
2. What is the terminal year for the spread comparison, and does it match the year
   used for the headline result in the manuscript?
3. Should panel C show biomass only, or biomass and lambda as two rows?
4. Are the exploited and unexploited runs stored paired, and what is the join key?
5. Is total community biomass the right response variable, or a specific
   size class or functional group?
