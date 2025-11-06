# Monte Carlo Analysis Helper Functions - Quick Reference

## 📋 Overview

This guide provides helper functions to analyze your 5000 Monte Carlo simulations, including:

1. **Parameter Distribution Analysis** - Prior vs Posterior comparisons
2. **Biomass Time Series** - Model uncertainty vs observations
3. **Yield Time Series** - Catch predictions with uncertainty
4. **Custom Analyses** - Correlations, stability, acceptance rates

---

## 🚀 Quick Start (30 seconds)

```r
# Load the helper functions
source("mc_analysis_helpers.R")

# Run complete analysis
results <- analyze_mc_results(
  base_save_path = "Output_large_files/mc_5k_blocks",
  obs_biomass_data = obs_biomass_data,
  obs_yield_data = yield_ts_tidy
)

# View plots
print(results$plots$gamma)
print(results$plots$biomass_timeseries)
```

**Done!** All plots saved to `mc_analysis_output/`

---

## 📚 Main Functions

### 1. `load_and_prepare_mc_results()`
Load MC results from block files

```r
mc_results <- load_and_prepare_mc_results(
  base_save_path = "Output_large_files/mc_5k_blocks",
  load_simulations = FALSE,  # TRUE = load full sims (high memory!)
  verbose = TRUE
)

# Returns:
# - $master: Master index with run info
# - $successful_params: Parameters from accepted sims
# - $all_attempted_params: All attempted parameters
# - $failed_params: Failed simulation details
```

### 2. `analyze_parameter_distributions()`
Analyze prior vs posterior distributions

```r
param_analysis <- analyze_parameter_distributions(
  mc_results = mc_results,
  catchability_sd = 2,  # Match your MC settings
  abundance_sd = 4,
  gamma_sd = 2
)

# Returns:
# - $gamma_comparison: Prior/posterior for gamma
# - $abundance_comparison: Prior/posterior for abundance
# - $catchability_comparison: Prior/posterior for catchability
# - $gamma_summary: Summary statistics
# - $abundance_summary: Summary statistics
```

### 3. `plot_prior_posterior_distributions()`
Create prior/posterior comparison plots

```r
# Gamma plot
p_gamma <- plot_prior_posterior_distributions(
  param_comparison = param_analysis$gamma_comparison,
  param_name = "gamma_change",
  title = "Gamma Parameter Distribution"
)

# Abundance plot
p_abundance <- plot_prior_posterior_distributions(
  param_comparison = param_analysis$abundance_comparison,
  param_name = "abundance_scaling",
  title = "Abundance Scaling Distribution"
)
```

### 4. `extract_biomass_timeseries()`
Extract biomass from all simulations

```r
biomass_df <- extract_biomass_timeseries(
  mc_results = mc_results,
  load_from_blocks = TRUE  # Load from disk (memory efficient)
)

# Returns data frame:
# Year | Species | Biomass | sim_id

# Save for later use
saveRDS(biomass_df, "biomass_timeseries.rds")
```

### 5. `plot_biomass_uncertainty()`
Plot biomass with uncertainty bounds

```r
p_biomass <- plot_biomass_uncertainty(
  biomass_df = biomass_df,
  obs_biomass_data = obs_biomass_data,
  obs_years = 2010:2020
)

# Shows:
# - 90% credible interval (light blue)
# - 50% credible interval (dark blue)
# - Median (navy line)
# - Observations (red points/line)
```

### 6. `extract_yield_timeseries()` & `plot_yield_uncertainty()`
Same as biomass, but for yield

```r
yield_df <- extract_yield_timeseries(mc_results, load_from_blocks = TRUE)

p_yield <- plot_yield_uncertainty(
  yield_df = yield_df,
  obs_yield_data = yield_ts_tidy
)
```

---

## 📊 Output Files Generated

### Plots (PNG format, 300 DPI)

1. **`gamma_prior_posterior.png`**
   - Prior vs posterior distribution for gamma (search rate)
   - Faceted by species
   - Log scale with reference line at 1× (no change)

2. **`abundance_prior_posterior.png`**
   - Prior vs posterior distribution for initial abundance
   - Faceted by species
   - Log scale with reference line at 1×

3. **`catchability_prior_posterior.png`**
   - Prior vs posterior distribution for catchability
   - Faceted by gear type
   - Log scale with reference line at 1×

4. **`biomass_timeseries_uncertainty.png`**
   - Full biomass time series (1841-2010)
   - Uncertainty ribbons (50% and 90% CI)
   - Observed data overlay (2010-2020)
   - Faceted by species

5. **`biomass_obs_period_comparison.png`**
   - Biomass comparison for 2010-2020 period only
   - Model median with 90% CI vs observations
   - Bar plot format for easy comparison

6. **`yield_timeseries_uncertainty.png`**
   - Yield time series with uncertainty
   - Observed data overlay
   - Focus on fished species

### Data Files (RDS format)

- **`biomass_timeseries_all_sims.rds`** - Complete biomass data frame
- **`yield_timeseries_all_sims.rds`** - Complete yield data frame
- **Summary CSV files** - Parameter statistics

---

## 🎯 Common Use Cases

### Case 1: Quick Overview
```r
source("mc_analysis_helpers.R")
results <- analyze_mc_results("Output_large_files/mc_5k_blocks", obs_biomass_data, yield_ts_tidy)
```

### Case 2: Focus on Specific Species
```r
mc_results <- load_and_prepare_mc_results("Output_large_files/mc_5k_blocks")
biomass_df <- extract_biomass_timeseries(mc_results)

# Filter to marine mammals
mammals <- c("minke whales", "orca", "sperm whales", "baleen whales")
biomass_mammals <- biomass_df %>% filter(Species %in% mammals)

p <- plot_biomass_uncertainty(biomass_mammals, obs_biomass_data)
```

### Case 3: Parameter Correlations
```r
param_analysis <- analyze_parameter_distributions(mc_results, 2, 4, 2)

# Extract wide format
param_wide <- param_analysis$successful_df %>%
  select(sim_id, species, gamma_change) %>%
  pivot_wider(names_from = species, values_from = gamma_change)

# Calculate correlations
cor_matrix <- cor(param_wide[-1], use = "pairwise.complete.obs")
```

### Case 4: Temporal Stability
```r
biomass_df <- readRDS("biomass_timeseries.rds")

stability <- biomass_df %>%
  group_by(Year, Species) %>%
  summarise(
    mean = mean(Biomass),
    sd = sd(Biomass),
    cv = sd / mean
  )

ggplot(stability, aes(Year, cv)) +
  geom_line() +
  facet_wrap(~Species)
```

### Case 5: Acceptance Rates
```r
param_analysis <- analyze_parameter_distributions(mc_results, 2, 4, 2)

acceptance <- param_analysis$gamma_comparison %>%
  group_by(species) %>%
  summarise(
    acceptance_rate = sum(distribution == "Posterior (Accepted)") / n()
  )
```

---

## 🔧 Memory Management

### Option A: Memory Efficient (Recommended)
```r
# Don't load full simulations
mc_results <- load_and_prepare_mc_results(
  base_save_path = "Output_large_files/mc_5k_blocks",
  load_simulations = FALSE  # ← Key setting
)

# Extract from block files as needed
biomass_df <- extract_biomass_timeseries(mc_results, load_from_blocks = TRUE)
```

**Memory usage:** ~100 MB

### Option B: Load Everything (High Memory)
```r
# Load all MizerSim objects
mc_results <- load_and_prepare_mc_results(
  base_save_path = "Output_large_files/mc_5k_blocks",
  load_simulations = TRUE  # ← Loads ~127 GB!
)

# Extract directly from memory
biomass_df <- extract_biomass_timeseries(mc_results, load_from_blocks = FALSE)
```

**Memory usage:** ~127 GB (2550 sims × 50 MB each)

*Note: For 5000 simulations at 51% success rate, you have ~2550 successful sims*

---

## 📐 Expected Data Structures

### Observed Biomass Data
```r
# Required columns:
Species      | ObsBiomass
-------------|-------------
"minke whales" | 1.5e12
"orca"         | 2.3e11
...
```

### Observed Yield Data (Tidy Format)
```r
# Required columns:
year | species        | yield
-----|----------------|----------
1950 | "baleen whales"| 5.2e9
1951 | "baleen whales"| 6.1e9
...
```

---

## ⚠️ Troubleshooting

### Problem: "Master index not found"
**Solution:**
```r
# Check your path
list.files("Output_large_files/mc_5k_blocks")  # Should show master_index.rds and block files

# Or use absolute path
mc_results <- load_and_prepare_mc_results("Output_large_files/mc_5k_blocks")
```

### Problem: "Out of memory"
**Solution:**
```r
# Use memory-efficient mode
mc_results <- load_and_prepare_mc_results(
  base_save_path = "Output_large_files/mc_5k_blocks",
  load_simulations = FALSE  # Don't load full sims
)

# Extract one metric at a time
biomass_df <- extract_biomass_timeseries(mc_results, load_from_blocks = TRUE)
saveRDS(biomass_df, "biomass.rds")  # Save immediately
rm(biomass_df)  # Free memory
gc()
```

### Problem: Missing block files
**Solution:**
```r
# Check which blocks are missing
mc_results <- load_and_prepare_mc_results("Output_large_files/mc_5k_blocks")
missing <- mc_results$master$block_files[!file.exists(mc_results$master$block_files)]
print(missing)

# Analysis will use available blocks only
```

### Problem: Plotting is slow
**Solution:**
```r
# Reduce data for plotting
biomass_sample <- biomass_df %>%
  filter(Year >= 1950) %>%  # Later years only
  filter(Species %in% c("minke whales", "orca"))  # Fewer species

plot_biomass_uncertainty(biomass_sample, obs_biomass_data)
```

---

## 🎨 Customizing Plots

### Change color scheme
```r
p <- plot_prior_posterior_distributions(...)
p + scale_fill_manual(values = c("gray60", "darkred"))
```

### Adjust facets
```r
p + facet_wrap(~species, ncol = 5)  # More columns
```

### Log scale adjustments
```r
p + scale_x_log10(breaks = c(0.1, 1, 10, 100))
```

### Font sizes
```r
p + theme(
  axis.text = element_text(size = 12),
  strip.text = element_text(size = 10)
)
```

---

## 📝 Example Workflow

```r
# 1. Load functions
source("mc_analysis_helpers.R")

# 2. Load data
mc_results <- load_and_prepare_mc_results("Output_large_files/mc_5k_blocks")
obs_biomass_data <- readRDS("obs_biomass_data.rds")
yield_ts_tidy <- readRDS("yield_ts_tidy.rds")

# 3. Parameter analysis
param_analysis <- analyze_parameter_distributions(mc_results, 2, 4, 2)

# 4. Create parameter plots
p1 <- plot_prior_posterior_distributions(param_analysis$gamma_comparison, "gamma_change")
p2 <- plot_prior_posterior_distributions(param_analysis$abundance_comparison, "abundance_scaling")
ggsave("gamma.png", p1, width = 14, height = 10)
ggsave("abundance.png", p2, width = 14, height = 10)

# 5. Extract time series
biomass_df <- extract_biomass_timeseries(mc_results, load_from_blocks = TRUE)
saveRDS(biomass_df, "biomass.rds")

# 6. Create biomass plots
p3 <- plot_biomass_uncertainty(biomass_df, obs_biomass_data)
p4 <- plot_biomass_obs_period(biomass_df, obs_biomass_data)
ggsave("biomass_ts.png", p3, width = 16, height = 12)
ggsave("biomass_obs.png", p4, width = 12, height = 8)

# 7. Yield analysis
yield_df <- extract_yield_timeseries(mc_results, load_from_blocks = TRUE)
p5 <- plot_yield_uncertainty(yield_df, yield_ts_tidy)
ggsave("yield_ts.png", p5, width = 12, height = 10)
```

**Total time:** ~10-15 minutes depending on disk speed

---

## 📚 Function Reference Table

| Function | Purpose | Memory | Time |
|----------|---------|--------|------|
| `load_and_prepare_mc_results()` | Load MC results | Low-High | 1-5 min |
| `analyze_parameter_distributions()` | Prior/posterior analysis | Low | <1 min |
| `plot_prior_posterior_distributions()` | Parameter plots | Low | <1 min |
| `extract_biomass_timeseries()` | Get biomass data | Medium | 5-10 min |
| `plot_biomass_uncertainty()` | Biomass plots | Low | 1-2 min |
| `extract_yield_timeseries()` | Get yield data | Medium | 5-10 min |
| `plot_yield_uncertainty()` | Yield plots | Low | 1-2 min |
| `analyze_mc_results()` | Complete analysis | Medium | 15-20 min |

---

## 🎓 Tips & Best Practices

1. **Save intermediate results** - Don't re-extract biomass/yield repeatedly
2. **Use memory-efficient mode** - Only load full sims if absolutely necessary
3. **Filter before plotting** - Reduce data size for faster rendering
4. **Check file paths** - Use absolute paths to avoid confusion
5. **Verify data structure** - Check observed data columns match expected format
6. **Save plots as you go** - Don't rely on keeping them all in memory
7. **Document your analysis** - Use comments and save analysis scripts

---

## 📞 Support

For issues or questions:
1. Check this reference guide
2. Look at `analyze_mc_results_EXAMPLE.R` for working examples
3. Review `MONTE_CARLO_CODE_ASSESSMENT.md` for compatibility issues

---

**Last Updated:** October 2025
