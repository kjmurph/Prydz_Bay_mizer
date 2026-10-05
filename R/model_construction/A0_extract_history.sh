#!/usr/bin/env bash
# A0_extract_history.sh -- copy the IWC intermediates that now exist only in
# git history into $ASSESS_OUT/history (default
# Output_large_files/model_construction/history), where R4 and R5 read them.
#
# Run from the repository root under Git Bash. git show's output is
# binary-safe here; PowerShell's > redirection is not, and would corrupt the
# .rds files. Nothing is restored into the working tree.
#
#   catch_timeseries_BanzareBank_1930_2019_CPUE.rds and three siblings:
#     added in f54387b (2023-04-21), removed in eff162e (2023-09-27)
#   fish_catch.csv: added in a2e58d2 (2023-06-21), removed in eff162e
#
# These are derived from IWC individual-catch records: check the IWC data terms
# before redistributing any of them.
set -euo pipefail
OUT="${ASSESS_OUT:-Output_large_files/model_construction}/history"
mkdir -p "$OUT"
for f in catch_timeseries_BanzareBank_1930_2019_CPUE.rds \
         ind_catch_weight_BanzareBank_1930_2019_CPUE.rds \
         ind_catch_weight_BanzareBank_1930_2019.rds \
         catch_timeseries_BanzareBank_1930_2019.csv; do
  git show "f54387b:$f" > "$OUT/$f"
done
git show "a2e58d2:fish_catch.csv" > "$OUT/fish_catch.csv"
# expected: catch_timeseries_BanzareBank_1930_2019_CPUE.rds b92bf815fb781563fa906d6f49d6a8f6
md5sum "$OUT"/*
