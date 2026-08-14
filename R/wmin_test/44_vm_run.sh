#!/usr/bin/env bash
# =============================================================================
# VM wrapper for R/wmin_test/44_rebuild_from_base.R
#
# Runs the full rebuild inside tmux, logs to disk, and fires an ntfy alert on
# completion -- success OR failure. Detaching the tmux session is safe; the job
# survives an ssh drop.
#
#   NTFY_TOPIC=my-topic ./R/wmin_test/44_vm_run.sh preflight
#   NTFY_TOPIC=my-topic ./R/wmin_test/44_vm_run.sh start
#   ./R/wmin_test/44_vm_run.sh status
#   ./R/wmin_test/44_vm_run.sh collect
#
# Run from the REPOSITORY ROOT. The script uses relative paths.
# =============================================================================
set -uo pipefail

SESSION="${SESSION:-mizer44}"
LOGDIR="Output_large_files/wmin_test"
LOG="$LOGDIR/44_full_run.log"
NTFY_TOPIC="${NTFY_TOPIC:-zoomss-someme-2026}"
NTFY_URL="${NTFY_URL:-https://ntfy.sh}"

export ARMS="${ARMS:-treated}"
export VM_ORDER="${VM_ORDER:-rank}"
export VM_CHUNK="${VM_CHUNK:-50}"
export VM_CORES="${VM_CORES:-30}"
export SAVE_STATES="${SAVE_STATES:-1}"
export VM_LIMIT="${VM_LIMIT:-0}"

notify () {   # notify <title> <priority> <message>
  [ -z "$NTFY_TOPIC" ] && { echo "[ntfy skipped: NTFY_TOPIC unset] $1 - $3"; return 0; }
  curl -fsS -H "Title: $1" -H "Priority: $2" -H "Tags: whale,chart_with_upwards_trend" \
       -d "$3" "$NTFY_URL/$NTFY_TOPIC" >/dev/null \
    || echo "[ntfy failed, job unaffected]"
}

case "${1:-start}" in

preflight)
  echo "=== preflight ==="
  if command -v tmux >/dev/null; then
    echo "  tmux       present"
  else
    echo "  tmux       NOT installed -- 'start' will fall back to nohup, which is"
    echo "             fine but gives no attachable session. To get one:"
    echo "               sudo apt-get install -y tmux   (or: sudo dnf install -y tmux)"
  fi
  Rscript --vanilla -e '
    ok <- TRUE
    for (p in c("mizer","therMizer","dplyr","reshape2")) {
      v <- tryCatch(as.character(packageVersion(p)), error = function(e) NA)
      cat(sprintf("  %-10s %s\n", p, ifelse(is.na(v), "MISSING", v)))
      if (is.na(v)) ok <<- FALSE
    }
    cat(sprintf("  %-10s %s\n", "R", getRversion()))
    cat(sprintf("  cores detected: %d\n", parallel::detectCores()))
    for (f in c("R/wmin_test/44_rebuild_from_base.R",
                "params_sel_adj_wmin_corrected_biocal.rds",
                "Output_large_files/wmin_test/43_member_draws.rds",
                "effort_array_1841_2010.rds",
                "yield_observed_timeseries.csv",
                "Manuscript data/yield_rmse_per_sim_deduped.csv")) {
      cat(sprintf("  %-58s %s\n", f, ifelse(file.exists(f), "ok", "MISSING")))
      if (!file.exists(f)) ok <<- FALSE
    }
    if (!ok) { cat("\nPREFLIGHT FAILED\n"); quit(status = 1) }
    cat("\nPREFLIGHT OK\n")' || exit 1

  echo
  echo "=== known-answer test: 6 members, random order, must match these ==="
  cat <<'EOF'
  sim_index      rmse          bw_1841
       1116  2.046299     1.667298e+11
        524  2.008199     6.029570e+11
       1983  2.083614     1.715258e+11
        989  2.079076     2.386908e+11
       2059  1.860046     3.162695e+12
        862  (rejected: steady_no_converge)
EOF
  echo
  echo "Running it now (~5 min). Results land in $LOGDIR/44_results/."
  rm -f "$LOGDIR"/44_results/*.rds "$LOGDIR"/44_states/*.rds 2>/dev/null
  ARMS=treated VM_ORDER=random VM_LIMIT=6 VM_CHUNK=6 VM_CORES=6 SAVE_STATES=0 \
    Rscript --vanilla R/wmin_test/44_rebuild_from_base.R run
  Rscript --vanilla -e '
    z <- readRDS(sort(list.files("Output_large_files/wmin_test/44_results",
                    pattern="^res_", full.names=TRUE))[1])
    s <- z$summary
    ref <- c(`1116`=2.046299, `524`=2.008199, `1983`=2.083614,
             `989`=2.079076, `2059`=1.860046)
    got <- setNames(s$treated_rmse, as.character(s$sim_index))[names(ref)]
    d <- max(abs(got - ref), na.rm = TRUE)
    cat(sprintf("\n  max absolute RMSE difference vs reference: %.3g\n", d))
    if (is.na(d) || d > 1e-5) {
      cat("  MISMATCH -- do NOT start the full run. The toolchain differs.\n"); quit(status=1)
    }
    cat("  MATCH. Toolchain reproduces the local results.\n")'
  st=$?
  echo
  echo "IMPORTANT: clear the smoke-test output before the real run:"
  echo "  rm -f $LOGDIR/44_results/*.rds $LOGDIR/44_states/*.rds"
  exit $st
  ;;

start)
  mkdir -p "$LOGDIR"
  n_done=$(ls "$LOGDIR"/44_results/res_*.rds 2>/dev/null | wc -l)
  echo "starting | arms=$ARMS cores=$VM_CORES chunk=$VM_CHUNK topic=$NTFY_TOPIC"
  echo "  $n_done chunk(s) already complete -- the job resumes, it does not restart"
  notify "mizer 44 started" "low" \
    "arms=$ARMS cores=$VM_CORES, resuming from $n_done chunk(s). ETA ~9 h."

  # Fallback when tmux is unavailable: nohup survives logout just as well, it
  # simply cannot be re-attached. Watch it with `tail -f` or `status` instead.
  if ! command -v tmux >/dev/null; then
    echo "  tmux not installed -- running under nohup instead"
    nohup bash -c "
      set -o pipefail
      Rscript --vanilla R/wmin_test/44_rebuild_from_base.R run 2>&1 | tee -a '$LOG'
      rc=\${PIPESTATUS[0]}
      t=\$(tail -n 3 '$LOG' | tr '\n' ' ')
      if [ \$rc -eq 0 ]; then
        curl -fsS -H 'Title: mizer 44 COMPLETE' -H 'Priority: high' -H 'Tags: white_check_mark' \
          -d \"Full rebuild finished. \$t\" '$NTFY_URL/$NTFY_TOPIC' >/dev/null || true
      else
        curl -fsS -H 'Title: mizer 44 FAILED' -H 'Priority: urgent' -H 'Tags: rotating_light' \
          -d \"Exit \$rc. \$t\" '$NTFY_URL/$NTFY_TOPIC' >/dev/null || true
      fi" >/dev/null 2>&1 &
    echo "  pid $!  |  watch: tail -f $LOG"
    exit 0
  fi

  if tmux has-session -t "$SESSION" 2>/dev/null; then
    echo "session '$SESSION' already exists. Attach with: tmux attach -t $SESSION"
    exit 1
  fi

  tmux new-session -d -s "$SESSION" \
    "set -o pipefail
     Rscript --vanilla R/wmin_test/44_rebuild_from_base.R run 2>&1 | tee -a '$LOG'
     rc=\${PIPESTATUS[0]}
     tail_txt=\$(tail -n 3 '$LOG' | tr '\n' ' ')
     if [ \$rc -eq 0 ]; then
       curl -fsS -H 'Title: mizer 44 COMPLETE' -H 'Priority: high' -H 'Tags: white_check_mark' \
         -d \"Full rebuild finished. \$tail_txt\" '$NTFY_URL/$NTFY_TOPIC' >/dev/null || true
     else
       curl -fsS -H 'Title: mizer 44 FAILED' -H 'Priority: urgent' -H 'Tags: rotating_light' \
         -d \"Exit \$rc. \$tail_txt\" '$NTFY_URL/$NTFY_TOPIC' >/dev/null || true
     fi
     echo; echo 'exit code:' \$rc
     echo 'window stays open so the log is readable; press q or close it.'
     read -r _"
  echo "detached. attach with:  tmux attach -t $SESSION   (detach again: Ctrl-b then d)"
  echo "log: $LOG"
  ;;

status)
  n=$(ls "$LOGDIR"/44_results/res_*.rds 2>/dev/null | wc -l)
  echo "completed chunks: $n"
  [ -f "$LOG" ] && tail -n 5 "$LOG"
  tmux has-session -t "$SESSION" 2>/dev/null \
    && echo "tmux session '$SESSION' is alive" \
    || echo "no tmux session '$SESSION'"
  ;;

collect)
  Rscript --vanilla R/wmin_test/44_rebuild_from_base.R collect 2>&1 | tee -a "$LOGDIR/44_collect.log"
  notify "mizer 44 collected" "default" "Ranking written to 44_rebuild_summary.csv"
  ;;

*)
  echo "usage: $0 {preflight|start|status|collect}"; exit 1 ;;
esac
