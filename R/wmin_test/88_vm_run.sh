#!/usr/bin/env bash
# =============================================================================
# VM wrapper for the 87 -> 88 -> 89 re-run.
#
# Same pattern as 44_vm_run.sh: runs inside tmux, logs to disk, and fires an
# ntfy alert on BOTH success and failure so silence never reads as success.
# Detaching is safe; the job survives an ssh drop.
#
#   ./R/wmin_test/88_vm_run.sh preflight    # 91_preflight.R, exits non-zero on fail
#   ./R/wmin_test/88_vm_run.sh start        # 87 + 88          (~12 h)
#   ./R/wmin_test/88_vm_run.sh refit        # 89 alone         (~1-2 h)
#   ./R/wmin_test/88_vm_run.sh all          # 87 + 88 + 89 chained
#   ./R/wmin_test/88_vm_run.sh status       # progress, safe at any time
#   ./R/wmin_test/88_vm_run.sh attach
#
# Each STAGE notifies separately, so a chained run tells you when 88 finished
# even though 89 is still going.
#
# Run from the REPOSITORY ROOT. All paths are relative to it.
#
# ENV  NTFY_TOPIC (zoomss-someme-2026), NTFY_URL, SESSION, CORES
# =============================================================================
set -uo pipefail

SESSION="${SESSION:-mizer88}"
LOGDIR="Output_large_files/wmin_test"
LOG="$LOGDIR/88_full_run.log"
JOB="$LOGDIR/88_job.sh"
NTFY_TOPIC="${NTFY_TOPIC:-zoomss-someme-2026}"
NTFY_URL="${NTFY_URL:-https://ntfy.sh}"
DRAWS_OUT="$LOGDIR/87_member_draws_substituted.rds"
STATE_DIR="$LOGDIR/88_full_states"
CHUNK_DIR="$LOGDIR/88_full_chunks"

# Leave 2 cores free. Saturating every core risks a hard shutdown, which kills
# the in-flight run as well as the machine.
NCORE=$(nproc 2>/dev/null || echo 4)
CORES="${CORES:-$(( NCORE > 2 ? NCORE - 2 : 1 ))}"

if [ ! -f "R/wmin_test/88_full_ensemble.R" ]; then
  echo "STOP: run this from the repository root (no R/wmin_test/88_full_ensemble.R here)."
  exit 1
fi

notify () {   # notify <title> <priority> <tags> <message>
  [ -z "$NTFY_TOPIC" ] && { echo "[ntfy skipped] $1 - $4"; return 0; }
  curl -fsS -H "Title: $1" -H "Priority: $2" -H "Tags: $3" \
       -d "$4" "$NTFY_URL/$NTFY_TOPIC" >/dev/null \
    || echo "[ntfy failed, job unaffected]"
}

# Write the job body to a file rather than inlining it in the tmux command.
# Three chained stages with their own notifications do not survive the quoting.
build_job () {
  mkdir -p "$LOGDIR"
  {
    echo '#!/usr/bin/env bash'
    echo 'set -uo pipefail'
    printf 'cd %q || exit 1\n' "$(pwd)"
    printf 'LOG=%q\n' "$LOG"
    printf 'NTFY_URL=%q\n' "$NTFY_URL"
    printf 'NTFY_TOPIC=%q\n' "$NTFY_TOPIC"
    printf 'CORES=%q\n' "$CORES"
    cat <<'HEADER'
say () {   # say <title> <priority> <tags> <message>
  [ -z "$NTFY_TOPIC" ] && return 0
  curl -fsS -H "Title: $1" -H "Priority: $2" -H "Tags: $3" \
       -d "$4" "$NTFY_URL/$NTFY_TOPIC" >/dev/null || true
}
stage () {  # stage <label> <command...>
  local label="$1"; shift
  local t0=$SECONDS
  echo "=== $label : $(date -Is) ===" | tee -a "$LOG"
  "$@" 2>&1 | tee -a "$LOG"
  local rc=${PIPESTATUS[0]}
  local mins=$(( (SECONDS - t0) / 60 ))
  local tail_txt
  tail_txt=$(tail -n 4 "$LOG" | tr '\n' ' ' | cut -c1-400)
  if [ "$rc" -eq 0 ]; then
    say "mizer 88: $label DONE" high white_check_mark \
        "${mins} min. $tail_txt"
  else
    say "mizer 88: $label FAILED" urgent rotating_light \
        "exit $rc after ${mins} min. $tail_txt"
  fi
  return $rc
}
HEADER
    for s in "$@"; do
      case "$s" in
        87) cat <<'S87'
if [ -f "Output_large_files/wmin_test/87_member_draws_substituted.rds" ]; then
  echo "87: substituted draws already exist -- skipping (P87_FORCE=1 to redo)" \
    | tee -a "$LOG"
else
  stage "87 substitute draws" Rscript R/wmin_test/87_substitute_draws.R || exit 1
fi
S87
        ;;
        88) cat <<'S88'
# `env` is not decoration: "VAR=x stage ..." puts the assignment on the FUNCTION
# call, and bash does not reliably export it through to the Rscript inside.
stage "88 build 1668 members" \
  env P61_CORES="$CORES" Rscript R/wmin_test/88_full_ensemble.R run || exit 1
S88
        ;;
        89) cat <<'S89'
stage "89 catchability refit" \
  env P89_CORES="$CORES" Rscript R/wmin_test/89_catchability_refit_2004.R || exit 1
S89
        ;;
      esac
    done
    cat <<'FOOTER'
say "mizer 88: ALL STAGES COMPLETE" high tada "Nothing left to run."
echo; echo "all stages complete. window stays open so the log is readable."
read -r _
FOOTER
  } > "$JOB"
  chmod +x "$JOB"
}

launch () {   # launch <label> <stages...>
  local label="$1"; shift
  mkdir -p "$LOGDIR"
  if tmux has-session -t "$SESSION" 2>/dev/null; then
    echo "session '$SESSION' already exists -- refusing to start a second job."
    echo "  attach : tmux attach -t $SESSION"
    echo "  kill   : tmux kill-session -t $SESSION"
    exit 1
  fi
  build_job "$@"
  local ndone=0
  [ -d "$CHUNK_DIR" ] && ndone=$(ls "$CHUNK_DIR"/res_*.rds 2>/dev/null | wc -l)
  echo "starting '$label' | cores=$CORES of $NCORE | topic=$NTFY_TOPIC"
  echo "  $ndone chunk(s) already complete -- 88 RESUMES, it does not restart"
  notify "mizer 88 started" low hourglass \
    "$label | cores=$CORES, resuming from $ndone chunk(s)."

  if ! command -v tmux >/dev/null; then
    echo "  tmux not installed -- falling back to nohup (not attachable)"
    echo "    sudo apt-get install -y tmux"
    nohup "$JOB" >/dev/null 2>&1 &
    echo "  pid $!  |  watch: tail -f $LOG"
    exit 0
  fi
  tmux new-session -d -s "$SESSION" "$JOB"
  echo "detached. attach: tmux attach -t $SESSION   (detach again: Ctrl-b then d)"
  echo "log: $LOG"
}

case "${1:-status}" in

preflight)
  Rscript R/wmin_test/91_preflight.R
  st=$?
  if [ $st -ne 0 ]; then
    notify "mizer 88 preflight FAILED" high rotating_light \
      "Not safe to start. See the output."
  else
    command -v tmux >/dev/null || echo "
NOTE: tmux is not installed. 'start' still works under nohup, but the session
cannot be re-attached. To get one: sudo apt-get install -y tmux"
  fi
  exit $st
  ;;

start) launch "87 + 88" 87 88 ;;
refit) launch "89 refit" 89 ;;
all)   launch "87 + 88 + 89" 87 88 89 ;;

dry-run)
  build_job 87 88 89
  echo "=== generated job: $JOB ==="
  cat "$JOB"
  echo "=== syntax check ==="
  bash -n "$JOB" && echo "OK -- valid bash, nothing was launched"
  ;;

status)
  echo "=== phase 88 ==="
  # P61_CORES MUST match the running job. Chunk boundaries are
  # split(members, ceiling(seq_along(members) / CORES)), so a different core
  # count yields a different number of chunks and the progress figure is
  # meaningless -- status at the default 10 reported "1/167" against a real
  # "1/56". The same mismatch would also break a RESUME, because completed
  # chunk files would be matched to the wrong member ranges.
  env P61_CORES="$CORES" Rscript R/wmin_test/88_full_ensemble.R status 2>/dev/null \
    || echo "  (could not read status -- run not started?)"
  [ -f "$DRAWS_OUT" ] && echo "87 output: present" || echo "87 output: absent"
  echo
  if tmux has-session -t "$SESSION" 2>/dev/null; then
    echo "tmux session '$SESSION' is ALIVE"
  else
    echo "no tmux session '$SESSION' -- finished, failed, or never started"
  fi
  echo
  [ -f "$LOG" ] && { echo "--- last 12 log lines ---"; tail -n 12 "$LOG"; }
  ;;

attach)
  tmux attach -t "$SESSION" \
    || echo "no session '$SESSION'. Start one with: $0 start"
  ;;

test-ntfy)
  notify "mizer 88 test" default bell \
    "If you can read this, notifications are working. Topic: $NTFY_TOPIC"
  echo "sent to $NTFY_URL/$NTFY_TOPIC"
  ;;

*)
  echo "usage: $0 {preflight|start|refit|all|status|attach|test-ntfy}"
  exit 1 ;;
esac