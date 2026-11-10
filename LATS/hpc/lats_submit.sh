#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: lats_submit.sh -l LIST -i IMAGE.sif [options] [-- LATS options]

Submit one Slurm array task per line of a list made by lats_make_lists.sh.
Each task runs lats_task.sh, which runs LATS in the Apptainer image.

Required:
  -l, --list FILE          longitudinal.tsv or cross-sectional.tsv
  -i, --image FILE         LATS Apptainer image (.sif)

Slurm:
  -p, --partition NAME
  -A, --account NAME
  -c, --cpus N             CPUs per run, also passed to LATS as -j (default: 1;
                           only 1 gives identical results when a run is repeated)
  -m, --mem SIZE           memory per run (default: 16G)
  -t, --time T             time limit per run (default: 04:00:00)
  -P, --max-parallel N     runs at once (default: 20)
  -J, --job-name NAME      (default: lats)
      --tasks SPEC         which lines to run, as a Slurm array range such as
                           1-50 or 3,7,12 (default: all lines)
      --logs DIR           Slurm logs and the frozen copy of the list
                           (default: ./lats_logs)
      --sbatch-args STR    extra sbatch options, e.g. "--qos=long --mail-type=FAIL"
      --qa DIR             QA mode: re-apply the corrected template labels in DIR
                           (a list made with lats_make_lists.sh --qa) instead of a full run
      --dry-run            print the sbatch command and submit nothing
  -h, --help               show this help

LATS options: everything after "--" replaces the default
  -M T1w -n --denoise --prior-refine --prior-probs
-o, -j and the images are set per run, so leave them out.

The list is copied into the logs folder at submission and the tasks read that
copy, so remaking the lists does not change a running job. A run whose
stats/volumes.csv exists is skipped, so resubmitting a list only redoes the
missing runs.
EOF
}

die() { echo "lats_submit.sh: $*" >&2; exit 2; }
need() { [ $# -ge 2 ] && [ -n "$2" ] || die "$1 needs a value"; }
abs() { case $1 in /*) echo "$1" ;; *) echo "$PWD/$1" ;; esac; }

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
task=$here/lats_task.sh
list= sif= partition= account= cpus=1 mem=16G time=04:00:00 maxpar=20 name=lats
tasks= logs=lats_logs extra= dry=0 qa=
lats_args=(-M T1w -n --denoise --prior-refine --prior-probs)

while [ $# -gt 0 ]; do
  case $1 in
    -l|--list) need "$@"; list=$2; shift ;;
    -i|--image) need "$@"; sif=$2; shift ;;
    -p|--partition) need "$@"; partition=$2; shift ;;
    -A|--account) need "$@"; account=$2; shift ;;
    -c|--cpus) need "$@"; cpus=$2; shift ;;
    -m|--mem) need "$@"; mem=$2; shift ;;
    -t|--time) need "$@"; time=$2; shift ;;
    -P|--max-parallel) need "$@"; maxpar=$2; shift ;;
    -J|--job-name) need "$@"; name=$2; shift ;;
    --tasks) need "$@"; tasks=$2; shift ;;
    --logs) need "$@"; logs=$2; shift ;;
    --sbatch-args) need "$@"; extra=$2; shift ;;
    --qa) need "$@"; qa=$2; shift ;;
    --dry-run) dry=1 ;;
    -h|--help) usage; exit 0 ;;
    --) shift; lats_args=("$@"); break ;;
    *) die "unknown option '$1' (see --help)" ;;
  esac
  shift
done

[ -n "$list" ] && [ -n "$sif" ] || { usage >&2; exit 2; }
[ -s "$list" ] || die "list is missing or empty: $list"
[ -f "$sif" ] || die "no such image: $sif"
[ -r "$task" ] || die "lats_task.sh must sit next to lats_submit.sh ($task)"
[[ $cpus =~ ^[1-9][0-9]*$ ]] || die "--cpus must be a positive whole number"
[[ $maxpar =~ ^[1-9][0-9]*$ ]] || die "--max-parallel must be a positive whole number"
awk -F'\t' 'NF < 4 || ($2 != "longitudinal" && $2 != "cross-sectional") { exit 1 }' "$list" \
  || die "$list is not a list from lats_make_lists.sh"
for a in "${lats_args[@]}"; do
  case $a in -o|--output-prefix|-j|--n-procs) die "LATS option $a is set per run; leave it out" ;; esac
done
[ ${#lats_args[@]} -gt 0 ] || die "no LATS options after --"
[ -z "$qa" ] || { [ -d "$qa" ] || die "no such QA directory: $qa"; qa=$(cd "$qa" && pwd -P); }

list=$(abs "$list")
sif=$(abs "$sif")
logs=$(abs "$logs")
n=$(grep -c '' "$list")
tasks=${tasks:-1-$n}
frozen=$logs/${name}_$(date +%Y%m%d-%H%M%S)_$(basename "$list")

cmd=(sbatch --parsable --job-name="$name" --array="${tasks}%${maxpar}"
     --cpus-per-task="$cpus" --mem="$mem" --time="$time"
     --output="$logs/${name}_%A_%a.out")
[ -z "$partition" ] || cmd+=(--partition="$partition")
[ -z "$account" ] || cmd+=(--account="$account")
[ -z "$qa" ] || cmd+=(--export=ALL,LATS_QA_DIR="$qa")
if [ -n "$extra" ]; then
  read -r -a extra_args <<<"$extra"
  cmd+=("${extra_args[@]}")
fi
cmd+=("$task" "$frozen" "$sif" "${lats_args[@]}")

echo "list: $list ($n runs; submitting lines $tasks, at most $maxpar at once)"
if [ "$dry" = 1 ]; then
  echo "dry run, nothing submitted or written. Command:"
  printf '%q ' "${cmd[@]}"
  echo
  exit 0
fi

mkdir -p "$logs"
cp "$list" "$frozen"
jobid=$("${cmd[@]}")
jobid=${jobid%%;*}
echo "submitted array job $jobid"
echo "frozen list: $frozen"
echo "logs: $logs/${name}_${jobid}_<line>.out"
