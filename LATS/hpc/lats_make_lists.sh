#!/usr/bin/env bash
set -euo pipefail
shopt -s nullglob

usage() {
  cat <<'EOF'
Usage: lats_make_lists.sh -b BIDS_DIR [options]

Find one image per session in a BIDS dataset and write the run lists that
lats_submit.sh submits:
  longitudinal.tsv     one LATS run per subject over all their sessions
  cross-sectional.tsv  one single-session LATS run per subject
  excluded.tsv         subjects and sessions left out, with the reason
  summary.txt          counts and the command that made the lists

Required:
  -b, --bids DIR              BIDS dataset root

Options:
  -o, --out DIR               where to write the lists (default: ./lats_lists)
  -d, --derivatives DIR       LATS output root (default: BIDS_DIR/derivatives/lats);
                              runs go to DIR/longitudinal/sub-X and
                              DIR/cross-sectional/sub-X_ses-Y
  -s, --suffix STR            image suffix (default: T1w)
  -e, --ext STR               image extension (default: .nii.gz)
      --acq LABEL             only images with acq-LABEL
      --rec LABEL             only images with rec-LABEL
      --run LABEL             only images with run-LABEL
      --multi MODE            when a session has more than one matching image:
                              skip (default; listed in excluded.tsv), first or
                              last (in natural sort order of the file names)
  -p, --participant-label L   only these subjects, comma separated, with or
                              without "sub-" (repeatable)
      --exclude L             leave out these subjects (repeatable)
      --sessions L            only these sessions, comma separated, with or
                              without "ses-"
      --min-sessions N        sessions needed for a longitudinal run (default: 2)
      --max-sessions N        use at most the first N sessions (default: all)
      --cross MODE            which subjects get a cross-sectional run:
                              single (default) those with fewer than
                                --min-sessions sessions,
                              all     every subject, longitudinal ones too,
                              none    no cross-sectional runs
      --cross-session LABEL   session used for cross-sectional runs (default: 01)
      --skip-done             leave out runs whose stats/volumes.csv already exists
  -h, --help                  show this help

List format: tab separated, one run per line, no header:
  run_id  kind  output_dir  image_1  [image_2 ...]
Images are in session order (natural sort of the ses- labels).
EOF
}

die() { echo "lats_make_lists.sh: $*" >&2; exit 2; }
need() { [ $# -ge 2 ] && [ -n "$2" ] || die "$1 needs a value"; }
labels() { local x; for x in ${1//,/ }; do echo "${x#"$2"-}"; done; }
in_list() { local x=$1 y; shift; for y in "$@"; do [ "$x" = "$y" ] && return 0; done; return 1; }
abs() { case $1 in /*) echo "$1" ;; *) echo "$PWD/$1" ;; esac; }

if [ "${BASH_VERSINFO[0]}" -lt 4 ] || { [ "${BASH_VERSINFO[0]}" -eq 4 ] && [ "${BASH_VERSINFO[1]}" -lt 4 ]; }; then
  die "needs bash 4.4 or newer (this is $BASH_VERSION)"
fi

args=("$@")
bids= out=lats_lists deriv= suffix=T1w ext=.nii.gz acq= rec= run= multi=skip
min_ses=2 max_ses=0 cross=single cross_ses=01 skip_done=0
include=() exclude=() sessions=()

while [ $# -gt 0 ]; do
  case $1 in
    -b|--bids) need "$@"; bids=$2; shift ;;
    -o|--out) need "$@"; out=$2; shift ;;
    -d|--derivatives) need "$@"; deriv=$2; shift ;;
    -s|--suffix) need "$@"; suffix=$2; shift ;;
    -e|--ext) need "$@"; ext=$2; shift ;;
    --acq) need "$@"; acq=$2; shift ;;
    --rec) need "$@"; rec=$2; shift ;;
    --run) need "$@"; run=$2; shift ;;
    --multi) need "$@"; multi=$2; shift ;;
    -p|--participant-label) need "$@"; include+=($(labels "$2" sub)); shift ;;
    --exclude) need "$@"; exclude+=($(labels "$2" sub)); shift ;;
    --sessions) need "$@"; sessions+=($(labels "$2" ses)); shift ;;
    --min-sessions) need "$@"; min_ses=$2; shift ;;
    --max-sessions) need "$@"; max_ses=$2; shift ;;
    --cross) need "$@"; cross=$2; shift ;;
    --cross-session) need "$@"; cross_ses=${2#ses-}; shift ;;
    --skip-done) skip_done=1 ;;
    -h|--help) usage; exit 0 ;;
    *) die "unknown option '$1' (see --help)" ;;
  esac
  shift
done

[ -n "$bids" ] || { usage >&2; exit 2; }
[ -d "$bids" ] || die "no such directory: $bids"
bids=$(cd "$bids" && pwd -P)
deriv=$(abs "${deriv:-$bids/derivatives/lats}")
case $multi in skip|first|last) ;; *) die "--multi must be skip, first or last" ;; esac
case $cross in single|all|none) ;; *) die "--cross must be single, all or none" ;; esac
[[ $min_ses =~ ^[0-9]+$ ]] && [ "$min_ses" -ge 2 ] || die "--min-sessions must be a whole number, at least 2"
[[ $max_ses =~ ^[0-9]+$ ]] || die "--max-sessions must be a whole number"
[ "$max_ses" -eq 0 ] || [ "$max_ses" -ge "$min_ses" ] || die "--max-sessions must be at least --min-sessions"

subs=("$bids"/sub-*/)
[ ${#subs[@]} -gt 0 ] || die "no sub-* folders in $bids"

mkdir -p "$out"
long_tsv=$out/longitudinal.tsv
cross_tsv=$out/cross-sectional.tsv
excl_tsv=$out/excluded.tsv
: >"$long_tsv"
: >"$cross_tsv"
printf 'subject\tsession\treason\n' >"$excl_tsv"

n_sub=0 n_long=0 n_cross=0 n_excl=0 n_done=0
declare -A per_n=()

excl() { printf '%s\t%s\t%s\n' "$1" "$2" "$3" >>"$excl_tsv"; n_excl=$((n_excl + 1)); }

emit() {
  local file=$1 id=$2 kind=$3 dir=$4
  shift 4
  if [ "$skip_done" = 1 ] && [ -s "$dir/stats/volumes.csv" ]; then
    n_done=$((n_done + 1))
    return 1
  fi
  (IFS=$'\t'; echo "$id"$'\t'"$kind"$'\t'"$dir"$'\t'"$*") >>"$file"
}

for d in "${subs[@]}"; do
  sub=$(basename "$d")
  label=${sub#sub-}
  if [ ${#include[@]} -gt 0 ] && ! in_list "$label" "${include[@]}"; then continue; fi
  if [ ${#exclude[@]} -gt 0 ] && in_list "$label" "${exclude[@]}"; then continue; fi
  n_sub=$((n_sub + 1))

  ses_glob=("$d"ses-*/)
  if [ ${#ses_glob[@]} -eq 0 ]; then
    excl "$sub" - "no ses-* folders"
    continue
  fi
  mapfile -t ses_dirs < <(printf '%s\n' "${ses_glob[@]}" | sort -V)

  imgs=() labs=()
  for s in "${ses_dirs[@]}"; do
    ses=$(basename "$s")
    sl=${ses#ses-}
    if [ ${#sessions[@]} -gt 0 ] && ! in_list "$sl" "${sessions[@]}"; then continue; fi
    cands=()
    for f in "$s"anat/"${sub}_${ses}"*"_${suffix}${ext}"; do
      b=$(basename "$f")
      [ -z "$acq" ] || [[ $b == *"_acq-${acq}_"* ]] || continue
      [ -z "$rec" ] || [[ $b == *"_rec-${rec}_"* ]] || continue
      [ -z "$run" ] || [[ $b == *"_run-${run}_"* ]] || continue
      cands+=("$f")
    done
    if [ ${#cands[@]} -eq 0 ]; then
      excl "$sub" "$ses" "no *_${suffix}${ext} in anat/ that passes the filters"
      continue
    fi
    if [ ${#cands[@]} -gt 1 ]; then
      mapfile -t cands < <(printf '%s\n' "${cands[@]}" | sort -V)
      case $multi in
        skip) excl "$sub" "$ses" "${#cands[@]} matching images; choose with --acq, --rec, --run or --multi"; continue ;;
        last) cands=("${cands[${#cands[@]}-1]}") ;;
      esac
    fi
    imgs+=("${cands[0]}")
    labs+=("$sl")
  done

  n=${#imgs[@]}
  if [ "$n" -eq 0 ]; then
    excl "$sub" - "no usable session"
    continue
  fi
  if [ "$max_ses" -gt 0 ] && [ "$n" -gt "$max_ses" ]; then
    for sl in "${labs[@]:max_ses}"; do excl "$sub" "ses-$sl" "beyond --max-sessions $max_ses"; done
    imgs=("${imgs[@]:0:max_ses}")
    labs=("${labs[@]:0:max_ses}")
    n=$max_ses
  fi

  if [ "$n" -ge "$min_ses" ]; then
    if emit "$long_tsv" "$sub" longitudinal "$deriv/longitudinal/$sub" "${imgs[@]}"; then
      n_long=$((n_long + 1))
      per_n[$n]=$(( ${per_n[$n]:-0} + 1 ))
    fi
  fi

  if [ "$cross" = all ] || { [ "$cross" = single ] && [ "$n" -lt "$min_ses" ]; }; then
    k=-1
    for i in "${!labs[@]}"; do [ "${labs[$i]}" = "$cross_ses" ] && k=$i; done
    if [ "$k" -ge 0 ]; then
      id=${sub}_ses-${cross_ses}
      if emit "$cross_tsv" "$id" cross-sectional "$deriv/cross-sectional/$id" "${imgs[$k]}"; then
        n_cross=$((n_cross + 1))
      fi
    elif [ "$n" -lt "$min_ses" ]; then
      excl "$sub" "ses-$(IFS=,; echo "${labs[*]}")" "fewer than $min_ses sessions and no ses-$cross_ses"
    fi
  elif [ "$n" -lt "$min_ses" ]; then
    excl "$sub" "ses-$(IFS=,; echo "${labs[*]}")" "fewer than $min_ses sessions (--cross none)"
  fi
done

{
  echo "lats_make_lists.sh, $(date '+%Y-%m-%d %H:%M:%S')"
  printf 'command:'
  printf ' %q' "$0" "${args[@]}"
  echo
  echo "bids: $bids"
  echo "derivatives: $deriv"
  echo "image: anat/sub-X_ses-Y*_${suffix}${ext}${acq:+, acq-$acq}${rec:+, rec-$rec}${run:+, run-$run}; --multi $multi"
  echo "subjects considered: $n_sub"
  echo "longitudinal runs: $n_long"
  for k in $(printf '%s\n' "${!per_n[@]}" | sort -n); do echo "  with $k sessions: ${per_n[$k]}"; done
  echo "cross-sectional runs (ses-$cross_ses, --cross $cross): $n_cross"
  [ "$skip_done" = 0 ] || echo "already done, left out: $n_done"
  echo "excluded.tsv entries: $n_excl"
  echo "lists in: $(cd "$out" && pwd -P)"
} | tee "$out/summary.txt"
