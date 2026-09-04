#!/usr/bin/env bash
set -euo pipefail

EXPECTED_OLD_HEAD=4df1052ff987f89af26c68db496561231199a72d
OLD_MERGE_BASE=c9bdc8d64720b90b5c2c005b8c3c6aac675da266
REVIEW_DIR=/tmp/wasm32-review
mkdir -p "$REVIEW_DIR/commits"
exec > >(tee "$REVIEW_DIR/workflow.log") 2>&1

on_exit() {
  status=$?
  {
    echo "EXIT_STATUS=$status"
    echo "CURRENT_HEAD=$(git rev-parse HEAD 2>/dev/null || true)"
    echo "UPSTREAM_HEAD=$(git rev-parse upstream/master 2>/dev/null || true)"
    echo "FEATURE_REMOTE_HEAD=$(git ls-remote origin refs/heads/feat/wasm32 2>/dev/null | cut -f1 || true)"
  } > "$REVIEW_DIR/workflow-result.env"
}
trap on_exit EXIT

git config user.name "github-actions[bot]"
git config user.email "41898282+github-actions[bot]@users.noreply.github.com"

actual_old_head=$(git rev-parse HEAD)
if [[ "$actual_old_head" != "$EXPECTED_OLD_HEAD" ]]; then
  echo "feat/wasm32 moved: expected $EXPECTED_OLD_HEAD, got $actual_old_head" >&2
  exit 2
fi

git remote add upstream https://github.com/golang/go.git
git fetch --no-tags upstream master
upstream_head=$(git rev-parse upstream/master)
echo "Rebasing $EXPECTED_OLD_HEAD onto upstream master $upstream_head"

set +e
GIT_EDITOR=true git rebase upstream/master
rebase_status=$?
set -e

if (( rebase_status != 0 )); then
  rebase_head=$(git rev-parse REBASE_HEAD)
  if [[ "$rebase_head" != "$EXPECTED_OLD_HEAD" ]]; then
    echo "Unexpected conflicting commit: $rebase_head" >&2
    git status --short
    exit 3
  fi

  mapfile -t conflicts < <(git diff --name-only --diff-filter=U | sort)
  expected=(
    "src/simd/archsimd/_gen/midway/intersect_simd_ops.go"
    "src/simd/archsimd/_gen/wasmgen/main.go"
  )
  if [[ "${conflicts[*]}" != "${expected[*]}" ]]; then
    echo "Unexpected conflict set" >&2
    printf 'actual:   %s\n' "${conflicts[*]}" >&2
    printf 'expected: %s\n' "${expected[*]}" >&2
    exit 4
  fi

  # During rebase, --ours is the new upstream base. Keep upstream's gentools
  # refactor and apply only the wasm32-specific behavior from the old commit.
  git checkout --ours -- "${expected[@]}"

  python3 <<'PY'
from pathlib import Path


def replace_exact(path: str, old: str, new: str, expected_count: int = 1) -> None:
    p = Path(path)
    text = p.read_text()
    count = text.count(old)
    if count != expected_count:
        raise SystemExit(
            f"{path}: expected {expected_count} occurrence(s), found {count}: {old!r}"
        )
    p.write_text(text.replace(old, new))


intersect = "src/simd/archsimd/_gen/midway/intersect_simd_ops.go"
replace_exact(
    intersect,
    'wasmFiles := []string{"ops_wasm.go", "types_wasm.go", "slicepart_wasm.go",\n\t\t"string.go", "slicepart_128.go", "ops_emulated_wasm.go"}',
    'wasmFiles := []string{"ops_wasmx.go", "types_wasmx.go", "slicepart_wasmx.go",\n\t\t"string.go", "slicepart_128.go", "ops_emulated_wasmx.go"}',
)
replace_exact(
    intersect,
    "//go:build goexperiment.simd && (amd64 || wasm || arm64)",
    "//go:build goexperiment.simd && (amd64 || wasm || wasm32 || arm64)",
)
replace_exact(
    intersect,
    "\t\tarch := aaf.arch\n\t\tdoArchWrites := func(w io.Writer) {",
    "\t\tarch := aaf.arch\n\t\tbuildArch := arch\n\t\tfileArch := arch\n\t\tif arch == \"wasm\" {\n\t\t\tbuildArch = \"(wasm || wasm32)\"\n\t\t\tfileArch = \"wasmx\"\n\t\t}\n\t\tdoArchWrites := func(w io.Writer) {",
)
replace_exact(
    intersect,
    'pf("//go:build goexperiment.simd && %s\\n\\n", arch)',
    'pf("//go:build goexperiment.simd && %s\\n\\n", buildArch)',
    2,
)
replace_exact(
    intersect,
    'doArchWrites(files.NewGoFile("simd/internal/bridge/decls_" + arch + ".go"))',
    'doArchWrites(files.NewGoFile("simd/internal/bridge/decls_" + fileArch + ".go"))',
)
replace_exact(
    intersect,
    'doToFromWrites(files.NewGoFile("simd/tofrom_" + arch + ".go"))',
    'doToFromWrites(files.NewGoFile("simd/tofrom_" + fileArch + ".go"))',
)

wasmgen = "src/simd/archsimd/_gen/wasmgen/main.go"
replace_exact(wasmgen, "types_wasm.go", "types_wasmx.go", 2)
replace_exact(wasmgen, "ops_wasm.go", "ops_wasmx.go", 2)
replace_exact(
    wasmgen,
    'fmt.Fprintln(f, "//go:build goexperiment.simd && wasm")',
    'fmt.Fprintln(f, "//go:build goexperiment.simd && (wasm || wasm32)")',
    2,
)
replace_exact(
    wasmgen,
    '\t\tintrinsics.add(sys.ArchWasm, pkg, fn, builder)\n\t}',
    '\t\tintrinsics.add(sys.ArchWasm, pkg, fn, builder)\n\t\tintrinsics.add(sys.ArchWasm32, pkg, fn, builder)\n\t}',
)
PY

  gofmt -w "${expected[@]}"
  git add "${expected[@]}"
  GIT_EDITOR=true git rebase --continue
fi

if git rev-parse -q --verify REBASE_HEAD >/dev/null 2>&1; then
  echo "Rebase state remains unexpectedly" >&2
  exit 5
fi
if [[ -n "$(git diff --name-only --diff-filter=U)" ]]; then
  echo "Unresolved conflicts remain" >&2
  exit 6
fi

commit_count=$(git rev-list --count upstream/master..HEAD)
if [[ "$commit_count" != "11" ]]; then
  echo "Expected 11 rebased commits, got $commit_count" >&2
  exit 7
fi

git diff --check upstream/master...HEAD
git range-diff "$OLD_MERGE_BASE..$EXPECTED_OLD_HEAD" "upstream/master..HEAD" \
  > "$REVIEW_DIR/range-diff-before-generation.txt"

# Regenerate from the rebased generators. This both validates the generators and
# catches stale generated files after upstream's gentools migration.
(
  cd src/simd/archsimd
  env GO111MODULE=off GOPATH="$GITHUB_WORKSPACE" go generate -tags=goexperiment.simd
)
git diff --binary > "$REVIEW_DIR/generator-diff-before-amend.patch"
git diff --check
if [[ -n "$(git status --porcelain)" ]]; then
  echo "Generated outputs changed; amending the final SIMD commit."
  git add -A
  GIT_EDITOR=true git commit --amend --no-edit
fi

# Build the new toolchain, then exercise both the compiler plumbing and wasm32.
./src/make.bash
./bin/go version
./bin/go tool dist list | tee "$REVIEW_DIR/dist-list.txt"
grep -qx 'js/wasm32' "$REVIEW_DIR/dist-list.txt"
grep -qx 'wasip1/wasm32' "$REVIEW_DIR/dist-list.txt"

./bin/go test cmd/internal/sys internal/platform
GOEXPERIMENT=simd ./bin/go test cmd/compile/internal/ssa cmd/compile/internal/ssagen

env GOEXPERIMENT=simd GOOS=js GOARCH=wasm32 ./bin/go build std
env GOEXPERIMENT=simd GOOS=wasip1 GOARCH=wasm32 ./bin/go build std
env GOEXPERIMENT=simd GOOS=wasip1 GOARCH=wasm32 \
  ./bin/go test -c simd/archsimd -o /tmp/archsimd-wasip1-wasm32.test

test -x lib/wasm/go_js_wasm32_exec
env GOEXPERIMENT=simd GOOS=js GOARCH=wasm32 \
  ./bin/go test -timeout=20m -exec="$PWD/lib/wasm/go_js_wasm32_exec" simd/archsimd

# Generation, build, and tests must not dirty tracked source files.
git diff --check upstream/master...HEAD
if [[ -n "$(git status --porcelain)" ]]; then
  git status --short
  git diff --stat
  git diff
  echo "Build or tests changed tracked files" >&2
  exit 8
fi

# Export every commit independently for the subsequent detailed review.
git log --reverse --format='%H%x09%P%x09%aI%x09%s' upstream/master..HEAD \
  | tee "$REVIEW_DIR/commits.tsv"
git diff --stat upstream/master...HEAD > "$REVIEW_DIR/diff-stat.txt"
git diff --name-status -M upstream/master...HEAD > "$REVIEW_DIR/changed-files.txt"
git diff --binary upstream/master...HEAD > "$REVIEW_DIR/series.patch"
git range-diff "$OLD_MERGE_BASE..$EXPECTED_OLD_HEAD" "upstream/master..HEAD" \
  > "$REVIEW_DIR/range-diff.txt"

i=0
while IFS=$'\t' read -r sha parents authored subject; do
  i=$((i + 1))
  safe_subject=$(printf '%s' "$subject" | tr -cs 'A-Za-z0-9._-' '-' | sed 's/^-//;s/-$//')
  printf -v n '%02d' "$i"
  git show --format=fuller --find-renames --find-copies --stat --patch "$sha" \
    > "$REVIEW_DIR/commits/${n}-${sha:0:12}-${safe_subject}.patch"
done < "$REVIEW_DIR/commits.tsv"

new_head=$(git rev-parse HEAD)
cat > "$REVIEW_DIR/result.env" <<EOF
UPSTREAM_HEAD=$upstream_head
OLD_FEATURE_HEAD=$EXPECTED_OLD_HEAD
NEW_FEATURE_HEAD=$new_head
COMMIT_COUNT=$(git rev-list --count upstream/master..HEAD)
EOF

# The lease prevents overwriting any commit created while this validation ran.
git push --force-with-lease="refs/heads/feat/wasm32:$EXPECTED_OLD_HEAD" \
  origin HEAD:refs/heads/feat/wasm32
echo "Force-pushed $new_head onto feat/wasm32"
