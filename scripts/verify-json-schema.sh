#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  printf '%s\n' 'Usage: verify-json-schema.sh <generated-code-directory>' >&2
  exit 2
fi

output_root="$(cd "$1" && pwd)"
read_directory="$output_root/php-json-schema-read"
update_directory="$output_root/php-json-schema-update"
php -l "$read_directory/smoke.php" >/dev/null
php -l "$update_directory/smoke.php" >/dev/null

check_case() {
  local label="$1" script="$2" expected="$3" file="$4" contents="$5"
  printf '%s' "$contents" > "$file"
  local actual
  actual="$(php "$script" "$file")"
  if [[ "$actual" != "$expected" ]]; then
    printf '%s: expected %s, received %s\n' "$label" "$expected" "$actual" >&2
    exit 1
  fi
  printf '%s: passed\n' "$label"
}

valid='{"Identity":7,"Items":[{"Value":"a"}],"Nested":[[1,2],[3]],"EmptyObject":{},"Map":{"k":"v"}}'
wrong_identity='{"Identity":"wrong","Items":[{"Value":"a"}],"Nested":[[1,2],[3]],"EmptyObject":{},"Map":{"k":"v"}}'
object_as_list='{"Identity":7,"Items":{"0":{"Value":"a"}},"Nested":[[1,2],[3]],"EmptyObject":{},"Map":{"k":"v"}}'
nested_object_as_list='{"Identity":7,"Items":[{"Value":"a"}],"Nested":[[1,2],{"0":3}],"EmptyObject":{},"Map":{"k":"v"}}'
list_as_object='{"Identity":7,"Items":[{"Value":"a"}],"Nested":[[1,2],[3]],"EmptyObject":[],"Map":{"k":"v"}}'
list_as_map='{"Identity":7,"Items":[{"Value":"a"}],"Nested":[[1,2],[3]],"EmptyObject":{},"Map":["v"]}'

check_case 'JSON schema accepts valid data' "$read_directory/smoke.php" ok "$read_directory/valid.json" "$valid"
check_case 'JSON schema rejects a mismatched string' "$read_directory/smoke.php" schema-error "$read_directory/wrong-identity.json" "$wrong_identity"
check_case 'JSON schema rejects an object as a list' "$read_directory/smoke.php" schema-error "$read_directory/object-as-list.json" "$object_as_list"
check_case 'JSON schema rejects a nested object as a list' "$read_directory/smoke.php" schema-error "$read_directory/nested-object-as-list.json" "$nested_object_as_list"
check_case 'JSON schema rejects a list as an object' "$read_directory/smoke.php" schema-error "$read_directory/list-as-object.json" "$list_as_object"
check_case 'JSON schema rejects a list as a map' "$read_directory/smoke.php" schema-error "$read_directory/list-as-map.json" "$list_as_map"

check_case 'JSON schema updates valid data' "$update_directory/smoke.php" ok "$update_directory/valid.json" "$valid"
python3 - "$update_directory/valid.json" <<'PY'
import json
import sys

with open(sys.argv[1], encoding="utf-8") as stream:
    data = json.load(stream)
assert data == {
    "Identity": 7,
    "Items": [{"Value": "b"}],
    "Nested": [[1, 2], [3]],
    "EmptyObject": {},
    "Map": {"k": "v"},
}, data
PY

for case in wrong-identity object-as-list nested-object-as-list list-as-object list-as-map; do
  case "$case" in
    wrong-identity) contents="$wrong_identity" ;;
    object-as-list) contents="$object_as_list" ;;
    nested-object-as-list) contents="$nested_object_as_list" ;;
    list-as-object) contents="$list_as_object" ;;
    list-as-map) contents="$list_as_map" ;;
  esac
  file="$update_directory/$case.json"
  check_case "JSON update rejects $case" "$update_directory/smoke.php" update-error "$file" "$contents"
  if [[ "$(cat "$file")" != "$contents" ]]; then
    printf 'JSON update changed rejected %s input.\n' "$case" >&2
    exit 1
  fi
done

printf '%s\n' 'All generated JSON schema checks passed.'
