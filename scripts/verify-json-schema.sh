#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  printf '%s\n' 'Usage: verify-json-schema.sh <generated-code-directory>' >&2
  exit 2
fi

output_root="$(cd "$1" && pwd)"
read_directory="$output_root/php-json-schema-read"
update_directory="$output_root/php-json-schema-update"
expression_directory="$output_root/php-json-schema-read-expression"
path_directory="$output_root/php-json-schema-read-path"
transition_directory="$output_root/php-json-schema-update-object-to-list"
empty_transition_directory="$output_root/php-json-schema-update-object-to-empty-list"
numeric_transition_directory="$output_root/php-json-schema-update-numeric-object-to-list"
numeric_noop_directory="$output_root/php-json-schema-update-numeric-object-unchanged"
side_effect_directory="$output_root/php-json-schema-update-side-effect-key"
nested_numeric_directory="$output_root/php-json-schema-update-nested-numeric-key"
rule_order_directory="$output_root/php-json-schema-read-rule-order"
php -l "$read_directory/smoke.php" >/dev/null
php -l "$update_directory/smoke.php" >/dev/null
php -l "$expression_directory/smoke.php" >/dev/null
php -l "$path_directory/smoke.php" >/dev/null
php -l "$transition_directory/smoke.php" >/dev/null
php -l "$empty_transition_directory/smoke.php" >/dev/null
php -l "$numeric_transition_directory/smoke.php" >/dev/null
php -l "$numeric_noop_directory/smoke.php" >/dev/null
php -l "$side_effect_directory/smoke.php" >/dev/null
php -l "$nested_numeric_directory/smoke.php" >/dev/null
php -l "$rule_order_directory/smoke.php" >/dev/null

check_case() {
  local label="$1" script="$2" expected="$3" file="$4" contents="$5"
  printf '%s' "$contents" > "$file"
  local actual stderr_file="${file}.stderr"
  actual="$(php "$script" "$file" 2>"$stderr_file")"
  if grep -Eq 'PHP (Warning|Fatal error|Notice|Deprecated)|Fatal error:|Warning:' "$stderr_file"; then
    printf '%s: PHP emitted a warning or fatal error.\n' "$label" >&2
    cat "$stderr_file" >&2
    exit 1
  fi
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
check_case 'JSON sameAs rejects a mismatched compound expression' "$expression_directory/smoke.php" schema-error "$expression_directory/false.json" false
check_case 'JSON sameAs accepts a matching compound expression' "$expression_directory/smoke.php" ok "$expression_directory/true.json" true
check_case 'JSON read evaluates a dynamic path once' "$path_directory/smoke.php" ok:1 "$path_directory/data.json" false
check_case 'JSON relation rules reject missing fields' "$rule_order_directory/smoke.php" schema-error "$rule_order_directory/missing.json" '{}'
check_case 'JSON relation rules reject wrong field types' "$rule_order_directory/smoke.php" schema-error "$rule_order_directory/wrong-types.json" '{"A":null,"B":[],"Entries":null}'
check_case 'JSON relation rules reject nested arrays' "$rule_order_directory/smoke.php" schema-error "$rule_order_directory/nested-array.json" '{"A":[],"B":[],"Entries":[{"ID":[]}]}'
check_case 'JSON relation rules accept valid fields' "$rule_order_directory/smoke.php" ok "$rule_order_directory/valid.json" '{"A":[1],"B":[2],"Entries":[{"ID":"a"}]}'

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

check_case 'JSON update replaces an object with a list' "$transition_directory/smoke.php" ok "$transition_directory/data.json" '{"Payload":{}}'
python3 - "$transition_directory/data.json" <<'PY'
import json
import sys

with open(sys.argv[1], encoding="utf-8") as stream:
    data = json.load(stream)
assert data == {"Payload": [1, 2]}, data
PY

check_case 'JSON update replaces an object with an empty list' "$empty_transition_directory/smoke.php" ok "$empty_transition_directory/data.json" '{"Payload":{"Name":7}}'
check_case 'JSON update replaces a numeric-key object with a list' "$numeric_transition_directory/smoke.php" ok "$numeric_transition_directory/data.json" '{"Payload":{"0":7}}'
check_case 'JSON update preserves an unchanged numeric-key object' "$numeric_noop_directory/smoke.php" ok "$numeric_noop_directory/data.json" '{"Payload":{"0":7}}'
check_case 'JSON update evaluates a dynamic key once' "$side_effect_directory/smoke.php" 1 "$side_effect_directory/data.json" '{"Payload":{"Name":7}}'
check_case 'JSON update tracks a numeric-string nested key' "$nested_numeric_directory/smoke.php" ok "$nested_numeric_directory/data.json" '{"Payload":{"0":{"Name":7}}}'
python3 - "$empty_transition_directory/data.json" "$numeric_transition_directory/data.json" "$numeric_noop_directory/data.json" "$side_effect_directory/data.json" "$nested_numeric_directory/data.json" <<'PY'
import json
import sys

with open(sys.argv[1], encoding="utf-8") as stream:
    empty = json.load(stream)
with open(sys.argv[2], encoding="utf-8") as stream:
    numeric = json.load(stream)
with open(sys.argv[3], encoding="utf-8") as stream:
    unchanged = json.load(stream)
with open(sys.argv[4], encoding="utf-8") as stream:
    dynamic = json.load(stream)
with open(sys.argv[5], encoding="utf-8") as stream:
    nested = json.load(stream)
assert empty == {"Payload": []}, empty
assert numeric == {"Payload": [1, 2]}, numeric
assert unchanged == {"Payload": {"0": 7}}, unchanged
assert dynamic == {"Payload": []}, dynamic
assert nested == {"Payload": {"0": []}}, nested
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
