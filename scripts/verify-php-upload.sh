#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  printf '%s\n' 'Usage: verify-php-upload.sh <generated-code-directory>' >&2
  exit 2
fi

output_root="$(cd "$1" && pwd)"
test_root="$(mktemp -d /tmp/aqualis-upload-smoke.XXXXXX)"
server_pid=""

cleanup() {
  if [[ -n "$server_pid" ]]; then
    kill "$server_pid" 2>/dev/null || true
    wait "$server_pid" 2>/dev/null || true
  fi
  if [[ "$test_root" == /tmp/aqualis-upload-smoke.* && -d "$test_root" ]]; then
    rm -rf -- "$test_root"
  fi
}
trap cleanup EXIT

web_root="$test_root/public"
private_root="$test_root/private-uploads"
mkdir -m 700 "$web_root" "$private_root"
cp "$output_root/php-upload-private/upload.php" "$web_root/private.php"
cp "$output_root/php-upload-public/upload.php" "$web_root/public.php"
cp "$output_root/php-upload-alias/upload.php" "$web_root/alias.php"
printf '%s\n' 'Aqualis upload runtime smoke' > "$test_root/payload.txt"
printf '%*s' 1100 '' | tr ' ' 'a' > "$test_root/oversized.txt"
printf '%s\n' '<?php echo "unexpected execution"; ?>' > "$test_root/script.php"

for script in private public alias; do
  php -l "$web_root/$script.php" >/dev/null
done

port="$(python3 -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1]); s.close()')"
php -S "127.0.0.1:$port" -t "$web_root" > "$test_root/server.log" 2>&1 &
server_pid=$!
base_url="http://127.0.0.1:$port"

ready=0
for _ in {1..50}; do
  if curl --silent --output /dev/null "$base_url/private.php"; then
    ready=1
    break
  fi
  sleep 0.1
done
if [[ "$ready" -ne 1 ]]; then
  printf '%s\n' 'PHP development server did not start.' >&2
  cat "$test_root/server.log" >&2
  exit 1
fi

upload() {
  local script_name="$1"
  local file_path="${2:-$test_root/payload.txt}"
  curl --fail --silent --show-error \
    --form "file=@$file_path;type=text/plain" \
    "$base_url/$script_name.php"
}

expect_failure() {
  local response="$1"
  local expected_message="$2"
  printf '%s' "$response" | php -r '
    $results = json_decode(stream_get_contents(STDIN), true);
    $expected = $argv[1];
    if (!is_array($results) || count($results) !== 1 ||
        ($results[0]["success"] ?? null) !== false ||
        !str_contains((string)($results[0]["error"] ?? ""), $expected)) {
      fwrite(STDERR, "Unexpected upload result: ".json_encode($results).PHP_EOL);
      exit(1);
    }
  ' "$expected_message"
}

stored_name="$(upload private | php -r '
  $results = json_decode(stream_get_contents(STDIN), true);
  if (!is_array($results) || count($results) !== 1 ||
      ($results[0]["success"] ?? null) !== true) {
    fwrite(STDERR, "Private upload failed: ".json_encode($results).PHP_EOL);
    exit(1);
  }
  echo $results[0]["stored_name"];
')"

if [[ ! "$stored_name" =~ ^[0-9a-f]+\.txt$ ]] ||
   [[ ! -f "$private_root/$stored_name" ]] ||
   ! cmp --silent "$test_root/payload.txt" "$private_root/$stored_name" ||
   [[ "$(stat -c '%a' "$private_root/$stored_name")" != '600' ]]; then
  printf '%s\n' 'Private upload name, contents, or mode did not match expectations.' >&2
  exit 1
fi

direct_status="$(curl --silent --output /dev/null --write-out '%{http_code}' "$base_url/$stored_name")"
if [[ "$direct_status" != '404' ]]; then
  printf 'Private upload was directly accessible (HTTP %s).\n' "$direct_status" >&2
  exit 1
fi

expect_failure "$(upload public)" 'publicly accessible'
expect_failure "$(upload private "$test_root/oversized.txt")" 'file size is not allowed'
expect_failure "$(upload private "$test_root/script.php")" 'file type is not allowed'
ln -s "$private_root" "$web_root/public-link"
expect_failure "$(upload alias)" 'publicly accessible'

chmod 755 "$private_root"
expect_failure "$(upload private)" 'group or other users'

if [[ "$(find "$private_root" -maxdepth 1 -type f | wc -l)" -ne 1 ]]; then
  printf '%s\n' 'A rejected upload left an unexpected file in private storage.' >&2
  exit 1
fi

printf '%s\n' 'PHP HTTP uploads: private storage succeeded; unsafe content and storage were rejected.'
