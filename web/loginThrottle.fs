//
// Copyright (c) 2026 Jun-ichiro Sugisaka
//
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
//
namespace Aqualis

open System

/// Configuration for a bounded, file-backed PHP login-attempt throttle.
type FileLoginThrottleOptions = {
    Identifier: string
    DataDirectoryName: string
    ApplicationName: string
    WindowSeconds: int
    MaxIpAttempts: int
    MaxUserAttempts: int
    MaxEntries: int
    StateSoftMaxBytes: int
    StateHardMaxBytes: int
    MaxQuarantineFiles: int
}

[<RequireQualifiedAccess>]
module FileLoginThrottleOptions =
    /// Conservative defaults for a data directory located two levels above the generated PHP file.
    let defaults identifier dataDirectoryName applicationName = {
        Identifier = identifier
        DataDirectoryName = dataDirectoryName
        ApplicationName = applicationName
        WindowSeconds = 600
        MaxIpAttempts = 20
        MaxUserAttempts = 5
        MaxEntries = 4096
        StateSoftMaxBytes = 786432
        StateHardMaxBytes = 1048576
        MaxQuarantineFiles = 3
    }

[<RequireQualifiedAccess>]
module private FileLoginThrottleValidation =
    let validIdentifierCharacter character =
        ('a' <= character && character <= 'z') || ('0' <= character && character <= '9') || character = '_'

    let validPathCharacter character =
        ('A' <= character && character <= 'Z') || validIdentifierCharacter character || character = '-'

    let validate options =
        if String.IsNullOrWhiteSpace options.Identifier
           || not ('a' <= options.Identifier[0] && options.Identifier[0] <= 'z')
           || not (options.Identifier |> Seq.forall validIdentifierCharacter) then
            invalidArg (nameof options) "The throttle identifier must start with a lowercase ASCII letter and contain only lowercase letters, digits, or underscores."
        if String.IsNullOrWhiteSpace options.DataDirectoryName
           || not (options.DataDirectoryName |> Seq.forall validPathCharacter) then
            invalidArg (nameof options) "The throttle data directory name contains an invalid character."
        if String.IsNullOrWhiteSpace options.ApplicationName then
            invalidArg (nameof options) "The throttle application name is required."
        if options.WindowSeconds <= 0 || options.MaxIpAttempts <= 0 || options.MaxUserAttempts <= 0
           || options.MaxEntries <= 0 || options.StateSoftMaxBytes <= 0
           || options.StateHardMaxBytes < options.StateSoftMaxBytes || options.MaxQuarantineFiles < 0 then
            invalidArg (nameof options) "The throttle limits are inconsistent."
        options

/// Generates PHP functions for a bounded, file-backed login-attempt throttle.
type FileLoginThrottle(context:Aqualis, options:FileLoginThrottleOptions) =
    let options = FileLoginThrottleValidation.validate options
    let id = options.Identifier
    let constant = id.ToUpperInvariant()
    let fn name = id + "_" + name
    let c name = constant + "_LOGIN_" + name
    let appMessage suffix = PhpEncoding.stringLiteral (options.ApplicationName + suffix)
    let stateFile suffix = PhpEncoding.stringLiteral ("." + id + "-login-rate-limit" + suffix)

    static member DummyPasswordHash = "$2a$12$KoAU7tfTo34q7Kp7ypc4ROhSCM2Eoif/iJZ5z0sLl3RsbAkrJfIde"

    member _.EmitDefinitions() =
        let atomicWrite = fn "atomic_private_write"
        let recover = fn "recover_login_rate_state"
        let normalize = fn "normalize_login_rate_state"
        let prune = fn "prune_login_rate_state"
        let retryAfter = fn "login_capacity_retry_after"
        let withState = fn "with_login_rate_state"
        let reserveAttempt = fn "reserve_login_attempt"
        let releaseAttempt = fn "release_successful_login_attempt"
        let window = c "WINDOW_SECONDS"
        let maxIp = c "MAX_IP_ATTEMPTS"
        let maxUser = c "MAX_USER_ATTEMPTS"
        let maxEntries = c "MAX_ENTRIES"
        let softBytes = c "STATE_SOFT_MAX_BYTES"
        let hardBytes = c "STATE_HARD_MAX_BYTES"
        let maxQuarantine = c "MAX_QUARANTINE_FILES"
        let source = [
            "const " + window + " = " + string options.WindowSeconds + ";"
            "const " + maxIp + " = " + string options.MaxIpAttempts + ";"
            "const " + maxUser + " = " + string options.MaxUserAttempts + ";"
            "const " + maxEntries + " = " + string options.MaxEntries + ";"
            "const " + softBytes + " = " + string options.StateSoftMaxBytes + ";"
            "const " + hardBytes + " = " + string options.StateHardMaxBytes + ";"
            "const " + maxQuarantine + " = " + string options.MaxQuarantineFiles + ";"
            "function " + atomicWrite + "(string $target, string $contents): void {"
            "$temporaryPath = tempnam(dirname($target), " + PhpEncoding.stringLiteral ("." + id + "-login-rate-") + ");"
            "if ($temporaryPath === false) { throw new \\RuntimeException('Failed to create a temporary rate-limit file.'); }"
            "try { $written = @file_put_contents($temporaryPath, $contents, LOCK_EX); if ($written === false || $written !== strlen($contents)) { throw new \\RuntimeException('Failed to write rate-limit data.'); } if (!@chmod($temporaryPath, 0640)) { throw new \\RuntimeException('Failed to protect rate-limit data.'); } if (!@rename($temporaryPath, $target)) { throw new \\RuntimeException('Failed to publish rate-limit data.'); } $temporaryPath = null; }"
            "finally { if (is_string($temporaryPath) && is_file($temporaryPath)) { @unlink($temporaryPath); } }"
            "}"
            "function " + recover + "(string $statePath, string $reason): void {"
            "$quarantinePath = dirname($statePath).DIRECTORY_SEPARATOR." + stateFile ".invalid-" + ".gmdate('Ymd-His').'-'.bin2hex(random_bytes(4)).'.json';"
            "if (!@rename($statePath, $quarantinePath)) { throw new \\RuntimeException('Failed to quarantine invalid login rate-limit data.'); }"
            atomicWrite + "($statePath, '{\"entries\":[]}');"
            "$quarantineFiles = glob(dirname($statePath).DIRECTORY_SEPARATOR." + stateFile ".invalid-*.json" + ", GLOB_NOSORT);"
            "if (is_array($quarantineFiles)) { rsort($quarantineFiles, SORT_STRING); foreach (array_slice($quarantineFiles, " + maxQuarantine + ") as $expiredQuarantine) { if (is_file($expiredQuarantine) && !@unlink($expiredQuarantine)) { error_log(" + appMessage " could not remove an old login rate-limit quarantine file." + "); } } }"
            "error_log(" + appMessage " reset invalid login rate-limit data: " + ".$reason.'. Quarantine: '.basename($quarantinePath));"
            "}"
            "function " + normalize + "($state): ?array {"
            "if (!is_array($state) || !isset($state['entries']) || !is_array($state['entries'])) { return null; }"
            "$normalizedEntries = [];"
            "foreach ($state['entries'] as $key => $entry) { if (!is_string($key) || preg_match('/\\A(?:ip|user):[0-9a-f]{64}\\z/D', $key) !== 1 || !is_array($entry) || !isset($entry['count'], $entry['window_start']) || !is_int($entry['count']) || $entry['count'] < 0 || !is_int($entry['window_start']) || $entry['window_start'] < 0) { return null; } $entryLimit = strncmp($key, 'ip:', 3) === 0 ? " + maxIp + " : " + maxUser + "; if ($entry['count'] > $entryLimit) { return null; } $normalizedEntries[$key] = ['count' => $entry['count'], 'window_start' => $entry['window_start']]; }"
            "return ['entries' => $normalizedEntries];"
            "}"
            "function " + prune + "(array &$state, int $now): void {"
            "foreach ($state['entries'] as $key => $entry) { if ($entry['count'] === 0 || $entry['window_start'] <= $now - " + window + ") { unset($state['entries'][$key]); continue; } if ($entry['window_start'] > $now) { $state['entries'][$key]['window_start'] = $now; } }"
            "if (count($state['entries']) > " + maxEntries + ") { uasort($state['entries'], static function (array $left, array $right): int { return $left['window_start'] <=> $right['window_start']; }); $state['entries'] = array_slice($state['entries'], -" + maxEntries + ", null, true); }"
            "}"
            "function " + retryAfter + "(array $entries, int $now): int { $oldestExpiry = null; foreach ($entries as $entry) { $expiry = $entry['window_start'] + " + window + "; if ($oldestExpiry === null || $expiry < $oldestExpiry) { $oldestExpiry = $expiry; } } return $oldestExpiry === null ? " + window + " : min(" + window + ", max(1, $oldestExpiry - $now)); }"
            "function " + withState + "(callable $operation): array {"
            "$dataDirectory = dirname(__DIR__, 2).DIRECTORY_SEPARATOR." + PhpEncoding.stringLiteral options.DataDirectoryName + ";"
            "if (!is_dir($dataDirectory) || !is_writable($dataDirectory)) { error_log(" + appMessage " login rate-limit directory is unavailable." + "); return ['ok' => false, 'value' => null]; }"
            "$lockPath = $dataDirectory.DIRECTORY_SEPARATOR." + stateFile ".lock" + "; $lockHandle = @fopen($lockPath, 'c+');"
            "if ($lockHandle === false) { error_log(" + appMessage " login rate-limit lock could not be opened." + "); return ['ok' => false, 'value' => null]; }"
            "if (!@chmod($lockPath, 0640)) { fclose($lockHandle); error_log(" + appMessage " login rate-limit lock could not be protected." + "); return ['ok' => false, 'value' => null]; }"
            "$locked = false;"
            "try {"
            "if (!flock($lockHandle, LOCK_EX)) { throw new \\RuntimeException('Failed to lock login rate-limit data.'); } $locked = true;"
            "$secretPath = $dataDirectory.DIRECTORY_SEPARATOR." + stateFile "-secret" + "; if (!is_file($secretPath)) { " + atomicWrite + "($secretPath, bin2hex(random_bytes(32))); }"
            "$secret = @file_get_contents($secretPath, false, null, 0, 65); if (!is_string($secret) || preg_match('/\\A[0-9a-f]{64}\\z/D', $secret) !== 1) { throw new \\RuntimeException('Invalid login rate-limit secret.'); }"
            "$statePath = $dataDirectory.DIRECTORY_SEPARATOR." + stateFile ".json" + "; $state = ['entries' => []];"
            "if (is_file($statePath)) { $stateSize = @filesize($statePath); if ($stateSize === false) { throw new \\RuntimeException('Failed to inspect login rate-limit state.'); } if ($stateSize > " + hardBytes + ") { " + recover + "($statePath, 'state exceeded the hard size limit ('.(string)$stateSize.' bytes)'); return ['ok' => false, 'value' => null]; } $stateText = @file_get_contents($statePath, false, null, 0, " + hardBytes + " + 1); if (!is_string($stateText)) { throw new \\RuntimeException('Failed to read login rate-limit state.'); } if (strlen($stateText) > " + hardBytes + ") { " + recover + "($statePath, 'state exceeded the hard size limit while being read'); return ['ok' => false, 'value' => null]; } try { $decodedState = json_decode($stateText, true, 16, JSON_THROW_ON_ERROR); } catch (\\JsonException $error) { $decodedState = null; } $state = " + normalize + "($decodedState); if ($state === null) { " + recover + "($statePath, 'state JSON or schema was invalid'); return ['ok' => false, 'value' => null]; } }"
            prune + "($state, time()); $value = $operation($state, $secret); $encoded = json_encode($state, JSON_THROW_ON_ERROR | JSON_UNESCAPED_SLASHES); if (strlen($encoded) > " + softBytes + ") { throw new \\RuntimeException('Login rate-limit state exceeded the soft size limit.'); } " + atomicWrite + "($statePath, $encoded); return ['ok' => true, 'value' => $value];"
            "} catch (\\Throwable $error) { error_log(" + appMessage " login rate-limit failure: " + ".$error->getMessage()); return ['ok' => false, 'value' => null]; }"
            "finally { if ($locked) { flock($lockHandle, LOCK_UN); } fclose($lockHandle); }"
            "}"
            "function " + reserveAttempt + "($userId): array { return " + withState + "(static function (array &$state, string $secret) use ($userId): array { $now = time(); $remoteAddress = $_SERVER['REMOTE_ADDR'] ?? ''; if (!is_string($remoteAddress) || filter_var($remoteAddress, FILTER_VALIDATE_IP) === false) { $remoteAddress = 'unknown'; } $normalizedUserId = is_string($userId) ? $userId : ''; $ipKey = 'ip:'.hash_hmac('sha256', $remoteAddress, $secret); $userKey = 'user:'.hash_hmac('sha256', $normalizedUserId, $secret); $reserve = static function (array &$entries, string $key, int $limit, int $now): int { if (!isset($entries[$key])) { if (count($entries) >= " + maxEntries + ") { return " + retryAfter + "($entries, $now); } $entries[$key] = ['count' => 0, 'window_start' => $now]; } $entry =& $entries[$key]; if ($entry['window_start'] <= $now - " + window + ") { $entry = ['count' => 0, 'window_start' => $now]; } if ($entry['count'] >= $limit) { return min(" + window + ", max(1, $entry['window_start'] + " + window + " - $now)); } $entry['count']++; return 0; }; $ipRetryAfter = $reserve($state['entries'], $ipKey, " + maxIp + ", $now); $userRetryAfter = $ipRetryAfter === 0 ? $reserve($state['entries'], $userKey, " + maxUser + ", $now) : 0; $retryAfter = max($ipRetryAfter, $userRetryAfter); return ['allowed' => $retryAfter === 0, 'retry_after' => $retryAfter]; }); }"
            "function " + releaseAttempt + "($userId): bool { $result = " + withState + "(static function (array &$state, string $secret) use ($userId): bool { $remoteAddress = $_SERVER['REMOTE_ADDR'] ?? ''; if (!is_string($remoteAddress) || filter_var($remoteAddress, FILTER_VALIDATE_IP) === false) { $remoteAddress = 'unknown'; } $normalizedUserId = is_string($userId) ? $userId : ''; $ipKey = 'ip:'.hash_hmac('sha256', $remoteAddress, $secret); $userKey = 'user:'.hash_hmac('sha256', $normalizedUserId, $secret); if (isset($state['entries'][$ipKey])) { if ($state['entries'][$ipKey]['count'] > 1) { $state['entries'][$ipKey]['count']--; } else { unset($state['entries'][$ipKey]); } } unset($state['entries'][$userKey]); return true; }); return $result['ok'] === true; }"
        ]
        context.php.phpcode <| fun () -> source |> List.iter context.writein

    member _.RequireAllowed(userId:PHPdata) =
        Aqualis.merge context userId.Context |> ignore
        context.php.phpcode <| fun () ->
            context.writein("$loginThrottle = " + fn "reserve_login_attempt" + "(" + userId.code + ");")
            context.writein("if ($loginThrottle['ok'] !== true) { http_response_code(503); exit('ログイン機能を利用できません。しばらくしてから再試行してください。'); }")
            context.writein("if ($loginThrottle['value']['allowed'] !== true) { http_response_code(429); header('Retry-After: '.(string)$loginThrottle['value']['retry_after']); exit('ログイン試行回数が上限に達しました。しばらくしてから再試行してください。'); }")

    member _.ReleaseSuccessfulAttempt(userId:PHPdata) =
        Aqualis.merge context userId.Context |> ignore
        context.php.phpcode <| fun () ->
            context.writein("if (!" + fn "release_successful_login_attempt" + "(" + userId.code + ")) { error_log(" + appMessage " failed to release the successful login attempt." + "); }")
