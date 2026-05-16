#! /bin/sh
#
# Utility functions for downloading and verifying files, and mounting ISOs.

download_or_verify() { # $1: URL, $2: expected hash, $3: output path
  url="$1"
  expected_hash="$2"
  output_path="$3"

  if [ ! -f "$output_path" ]; then
    wget -O "$output_path" "$url"
  else
    echo "File already exists, skipping download: $output_path"
  fi

  # SKIP_HASH option for faster testing when we don't care about the authenticity of the downloaded files
  if [ "${SKIP_HASH:-0}" -eq 1 ]; then
    echo "SKIP_HASH is set, skipping hash verification for $output_path"
    return
  fi

  actual_hash="$(sha256sum "$output_path" | awk '{ print $1 }')"

  if [ "$actual_hash" != "$expected_hash" ]; then
    echo "SHA256 mismatch for $output_path" >&2
    echo "Expected: $expected_hash" >&2
    echo "Actual:   $actual_hash" >&2
    exit 1
  fi

  echo "SHA256 OK: $actual_hash"
}

mount_iso() { # $1: ISO path, $2: mount point
  iso_path="$1"
  mount_point="$2"

  mkdir -p "$mount_point"

  if mountpoint -q "$mount_point"; then
    echo "Already mounted: $mount_point"
  else
    sudo mount -o loop "$iso_path" "$mount_point"
  fi
}

reproducible_tar() {
  # Using clamp-time to preserve the original timestamps of the files, while
  # resetting the time of any file that was created for the purpose of creating
  # the tar (like install.sh) to a fixed timestamp.
  tar --sort=name \
      --clamp-mtime --mtime="2025-01-01" \
      --owner=0 --group=0 --numeric-owner \
      "$@"
}
