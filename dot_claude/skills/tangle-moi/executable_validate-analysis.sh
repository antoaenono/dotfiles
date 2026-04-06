#!/bin/sh
# Usage: validate-analysis.sh <path-to-analysis.json>
# Validates the structured analysis JSON and prints the proceed value.
# Exits 0 on success, 1 on validation failure.

file="${1:-}"
if [ -z "$file" ] || [ ! -f "$file" ]; then
  echo "ERROR: file not found: $file" >&2
  exit 1
fi

required_fields="files lines summary warnings confidence reason proceed"
for field in $required_fields; do
  if ! jq -e ".$field" "$file" > /dev/null 2>&1; then
    echo "ERROR: missing required field: $field" >&2
    exit 1
  fi
done

confidence=$(jq -r '.confidence' "$file")
case "$confidence" in
  HIGH|MEDIUM|LOW) ;;
  *) echo "ERROR: confidence must be HIGH, MEDIUM, or LOW (got: $confidence)" >&2; exit 1 ;;
esac

proceed=$(jq -r '.proceed' "$file")
case "$proceed" in
  yes|ask) ;;
  *) echo "ERROR: proceed must be yes or ask (got: $proceed)" >&2; exit 1 ;;
esac

echo "proceed=$proceed"
