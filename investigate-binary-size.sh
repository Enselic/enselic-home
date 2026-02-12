#!/usr/bin/env bash
set -euo pipefail

# From https://chatgpt.com/c/698d60ce-e40c-8385-b4a3-16de6726a015

if [[ $# -lt 1 || $# -gt 2 ]]; then
  echo "Usage: $0 <elf-file> [addr2line-extra-args]"
  echo "Example: $0 ./a.out \"-f -C -p\""
  exit 2
fi

ELF="$1"
A2L_EXTRA="${2:-}"

# Extract .text section info: VMA (Address), file Offset, and Size
read -r TEXT_VMA_HEX TEXT_OFF_HEX TEXT_SIZE_HEX < <(
  readelf -S --wide "$ELF" | awk '
    $0 ~ /\] *\.text[ \t]/ {
      # Columns: [Nr] Name Type Address Off Size ...
      gsub(/^0x/,"",$4); gsub(/^0x/,"",$5); gsub(/^0x/,"",$6);
      print $4, $5, $6;
      exit
    }'
)

if [[ -z "${TEXT_VMA_HEX:-}" || -z "${TEXT_OFF_HEX:-}" || -z "${TEXT_SIZE_HEX:-}" ]]; then
  echo "Error: failed to locate .text section in: $ELF" >&2
  exit 1
fi

TEXT_VMA=$((16#$TEXT_VMA_HEX))
TEXT_OFF=$((16#$TEXT_OFF_HEX))
TEXT_SIZE=$((16#$TEXT_SIZE_HEX))

START_OFF=$TEXT_OFF
END_OFF=$((TEXT_OFF + TEXT_SIZE)) # exclusive

echo "ELF:      $ELF"
printf ".text VMA: 0x%x\n" "$TEXT_VMA"
printf ".text off: 0x%x\n" "$TEXT_OFF"
printf ".text sz:  0x%x (%d bytes)\n" "$TEXT_SIZE" "$TEXT_SIZE"
echo

# Generate addresses (one per byte) and feed to addr2line in one shot.
# GNU addr2line supports reading addresses from stdin when no addresses are given.
# If yours doesn't, uncomment the chunking fallback below.
generate_addrs() {
  local off addr
  for ((off=START_OFF; off<END_OFF; off++)); do
    addr=$((TEXT_VMA + (off - TEXT_OFF)))
    printf "0x%x\n" "$addr"
  done
}

# Print mapping lines as: off addr  <addr2line output>
# We'll paste offsets+addrs with addr2line results.
offset_and_addr_lines() {
  local off addr
  for ((off=START_OFF; off<END_OFF; off++)); do
    addr=$((TEXT_VMA + (off - TEXT_OFF)))
    printf "off=0x%08x addr=0x%016x\n" "$off" "$addr"
  done
}

# Run addr2line once, consuming addresses from stdin
# shellcheck disable=SC2086
# paste -d' ' <(offset_and_addr_lines) <(generate_addrs | addr2line $A2L_EXTRA -e "$ELF") | sed 's/[[:space:]]\+$//'
generate_addrs | addr2line $A2L_EXTRA -e "$ELF"


###############################################################################
# If your addr2line DOES NOT read stdin, use chunking like this instead:
#
# CHUNK=5000   # addresses per call; tune as needed
# tmp_addrs="$(mktemp)"
# generate_addrs > "$tmp_addrs"
# tmp_out="$(mktemp)"
# : > "$tmp_out"
# while read -r -a batch && ((${#batch[@]})); do
#   addr2line $A2L_EXTRA -e "$ELF" "${batch[@]}" >> "$tmp_out"
# done < <(xargs -n "$CHUNK" < "$tmp_addrs")
# paste -d' ' <(offset_and_addr_lines) "$tmp_out" | sed 's/[[:space:]]\+$//'
# rm -f "$tmp_addrs" "$tmp_out"
###############################################################################
