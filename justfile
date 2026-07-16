run example="Hello" *args:
  #!/usr/bin/env bash
  set -euo pipefail

  if [[ "${LEAN_PLAY_DEV_SHELL:-}" != 1 ]]; then
    exec nix develop "path:." --command env LEAN_PLAY_DEV_SHELL=1 just run "{{example}}" {{args}}
  fi

  file="examples/{{example}}.lean"
  directory="examples/{{example}}"

  if [[ -f "$file" ]]; then
    exec lean --run "$file" {{args}}
  elif [[ -f "$directory/Main.lean" && -f "$directory/modules.txt" ]]; then
    cache="$(mktemp -d)"
    trap 'rm -rf "$cache"' EXIT
    mkdir -p "$cache/$directory"

    while IFS= read -r module; do
      LEAN_PATH="$cache${LEAN_PATH:+:$LEAN_PATH}" \
        lean -o "$cache/$directory/$module.olean" "$directory/$module.lean"
    done < "$directory/modules.txt"

    LEAN_PATH="$cache${LEAN_PATH:+:$LEAN_PATH}" lean --run "$directory/Main.lean" {{args}}
  else
    echo "unknown example: {{example}}" >&2
    exit 2
  fi
