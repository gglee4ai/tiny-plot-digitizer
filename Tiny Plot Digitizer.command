#!/bin/zsh

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
RSCRIPT="$(command -v Rscript || true)"

if [[ -z "$RSCRIPT" && -x "/Library/Frameworks/R.framework/Resources/bin/Rscript" ]]; then
  RSCRIPT="/Library/Frameworks/R.framework/Resources/bin/Rscript"
fi

if [[ -z "$RSCRIPT" ]]; then
  echo "Rscript를 찾을 수 없습니다. 먼저 R을 설치하세요."
  read -r "?Enter 키를 눌러 종료합니다."
  exit 1
fi

exec "$RSCRIPT" "$SCRIPT_DIR/run.R"
