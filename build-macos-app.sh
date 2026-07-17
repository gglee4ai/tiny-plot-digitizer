#!/bin/bash

set -euo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_BUNDLE="$PROJECT_DIR/Tiny Plot Digitizer.app"
BUNDLE_APP_DIR="$APP_BUNDLE/Contents/Resources/app"

if [[ "$(uname -s)" != "Darwin" ]]; then
  echo "이 스크립트는 macOS에서만 실행할 수 있습니다." >&2
  exit 1
fi

if [[ ! -d "$BUNDLE_APP_DIR" ]]; then
  echo "앱 번들의 Resources/app 폴더를 찾을 수 없습니다." >&2
  exit 1
fi

if ! command -v codesign >/dev/null 2>&1; then
  echo "codesign 명령을 찾을 수 없습니다." >&2
  exit 1
fi

cp "$PROJECT_DIR/app.R" "$BUNDLE_APP_DIR/app.R"
cp "$PROJECT_DIR/run.R" "$BUNDLE_APP_DIR/run.R"

codesign --force --deep --sign - "$APP_BUNDLE"
codesign --verify --deep --strict --verbose=2 "$APP_BUNDLE"

cmp -s "$PROJECT_DIR/app.R" "$BUNDLE_APP_DIR/app.R"
cmp -s "$PROJECT_DIR/run.R" "$BUNDLE_APP_DIR/run.R"

echo "macOS 앱 번들 동기화와 서명 검증이 완료되었습니다."
