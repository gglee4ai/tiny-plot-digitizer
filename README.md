# Digitizing Point Editor

`data-raw`의 CSV에 저장된 디지타이징 포인트를 원본 그림 위에서 확인하고, 마커 중심을 1 pixel 단위로 보정하는 로컬 Shiny 앱입니다.

## 실행

저장소 루트에서 다음 명령을 실행합니다.

```sh
Rscript tools/digitization-point-editor/run.R
```

기본 주소는 `http://127.0.0.1:8766`입니다. 다른 포트를 사용하려면 `DIGITIZER_PORT` 환경변수를 지정합니다.

## PNG와 CSV의 매칭 기준

앱은 파일명의 유사성으로 PNG와 CSV를 추측하지 않습니다. 먼저 `Folder (data-raw)`에서 작업 폴더를 선택하면, 해당 폴더 아래의 CSV를 재귀적으로 검색합니다. 각 CSV의 상단 주석에서 `# Source figure:`를 읽고 그 값으로 원본 PNG를 찾습니다.

```text
# Source figure: 02_source_figure-1_yield_strength_vs_fluence.png
# Axis calibration on the source PNG: x = 58.5 + 209.0 * fluence; y = 658.642857 - 4.219643 * YS_ksi
```

`# Source figure:`의 경로는 CSV가 있는 폴더를 기준으로 해석합니다. 따라서 일반적으로 PNG와 CSV를 같은 폴더에 두고 PNG 파일명만 기록하면 됩니다. 서로 다른 폴더에 둘 경우에는 CSV 위치를 기준으로 한 상대경로를 기록할 수 있습니다.

Dataset 목록에 표시되려면 다음 조건을 모두 만족해야 합니다.

- CSV에 `# Source figure:` 주석이 있어야 합니다.
- 지정된 PNG 파일이 실제로 존재해야 합니다.
- CSV에 앱이 해석할 수 있는 `# Axis calibration ...:` 주석이 있어야 합니다.
- CSV 본문에 한 행 이상의 데이터가 있어야 합니다.

CSV 본문의 `source_figure` 컬럼은 데이터의 출처 표시에 사용될 뿐 PNG 매칭에는 사용되지 않습니다. 하나의 PNG에서 여러 물성값을 디지타이징한 경우, 여러 CSV가 같은 PNG 파일을 가리킬 수도 있습니다.

## 사용 방법

1. `Folder (data-raw)`에서 작업 폴더를 선택합니다.
2. `Dataset`에서 수정할 CSV를 선택합니다.
3. `Point` 목록이나 원본 그림의 마커를 선택합니다.
4. 방향 버튼 또는 키보드 방향키로 선택한 포인트를 1 pixel씩 이동합니다.
5. `CSV 저장`을 눌러 변경한 행을 원본 CSV에 반영합니다.

`[`와 `]`는 이전·다음 포인트를 선택합니다. `Shift+Left`와 `Shift+Right`도 같은 기능을 수행합니다. 가운데 되돌리기 버튼은 현재 포인트를 저장 전 위치로 되돌립니다. CSV 저장 시 주석과 수정하지 않은 행은 그대로 유지됩니다.
