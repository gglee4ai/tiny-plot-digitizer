# Tiny Plot Digitizer

Tiny Plot Digitizer는 그래프 이미지에서 데이터 포인트를 수동으로 추출하고, 좌표축과 표시 그룹을 함께 관리하는 로컬 Shiny 앱입니다.

앱은 선택한 작업 폴더의 PNG 이미지와 Tiny Plot Digitizer CSV 파일을 직접 읽고 저장합니다. 데이터나 이미지는 외부 서버로 전송하지 않습니다.

## 주요 기능

- 투영된 사각형 그래프 영역과 선형·로그 축 설정
- 여러 그룹의 이름, 색상, 심볼, 크기와 불투명도 관리
- 포인트 연속 추가, 선택, 이동과 제거
- 원본 이미지와 확대 화면을 이용한 0.5픽셀 단위 보정
- CSV 안의 YAML 메타데이터와 변환 좌표 저장
- 저장본 복귀와 최초 파일 상태 초기화

## 요구 사항

- R
- R 패키지: `shiny`, `shinyFiles`, `png`, `yaml`

필요한 패키지는 R에서 다음 명령으로 설치합니다.

```r
install.packages(c("shiny", "shinyFiles", "png", "yaml"))
```

## 실행

### macOS

`Tiny Plot Digitizer.command`를 더블클릭합니다.

터미널에서는 다음 명령으로 실행할 수 있습니다.

```sh
Rscript run.R
```

### Windows

`run.bat`을 더블클릭합니다. R이 기본 설치 폴더에 있거나 `Rscript.exe`가 PATH에 등록되어 있어야 합니다.

### 환경변수

- `DIGITIZER_FOLDER`: 처음 열 작업 폴더. 지정하지 않으면 홈 폴더에서 시작합니다.
- `DIGITIZER_PORT`: 로컬 포트. 기본값은 `8766`입니다.
- `DIGITIZER_BROWSER`: `false`로 지정하면 실행 시 브라우저를 자동으로 열지 않습니다.

앱은 로컬 주소 `http://127.0.0.1:8766`에서 실행됩니다.

## 기본 사용 순서

1. **작업 폴더**에서 PNG와 CSV가 있는 폴더를 선택합니다.
2. 기존 CSV를 선택하거나 **신규**에서 PNG를 선택합니다.
3. **좌표설정** 탭에서 박스와 X·Y축을 지정합니다.
4. **포인트** 탭에서 그룹을 선택하고 포인트를 추가·보정합니다.
5. **파일 저장** 또는 **다른이름 저장**으로 CSV를 기록합니다.

키보드 방향키는 선택된 포인트 또는 설정점을 이동합니다. 포인트 탭에서 `[`와 `]`는 이전·다음 포인트를 선택합니다.

## 파일 구성

```text
tiny-plot-digitizer/
├── app.R
├── run.R
├── Tiny Plot Digitizer.command
├── run.bat
└── README.md
```

R과 필수 패키지가 설치된 컴퓨터라면 이 폴더만 복사하여 독립적으로 실행할 수 있습니다.
