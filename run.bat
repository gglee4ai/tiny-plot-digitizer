@echo off
chcp 65001 >nul
setlocal
cd /d "%~dp0"

set "RSCRIPT=Rscript.exe"
where Rscript.exe >nul 2>nul
if errorlevel 1 (
  set "RSCRIPT="
  for /d %%D in ("%ProgramFiles%\R\R-*") do (
    if exist "%%~fD\bin\Rscript.exe" set "RSCRIPT=%%~fD\bin\Rscript.exe"
  )
)

if not defined RSCRIPT (
  echo Rscript를 찾을 수 없습니다. 먼저 R을 설치하세요.
  pause
  exit /b 1
)

"%RSCRIPT%" run.R
if errorlevel 1 pause
