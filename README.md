# Digitizing Point Editor

Local Shiny app for adjusting digitized marker centers stored in `data-raw` CSV files.

```sh
Rscript tools/digitization-point-editor/run.R
```

The default address is `http://127.0.0.1:8766`. Set `DIGITIZER_PORT` to use another port.

Select a folder under `data-raw` first. The app then discovers compatible CSV files only within that folder. A compatible CSV contains a `Source figure` comment and a supported pixel-axis calibration formula. Clicking the source image selects the nearest point. Arrow buttons or keyboard arrow keys move it by one pixel. `[` and `]` select the previous and next points; `Shift+Left` and `Shift+Right` provide the same navigation. `CSV 저장` updates only changed rows while preserving the comment header and untouched rows.
