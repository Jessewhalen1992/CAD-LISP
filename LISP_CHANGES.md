# Change Log

## Recent Updates

- Renamed duplicated helper functions and added local error handlers for several routines:
  - UPCON.lsp: `getCoord` -> `ax-upcon-getCoord`, added `upcon-error` handler.
  - CrossingExtractor2.LSP and CrossingExtractor.LSP: `parse-crossing-attribute` renamed to file-specific variants.
  - BendTable with bubbles.lsp: `dtr`/`rtd` -> `bendtable-dtr`/`bendtable-rtd`.
  - CURVE.lsp: `rtd` -> `curve-rtd`.
  - UTILS2025.lsp: `DTR`/`RTD` -> `utils-dtr`/`utils-rtd`.
  - PointConnect V2.lsp: `getcoord` -> `ax-pointconnect-getCoord`, added `pointconnect-error` handler and prefix UCS transform function.
  - GTECH.LSP and JTB UTM Tool.LSP: renamed UCS transform routines to `gtech-transW2C` and `utm-transW2C`.

