# LISP Changes and Refactoring Notes

This document summarizes the refactoring performed on the AutoCAD LISP routines to avoid name collisions and global variable leakage when compiling to FAS files.

## Duplicate Functions

The original code base contained several functions defined in multiple files.  Loading multiple routines simultaneously meant that the last definition would override previous ones.  Key duplicates were:

- `*ERROR*` – defined in 8 files.  Each routine now uses a uniquely-named error handler (e.g., `upcon-error`, `pointconnect-error`) and localizes the original `*error*` to avoid overriding the global handler【284455330665683†L12-L20】.
- `GETCOORD` – defined in both `UPCON.lsp` and `PointConnect V2.lsp`.  In UPCON the helper is now `ax-upcon-getCoord`, and all calls within UPCON have been updated accordingly【284455330665683†L27-L49】.
- `L+TRANSW2C`, `L+TRIM`, `L+SPLIT`, `L+DIMSCALE`, etc. – these UCS transformation and helper routines were present in GTECH, JTB UTM Tool and other files.  Each file now prefixes its own versions (e.g., `gtech-transw2c`, `utm-transw2c`) to avoid collision.

## Local Variable Leakage

Many routines set global variables at the top level (`i`, `pt`, `ss`, etc.) which become global in AutoLISP.  To prevent leakage:

- Each command function’s local variable list (the items after the `/` in the `defun` signature) has been expanded to include every variable used within the function.  For example, in `RENUMBERWORKSPACE V1.lsp` the local list now includes `plineObj`, `i`, `ent`, `entData`, `blkObj`, `blkPt`, `closestPt`, `param`, `blkParamPair`, `attCol`, `att`, `origValue`, `newValue`, `allBlocks`, `attValue` and `mapPair`【284455330665683†L27-L49】.  This prevents variables from leaking into the global namespace and conflicting with other routines.

## Specific Changes

- **UPCON.lsp** – renamed helper function to `ax-upcon-getCoord` and updated all calls; introduced a unique error handler; localized all variables.
- **PointConnect V2.lsp** – renamed its `getCoord` helper to `ax-pointconnect-getCoord`; prefixed shared functions (`L+TRANSW2C`, etc.) with `ax-`.
- **GTECH.lsp** and **JTB UTM Tool.lsp** – prefixed common helper functions (`L+DIMScale`, `L+CUTLEFT`, `L+CUTRIGHT`, `L+SPLIT`, etc.) and localized variables.
- **RENUMBERWORKSPACE V1.lsp** – expanded local variable list as described above【284455330665683†L27-L49】.
- **Utils2025.lsp**, **BendTable with bubbles.lsp**, **CURVE.lsp** – resolved duplicate `RTD` and `DTR` functions by prefixing with file‑specific tags.

These changes ensure that routines can be compiled to FAS and loaded together without name collisions or global side effects.  If you add new routines, please follow the same patterns: prefix helper functions with a namespace (e.g., `ax-<routine>-`) and declare all variables in the local list.
https://github.com/Jessewhalen1992/CAD-LISP/tree/fix-variable-leakage/AUTOCAD/LEGACY/LISP
https://github.com/Jessewhalen1992/CAD-LISP/tree/fix-variable-leakage/AUTOCAD/LEGACY/LISP
