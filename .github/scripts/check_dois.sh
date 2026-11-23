#!/usr/bin/env bash
# Resolve every DOI in docs/Bibliography.md against Crossref and print the
# registered first author, year and title next to the line that cites it.
# ES: Resuelve cada DOI de docs/Bibliography.md contra Crossref.
set -u
fail=0
grep -oE 'https://doi\.org/[^ )>]+' docs/Bibliography.md | sed 's#https://doi.org/##; s/[.,;]$//' | sort -u |
while read -r doi; do
    js=$(curl -sS -m 20 -H 'User-Agent: AssumptionsLab-CI (doi check)' "https://api.crossref.org/works/$doi" 2>/dev/null)
    if echo "$js" | jq -e .message >/dev/null 2>&1; then
        echo "$js" | jq -r --arg d "$doi" '.message | "OK\t\($d)\t\(.author[0].family // "-")\t\((.issued["date-parts"][0][0]) // "-")\t\(.title[0] // "-")"'
    else
        echo -e "MISSING\t$doi"
    fi
done
