#!/bin/bash

LOG_DIRS=("data/log" "param/log" "model/log")

echo "Scanning log files for errors and warnings..."
echo "Since this routine searches for any lines in the output file containing the terms 'errors' and 'warnings,' it can show terms NOT related to actual errors, such as 'standard errors.'"

for dir in "${LOG_DIRS[@]}"; do
    full_path="./$dir"

    if [ ! -d "$full_path" ]; then
        echo "Warning: Directory '$full_path' does not exist. Skipping."
        continue
    fi

    echo "Checking log files in: $full_path"

    shopt -s nullglob
    for file in "$full_path"/*.out; do
        echo "  File: $file"

        # First, number all lines using `nl`, so we can preserve line numbers
        # Then, use `grep` to match error/warning lines and include 2 lines after each match
        nl -ba "$file" | grep -i -E -A2 "error|warning|ERROR:|Warning messages:" | sed 's/^/    /'

    done
done

echo "Scan complete."
