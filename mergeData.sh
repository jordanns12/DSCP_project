#!/bin/bash

# Initialize the output file
output_file="madisonTrain.csv"
echo "" > "$output_file"

# Loop through the CSV files from 2018 to 2023
for year in {2018..2023}; do
    # Check if the file exists
    if [ -f "madison${year}.csv" ]; then
        # Append the contents of the current year's CSV file to the output file
        cat "madison${year}.csv" >> "$output_file"
        echo "Appended madison${year}.csv to $output_file"
    else
        echo "File madison${year}.csv does not exist. Skipping..."
    fi
done

echo "Merged CSV files from 2018 to 2023 into $output_file"

