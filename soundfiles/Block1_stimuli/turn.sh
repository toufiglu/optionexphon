#!/bin/bash

# Create a folder to store the MP3 files
output_folder="mp3_files"
mkdir -p "$output_folder"

# Move each MP3 file to the output folder
for file in *.mp3; do
    mv "$file" "$output_folder/$file"
done