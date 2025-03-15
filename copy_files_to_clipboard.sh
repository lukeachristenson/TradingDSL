#!/bin/bash
# copy_files_to_clipboard.sh: Recursively copy file contents to clipboard,
# with each file preceded by its type (extension) and full path

# Create a temporary file to hold the concatenated output
temp_file=$(mktemp)

# Loop over each matching file
find . -type f \( -name "*.md" -o -name "*.rkt" -o -name "*.txt" -o -name "*.py" \) | while read -r file; do
    # Extract the file extension (without leading dot)
    ext="${file##*.}"
    # Print a header with the file type and name
    echo "[$ext] $file" >> "$temp_file"
    echo "------------------------------------" >> "$temp_file"
    # Append the file content
    cat "$file" >> "$temp_file"
    # Add some newlines for separation
    echo -e "\n\n" >> "$temp_file"
done

# Copy the content to the clipboard
if command -v pbcopy &>/dev/null; then
    cat "$temp_file" | pbcopy
    echo "Content copied to clipboard using pbcopy."
elif command -v xclip &>/dev/null; then
    cat "$temp_file" | xclip -selection clipboard
    echo "Content copied to clipboard using xclip."
else
    echo "Error: Neither pbcopy nor xclip is installed. Please install one to enable clipboard copying."
fi

# Remove the temporary file
rm "$temp_file"

