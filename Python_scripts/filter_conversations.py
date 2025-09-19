#!/usr/bin/env python3

# filter_conversations.py

"""
Filters conversation CSV files based on a consent list.

This script reads a list of usernames who have given research consent from
a specified CSV file. It then finds all conversation files in a source
directory that match these usernames and copies them to a new directory.

The matching process is flexible to account for inconsistencies:
- It is case-insensitive.
- It ignores special characters and spaces.
- It skips empty lines in the consent file.
"""

import os
import re
import shutil

def normalize_name(name):
    """
    Normalizes a name for matching by making it lowercase and removing
    non-alphanumeric characters.

    Args:
        name (str): The string to normalize.

    Returns:
        str: The normalized string.
    """
    if not isinstance(name, str):
        return ""
    # Convert to lowercase
    lower_name = name.lower()
    # Remove all non-alphanumeric characters
    return re.sub(r'[^a-z0-9]', '', lower_name)

def filter_conversations_by_consent(consent_file, source_dir, dest_dir):
    """
    Copies conversation files from a source directory to a destination directory
    if the filename matches a username in the consent file.

    Args:
        consent_file (str): Path to the CSV file containing consented usernames.
        source_dir (str): Path to the directory with all conversation files.
        dest_dir (str): Path to the directory where consented files will be copied.
    """
    # --- 1. Setup ---
    # Create the destination directory if it doesn't exist
    os.makedirs(dest_dir, exist_ok=True)
    print(f"Output directory '{dest_dir}' created or already exists.")

    # --- 2. Read and Normalize Consent List ---
    consented_names = set()
    try:
        with open(consent_file, 'r', encoding='latin1') as f:
            # Skip header
            next(f)
            for line in f:
                clean_line = line.strip()
                if clean_line:  # Skip empty lines
                    normalized = normalize_name(clean_line)
                    consented_names.add(normalized)
        print(f"Read and normalized {len(consented_names)} unique names from '{consent_file}'.")
    except FileNotFoundError:
        print(f"Error: The consent file '{consent_file}' was not found.")
        return
    except Exception as e:
        print(f"An error occurred while reading the consent file: {e}")
        return

    # --- 3. Filter and Copy Conversations ---
    if not os.path.isdir(source_dir):
        print(f"Error: The source directory '{source_dir}' does not exist.")
        return

    all_files = os.listdir(source_dir)
    copied_count = 0
    matched_files = []

    for filename in all_files:
        if filename.endswith(".csv"):
            # Normalize the filename (without extension) for comparison
            base_name = os.path.splitext(filename)[0]
            normalized_filename = normalize_name(base_name)

            if normalized_filename in consented_names:
                # If a match is found, copy the original file
                source_path = os.path.join(source_dir, filename)
                dest_path = os.path.join(dest_dir, filename)
                shutil.copy2(source_path, dest_path)
                copied_count += 1
                matched_files.append(filename)

    # --- 4. Report Results ---
    print(f"\nFiltering complete.")
    print(f"Copied {copied_count} files to '{dest_dir}'.")
    if copied_count > 0:
        print("\nMatched files:")
        for f in sorted(matched_files):
            print(f" - {f}")

if __name__ == '__main__':
    # Define file paths
    CONSENT_FILE = 'usernames_consent.csv'
    CONVERSATIONS_DIR = 'conversations'
    CONSENTED_CONVERSATIONS_DIR = 'consented_conversations'

    filter_conversations_by_consent(
        CONSENT_FILE,
        CONVERSATIONS_DIR,
        CONSENTED_CONVERSATIONS_DIR
    )
