#!/usr/bin/env python3

"""
Processes a large CSV file containing chatbot conversations and splits it into
separate CSV files for each user.

Each output file will be named after the user's email (with the domain removed)
and will contain only the 'role', 'content', and 'created_at' columns.
The output files are saved in a 'conversations' directory.
"""

import pandas as pd
import os

def process_chatbot_logs(input_csv_path, output_dir):
    """
    Reads a CSV file of chatbot logs, splits them by user, and saves them
    into individual CSV files.

    Args:
        input_csv_path (str): The path to the input CSV file.
        output_dir (str): The path to the directory where output files will be saved.
    """
    # Create the output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    print(f"Output directory '{output_dir}' created or already exists.")

    try:
        # Read the large CSV file
        print(f"Reading CSV file from '{input_csv_path}'...")
        # Explicitly specify dtype for columns that might have mixed types to avoid warnings
        dtype_spec = {'content': 'str'}
        df = pd.read_csv(input_csv_path, dtype=dtype_spec, low_memory=False, encoding='latin1')
        print("CSV file read successfully.")
    except FileNotFoundError:
        print(f"Error: The file '{input_csv_path}' was not found.")
        return
    except Exception as e:
        print(f"An error occurred while reading the CSV file: {e}")
        return

    # --- Data Cleaning: Strip whitespace from headers ---
    df.columns = df.columns.str.strip()

    # --- Debugging: Print column headers ---
    print("Cleaned columns found in CSV:", df.columns.tolist())

    # --- Verification Step ---
    required_columns = ['user_email', 'role', 'content', 'created_at']
    missing_columns = [col for col in required_columns if col not in df.columns]

    if missing_columns:
        print(f"Error: Missing required columns: {missing_columns}")
        print("Please check the CSV file to ensure it contains the correct headers.")
        return

    # Drop rows where user_email is NaN or empty
    df.dropna(subset=['user_email'], inplace=True)

    # Group by user_email
    print("Grouping conversations by user_email...")
    grouped = df.groupby('user_email')
    
    num_users = len(grouped)
    print(f"Found {num_users} unique users.")

    # Process each group
    for i, (user_email, user_df) in enumerate(grouped, 1):
        # Extract username from email
        if isinstance(user_email, str) and '@eduscenar.io' in user_email:
            username = user_email.replace('@eduscenar.io', '')
        elif isinstance(user_email, str):
            username = user_email.split('@')[0]
        else:
            # Skip if user_email is not a string
            print(f"Skipping invalid user_email: {user_email}")
            continue

        # Select the required columns
        output_df = user_df[['role', 'content', 'created_at']].copy()
        
        # Sort conversations by timestamp
        output_df.sort_values(by='created_at', inplace=True)

        # Construct the output file path
        output_csv_path = os.path.join(output_dir, f'{username}.csv')

        # Save to a new CSV file
        try:
            output_df.to_csv(output_csv_path, index=False)
            print(f"({i}/{num_users}) Successfully saved conversation for '{user_email}' to '{output_csv_path}'")
        except Exception as e:
            print(f"Could not save file for '{user_email}'. Error: {e}")

    print("\nProcessing complete.")
    print(f"Individual user conversation files are saved in the '{output_dir}' directory.")


if __name__ == '__main__':
    # Define file paths
    INPUT_CSV = '/Users/kaiyu/Library/CloudStorage/OneDrive-UMCG/Onderwijs/AI stuff/Research/Chatbots application/Pilot BSc Medicine 1/Pilot_2526/Onderzoek/analysis/chat-export-Geneeskunde B1-Chatbot Embryologie-2025-09-17-to-2025-09-18.csv'
    OUTPUT_DIRECTORY = 'conversations'
    
    process_chatbot_logs(INPUT_CSV, OUTPUT_DIRECTORY)

