import os
import pandas as pd
import asyncio
import json
from openai import AsyncOpenAI
from dotenv import load_dotenv
import glob
from tqdm.asyncio import tqdm_asyncio

# --- Configuration ---
# Load environment variables from .env file, overriding any existing system-level variables
load_dotenv(override=True)

# Get OpenAI API key from environment variables
API_KEY = os.getenv("OPENAI_API_KEY")

# Directory containing conversation files
CONVERSATIONS_DIR = "consented_conversations/"
# Output file for the analysis results
OUTPUT_FILE = "conversation_phase_analysis.csv"
# Batch size for parallel processing
BATCH_SIZE = 20
# Model to use for analysis
MODEL_NAME = "gpt-4.1-2025-04-14"

# --- LLM Prompt Structure ---
SYSTEM_PROMPT = """
You are an expert in analyzing educational chatbot conversations. Your task is to determine the highest phase a student reached in a conversation about embryology, based on the definitions provided.

Your output MUST be a valid JSON object with a single key "phase", and a value as an integer (1, 2, 3, or 4). Do not provide any other text or explanation.

Example output:
{
  "phase": 3
}
"""

USER_PROMPT_TEMPLATE = """
### Clinical Case Vignette Summary ###
A newborn boy has parts of his intestines and stomach outside his abdomen. The diagnosis is gastroschisis, a defect in the abdominal wall.

### Phase Definitions ###
- **Phase 1: Factual Knowledge.** The student answers basic "WHAT" questions about terminology (e.g., "What is morphogenesis?", "Name the three germ layers.").
- **Phase 2: General Processes.** The student explains "WHY" and "HOW" for general processes in a healthy context (e.g., "Explain the process of gastrulation.").
- **Phase 3: Application & Self-Explanation.** The student applies knowledge to the clinical case, explaining what might have gone wrong (e.g., "Can you explain what happened in this case?"). This phase ends just before the final assessment question is asked.
- **Phase 4: Assessment.** The student is asked and provides an answer to the final ASSESSMENT QUESTION: "How do you now understand the causes behind the clinical findings in this case?...". Reaching this stage marks Phase 4.

Analyze the conversation transcript below and determine the highest phase reached. If the conversation is very short or doesn't seem to engage with the topic, classify it as Phase 1.

###CONVERSATION TRANSCRIPT###
{conversation_text}
"""

# --- Main Logic ---

async def analyze_conversation(client: AsyncOpenAI, filepath: str) -> dict:
    """
    Analyzes a single conversation file to determine the phase.

    Args:
        client: An instance of AsyncOpenAI.
        filepath: Path to the conversation CSV file.

    Returns:
        A dictionary containing the username and the determined phase.
    """
    username = os.path.basename(filepath).replace('.csv', '')
    try:
        df = pd.read_csv(filepath)
        if df.empty or 'role' not in df.columns or 'content' not in df.columns:
            return {"username": username, "phase": "Error: Invalid format"}

        # Format conversation for the prompt
        conversation_text = ""
        for _, row in df.iterrows():
            conversation_text += f"{row['role']}: {row['content']}\\n"

        user_prompt = USER_PROMPT_TEMPLATE.format(conversation_text=conversation_text)

        response = await client.chat.completions.create(
            model=MODEL_NAME,
            messages=[
                {"role": "system", "content": SYSTEM_PROMPT},
                {"role": "user", "content": user_prompt}
            ],
            response_format={"type": "json_object"},
            temperature=0.0,
        )

        result_json = json.loads(response.choices[0].message.content)
        phase = result_json.get("phase", "Error: Phase not found in response")
        return {"username": username, "phase": phase}

    except FileNotFoundError:
        return {"username": username, "phase": "Error: File not found"}
    except pd.errors.EmptyDataError:
        return {"username": username, "phase": "Error: File is empty"}
    except Exception as e:
        return {"username": username, "phase": f"Error: {str(e)}"}


async def main():
    """
    Main function to run the analysis pipeline.
    """
    if not API_KEY:
        print("Error: OPENAI_API_KEY environment variable not set.")
        print("Please create a .env file and add your key: OPENAI_API_KEY='your_key_here'")
        return

    client = AsyncOpenAI(api_key=API_KEY)

    print("Starting conversation analysis...")
    
    # Find all CSV files in the directory
    files_to_process = glob.glob(os.path.join(CONVERSATIONS_DIR, "*.csv"))
    if not files_to_process:
        print(f"No CSV files found in {CONVERSATIONS_DIR}")
        return

    print(f"Found {len(files_to_process)} conversations to analyze.")

    all_results = []
    
    # Process files in batches
    for i in range(0, len(files_to_process), BATCH_SIZE):
        batch_files = files_to_process[i:i + BATCH_SIZE]
        print(f"\\nProcessing batch {i//BATCH_SIZE + 1} of {len(files_to_process)//BATCH_SIZE + 1}...")
        
        tasks = [analyze_conversation(client, f) for f in batch_files]
        batch_results = await tqdm_asyncio.gather(*tasks, desc="Analyzing conversations")
        all_results.extend(batch_results)

    # Convert results to a DataFrame and save
    results_df = pd.DataFrame(all_results)
    results_df.to_csv(OUTPUT_FILE, index=False)

    print(f"\\nAnalysis complete. Results saved to {OUTPUT_FILE}")
    print("--- Sample of results ---")
    print(results_df.head())
    print("-------------------------")


if __name__ == "__main__":
    asyncio.run(main())
