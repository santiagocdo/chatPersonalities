"""
infer_personalities.py

Uses the Anthropic Python SDK to predict Big Five personality scores from
chat interaction text, applying two different prompting strategies.

Prompts are based on the project's prompts.docx examples:
  Prompt 1 — Lexical Hypothesis: direct scoring from linguistic markers
  Prompt 2 — Evidence-Based CoT: three-step chain-of-thought protocol

Inputs:  llm_inputs/for_gemini_e{1,2,3}.csv  (participant_ID, text)
Outputs: claude_code_scripts/outputs/llm_pred_p{1,2}_exp{1,2,3}.csv
         (Participant_ID, Openness, Conscientiousness, Extroversion,
          Agreeableness, Neuroticism)

Progress is cached after each participant so the script is fully resumable.
Set ANTHROPIC_API_KEY before running.
"""

import os
import json
import time
import re
import csv
import subprocess
import shutil

# ── paths ─────────────────────────────────────────────────────────────────────
BASE_DIR  = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
INPUT_DIR = os.path.join(BASE_DIR, "llm_inputs")
OUT_DIR   = os.path.join(BASE_DIR, "claude_code_scripts", "outputs")
os.makedirs(OUT_DIR, exist_ok=True)

MODEL = "claude-haiku-4-5-20251001"
CLI_MODEL = "haiku"  # short alias accepted by the claude CLI

# ── backend detection ─────────────────────────────────────────────────────────
# Use the Anthropic Python SDK when ANTHROPIC_API_KEY is set; otherwise fall
# back to the local claude CLI (which uses its own stored credentials).
_API_KEY  = os.environ.get("ANTHROPIC_API_KEY")
_CLI_PATH = shutil.which("claude") or os.path.expanduser("~/.npm-global/bin/claude")

if _API_KEY:
    try:
        import anthropic as _anthropic
        _SDK_CLIENT = _anthropic.Anthropic(api_key=_API_KEY)
        USE_SDK = True
    except ImportError:
        USE_SDK = False
else:
    USE_SDK = False

if not USE_SDK and not os.path.exists(_CLI_PATH):
    raise EnvironmentError(
        "Neither ANTHROPIC_API_KEY (for the Python SDK) nor the claude CLI "
        f"was found at {_CLI_PATH}.\n"
        "Set ANTHROPIC_API_KEY or install Claude Code."
    )

# ── prompts ───────────────────────────────────────────────────────────────────

# Prompt 1 — Lexical Hypothesis
# Based on Prompt 1 in prompts.docx: expert Psychometrician using the Lexical
# Hypothesis to score personality from linguistic markers in informal chat logs.
PROMPT_1_SYSTEM = (
    "You are an expert Psychometrician and Personality Scientist specialising "
    "in the Lexical Hypothesis of personality, which states that personality "
    "traits are reflected in language use. Your task is to analyse chat "
    "interactions and estimate the user's Big Five personality traits from "
    "linguistic markers only. "
    "Respond with valid JSON only — no markdown, no explanation, no other text."
)

PROMPT_1_TEMPLATE = """\
Analyse ONLY the USER messages provided below and score each Big Five trait \
on a 0–10 scale using these linguistic markers:

- Openness (0=conventional, 10=inventive/curious): complex vocabulary, \
discussions of art, fiction, or abstract ideas, curiosity, creative language. \
High markers: "dream", "theory", "culture". Low markers: concrete, routine topics.

- Conscientiousness (0=disorganised, 10=efficient/organised): words related \
to work, schedules, goals, and achievement. \
High markers: "plan", "work", "ready". Low markers: disorganised, spontaneous.

- Extraversion (0=introverted, 10=outgoing/energetic): high word count, \
social words ("we", "friends", "party", "people"), positive emotion, enthusiasm. \
High markers: energetic, social. Low markers: "I", "solo", withdrawn.

- Agreeableness (0=competitive/critical, 10=friendly/compassionate): inclusive \
language, agreement, positive social emotion, empathic terms. \
High markers: "love", "yes", "thanks". Low markers: anger, conflict, criticism.

- Neuroticism (0=calm/resilient, 10=sensitive/nervous): negative emotion, \
anxiety, worry, and self-referential distress. \
High markers: "worry", "sad", "awful", "stress". Low markers: calm, stable.

Return ONLY a valid JSON object with numeric values (no other text):
{{"Openness": X, "Conscientiousness": X, "Extraversion": X, "Agreeableness": X, "Neuroticism": X}}

User messages:
{text}"""


# Prompt 2 — Evidence-Based Chain-of-Thought
# Based on Prompt 5 in prompts.docx: Lead Psychometrician with a structured
# three-step protocol (linguistic extraction → behavioural mapping → scoring).
PROMPT_2_SYSTEM = (
    "You are a Lead Psychometrician and Data Scientist specialising in "
    "Computational Psychology and the Big Five (OCEAN) personality model. "
    "You conduct evidence-based personality assessments using a structured "
    "chain-of-thought protocol. "
    "Respond with valid JSON only — no markdown, no explanation, no other text."
)

PROMPT_2_TEMPLATE = """\
Analyse ONLY the USER messages provided below using this three-step protocol:

STEP 1 — Linguistic Extraction: identify patterns in pronoun usage \
(I vs we/they), sentence complexity, vocabulary richness, emotional valence \
(positive / negative / neutral), and topic domains (social, work, creative, \
abstract). Note: high first-person singular ("I", "my") often correlates with \
Neuroticism; high social words correlate with Extraversion; complex syntax \
correlates with Openness.

STEP 2 — Behavioural Mapping: map the linguistic patterns to Big Five traits:
  Openness        (imagination, abstract ideas, vocabulary complexity)
  Conscientiousness (orderliness, duty, achievement-striving)
  Extraversion    (social engagement, assertiveness, positive energy)
  Agreeableness   (trust, altruism, compliance, modesty)
  Neuroticism     (anxiety, anger, depression, self-consciousness)

STEP 3 — Scoring: assign a score 1–10 for each trait.
  Scale: 1 = very low, 5 = population average, 10 = very high.

After completing all three steps output ONLY a valid JSON object (no other text):
{{"Openness": X, "Conscientiousness": X, "Extraversion": X, "Agreeableness": X, "Neuroticism": X}}

User messages:
{text}"""


# Prompt 3 — Persona Simulation
# Absorb the user's persona, compose an internal first-person monologue, then
# self-rate the Big Five from within that persona.  The two-stage design avoids
# the external-assessor bias of Prompts 1 and 2.
PROMPT_3_SYSTEM = (
    "You are an expert psychologist specialising in personality simulation. "
    "You will read a person's chat messages, deeply absorb their persona, and "
    "then assess their personality from the inside — as if you were that person "
    "reflecting on themselves. "
    "Respond with valid JSON only — no markdown, no extra text."
)

PROMPT_3_TEMPLATE = """\
STEP 1 — ABSORB THE PERSONA: Read every User message below carefully. \
Note their vocabulary level, sentence length, topics they choose, emotional \
tone, how they relate to others, what they seem to value, and what they avoid.

STEP 2 — INTERNAL MONOLOGUE (think, do not output): As this person, compose a \
brief first-person reflection beginning with "I tend to…" (50–70 words) that \
describes how they typically think, feel, and engage with the world.

STEP 3 — SELF-ASSESSMENT: Now, still AS this person, rate yourself on the \
Big Five personality traits on a 1–10 scale \
(1 = very low, 5 = population average, 10 = very high):
  • How extraverted/energetic am I?          → Extraversion
  • How friendly/compassionate am I?         → Agreeableness
  • How organised/self-disciplined am I?     → Conscientiousness
  • How anxious/emotionally reactive am I?   → Neuroticism
  • How curious/open to new ideas am I?      → Openness

Output ONLY a valid JSON object with numeric scores (no other text):
{{"Openness": X, "Conscientiousness": X, "Extraversion": X, "Agreeableness": X, "Neuroticism": X}}

User messages:
{text}"""


# ── helpers ───────────────────────────────────────────────────────────────────
TRAITS = ["Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism"]
# Column names in output CSV (Extraversion stored as Extroversion for R compatibility)
FIELDNAMES = ["Participant_ID", "Openness", "Conscientiousness",
              "Extroversion", "Agreeableness", "Neuroticism"]


def extract_user_messages(text: str) -> str:
    """
    Extract only the User's turns from a conversation string.
    Format: "GPT: msg; User: msg; GPT: msg; ..."
    Returns all user messages joined with a space.
    """
    segments = text.split(";")
    parts = []
    for seg in segments:
        cleaned = seg.strip()
        if cleaned.startswith("User:"):
            parts.append(cleaned[len("User:"):].strip())
    return " ".join(parts)


def extract_json(text: str) -> dict | None:
    """
    Three-tier JSON extraction from an LLM response:
    1. Direct parse (model followed instructions)
    2. Regex block extraction (handles markdown / preamble)
    3. Per-trait key-value regex fallback
    """
    text = text.strip()

    # Tier 1 — direct parse
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        pass

    # Tier 2 — extract first {...} block
    match = re.search(r"\{[^{}]+\}", text, re.DOTALL)
    if match:
        try:
            return json.loads(match.group())
        except json.JSONDecodeError:
            pass

    # Tier 3 — per-trait regex
    result = {}
    for trait in TRAITS:
        m = re.search(rf'"{trait}"\s*:\s*([0-9]+(?:\.[0-9]+)?)', text)
        if m:
            result[trait] = float(m.group(1))
    if len(result) == len(TRAITS):
        return result

    return None


def call_claude(
    system: str, user_msg: str, retries: int = 3, max_tokens: int = 256
) -> dict | None:
    """Call Claude (SDK or CLI) and return parsed JSON scores, or None on failure."""
    for attempt in range(retries):
        try:
            if USE_SDK:
                response = _SDK_CLIENT.messages.create(
                    model=MODEL,
                    max_tokens=max_tokens,
                    system=system,
                    messages=[{"role": "user", "content": user_msg}],
                    temperature=0.0,
                )
                output = response.content[0].text.strip()
            else:
                result = subprocess.run(
                    [_CLI_PATH, "--print", "--model", CLI_MODEL,
                     "--system-prompt", system,
                     "--no-session-persistence",
                     user_msg],
                    capture_output=True, text=True, timeout=120,
                )
                output = result.stdout.strip()

            scores = extract_json(output)
            if scores and all(t in scores for t in TRAITS):
                return scores
            print(f"    [parse fail attempt {attempt + 1}] raw: {output[:120]!r}")

        except subprocess.TimeoutExpired:
            print(f"    Timeout (attempt {attempt + 1}), retrying …")
        except Exception as exc:
            # Includes SDK errors (RateLimitError, APIStatusError, etc.)
            exc_name = type(exc).__name__
            if "RateLimit" in exc_name:
                wait = 60 * (attempt + 1)
                print(f"    Rate limit (attempt {attempt + 1}), waiting {wait}s …")
                time.sleep(wait)
                continue
            print(f"    Error [{exc_name}] (attempt {attempt + 1}): {exc}")

        time.sleep(2 * (attempt + 1))
    return None


def read_csv(path: str) -> list[dict]:
    with open(path, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def write_csv(path: str, rows: list[dict], fieldnames: list[str]) -> None:
    with open(path, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def load_progress(path: str) -> dict:
    if not os.path.exists(path):
        return {}
    with open(path, encoding="utf-8") as f:
        data = json.load(f)
    # Auto-retry: remove any entries where all traits are NaN (transient failures)
    nan_keys = [
        pid for pid, rec in data.items()
        if all(str(rec.get(t, "")) in ("nan", "NaN", "None", "") for t in FIELDNAMES[1:])
    ]
    for k in nan_keys:
        del data[k]
        print(f"  [retry] cleared stale NaN cache entry for participant {k}")
    return data


def save_progress(path: str, data: dict) -> None:
    with open(path, "w", encoding="utf-8") as f:
        json.dump(data, f)


# ── main processing ───────────────────────────────────────────────────────────
def process_experiment(exp_num: int) -> None:
    input_path   = os.path.join(INPUT_DIR, f"for_gemini_e{exp_num}.csv")
    participants = read_csv(input_path)
    print(f"\n=== Experiment {exp_num}: {len(participants)} participants ===")

    # (prompt_id, system, template, max_tokens)
    prompts = [
        (1, PROMPT_1_SYSTEM, PROMPT_1_TEMPLATE, 256),
        (2, PROMPT_2_SYSTEM, PROMPT_2_TEMPLATE, 256),
        (3, PROMPT_3_SYSTEM, PROMPT_3_TEMPLATE, 600),
    ]

    for prompt_id, system_prompt, template, max_tok in prompts:
        progress_path = os.path.join(OUT_DIR, f"progress_p{prompt_id}_exp{exp_num}.json")
        out_path      = os.path.join(OUT_DIR, f"llm_pred_p{prompt_id}_exp{exp_num}.csv")

        progress = load_progress(progress_path)
        results  = []

        for idx, row in enumerate(participants):
            pid  = str(row["participant_ID"]).strip()
            text = row["text"].strip()

            if pid in progress:
                results.append(progress[pid])
                print(f"  [P{prompt_id} Exp{exp_num}] {idx + 1}/{len(participants)} {pid} (cached)")
                continue

            user_text = extract_user_messages(text)
            if not user_text:
                print(f"  WARNING: no User messages for {pid} — writing NaN")
                record = {"Participant_ID": pid, **{t: float("nan") for t in TRAITS}}
                results.append(record)
                progress[pid] = record
                save_progress(progress_path, progress)
                continue

            print(
                f"  [P{prompt_id} Exp{exp_num}] {idx + 1}/{len(participants)} {pid} …",
                end=" ", flush=True,
            )
            user_msg = template.format(text=user_text)
            scores   = call_claude(system_prompt, user_msg, max_tokens=max_tok)

            if scores is None:
                print("FAILED — using NaN")
                scores = {t: float("nan") for t in TRAITS}

            # Store Extraversion as Extroversion to match R script conventions
            record = {
                "Participant_ID":   pid,
                "Openness":         scores.get("Openness",          float("nan")),
                "Conscientiousness": scores.get("Conscientiousness", float("nan")),
                "Extroversion":     scores.get("Extraversion",      float("nan")),
                "Agreeableness":    scores.get("Agreeableness",     float("nan")),
                "Neuroticism":      scores.get("Neuroticism",       float("nan")),
            }
            print(
                f"O={record['Openness']} C={record['Conscientiousness']} "
                f"E={record['Extroversion']} A={record['Agreeableness']} "
                f"N={record['Neuroticism']}"
            )

            results.append(record)
            progress[pid] = record
            save_progress(progress_path, progress)
            time.sleep(0.3)

        write_csv(out_path, results, FIELDNAMES)
        print(f"  Saved → {out_path}")


def main() -> None:
    backend = "Anthropic SDK" if USE_SDK else f"Claude CLI ({_CLI_PATH})"
    print(f"Backend: {backend}")
    for exp_num in [1, 2, 3, 4]:
        process_experiment(exp_num)
    print("\nAll experiments complete.")


if __name__ == "__main__":
    main()
