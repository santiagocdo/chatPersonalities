"""
infer_interactions_exp4.py

Figure 3: How does interaction volume affect personality prediction accuracy
for Experiment 4 (N=104)?

Chronological rolling-window design (sequential, resumable):
  For each participant, slide a window of size k across their first 26
  messages (chronological order, no shuffling).
    k = 26  -> 1 window
    k = 16  -> 11 windows  (slices [0..16), [1..17), ..., [10..26))
    k = 6   -> 21 windows
    k = 1   -> 26 windows
  Per participant: 1 + 11 + 21 + 26 = 59 windows x 3 prompts = 177 calls.
  Total: 18,408 calls across 104 participants.

Per-window lexical metrics recorded alongside the personality estimates:
  n_interactions, total_words, mean_words, total_chars, mean_chars,
  unique_words, ttr (type-token ratio), mean_sentence_len.

Outputs:
  claude_code_scripts/outputs/interaction_pred_exp4.csv
  claude_code_scripts/outputs/progress_interactions_exp4_windows.json

Usage:
  python3 claude_code_scripts/infer_interactions_exp4.py
"""

import os
import sys
import json
import csv
import re

# Import shared infrastructure (prompts, call_claude, traits, backend flag)
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from infer_personalities import (
    PROMPT_1_SYSTEM, PROMPT_1_TEMPLATE,
    PROMPT_2_SYSTEM, PROMPT_2_TEMPLATE,
    PROMPT_3_SYSTEM, PROMPT_3_TEMPLATE,
    TRAITS, call_claude, USE_SDK,
)

# ── configuration ────────────────────────────────────────────────────────────
STEP_LEVELS = [26, 16, 6, 1]
MAX_MSGS    = 26          # truncate every participant to this many messages
SAVE_EVERY  = 25          # cache flush cadence (in completed calls)

PROMPTS = {
    1: (PROMPT_1_SYSTEM, PROMPT_1_TEMPLATE, 256),
    2: (PROMPT_2_SYSTEM, PROMPT_2_TEMPLATE, 256),
    3: (PROMPT_3_SYSTEM, PROMPT_3_TEMPLATE, 600),
}

# ── paths ────────────────────────────────────────────────────────────────────
BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT_DIR  = os.path.join(BASE_DIR, "claude_code_scripts", "outputs")
os.makedirs(OUT_DIR, exist_ok=True)

INTERACTIONS_PATH = os.path.join(BASE_DIR, "exp4_n104_interactions.csv")
PROGRESS_PATH     = os.path.join(OUT_DIR, "progress_interactions_exp4_windows.json")
OUTPUT_PATH       = os.path.join(OUT_DIR, "interaction_pred_exp4.csv")


# ── progress cache ───────────────────────────────────────────────────────────
def load_progress() -> dict:
    if not os.path.exists(PROGRESS_PATH):
        return {}
    with open(PROGRESS_PATH, encoding="utf-8") as f:
        return json.load(f)


def save_progress(progress: dict) -> None:
    with open(PROGRESS_PATH, "w", encoding="utf-8") as f:
        json.dump(progress, f)


# ── participant data ─────────────────────────────────────────────────────────
def load_interactions() -> dict:
    """Return {pid: [(usermessage, userwordnums), ...]} in CSV (chronological) order."""
    participants: dict = {}
    with open(INTERACTIONS_PATH, newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            pid = row["userid"].strip()
            msg = row["usermessage"].strip()
            wc  = int(float(row["userwordnums"]))
            if not msg:
                continue
            participants.setdefault(pid, []).append((msg, wc))
    print(f"Loaded {len(participants)} participants")
    for pid in participants:
        participants[pid] = participants[pid][:MAX_MSGS]
    return participants


# ── lexical metrics ──────────────────────────────────────────────────────────
_WORD_RE       = re.compile(r"[a-z]+")
_SENT_SPLIT_RE = re.compile(r"[.!?]+")


def compute_metrics(slice_msgs: list) -> dict:
    """Lexical profile for a window slice (list of (text, word_count) tuples)."""
    n_int = len(slice_msgs)
    full_text   = " ".join(m for m, _ in slice_msgs)
    total_words = sum(w for _, w in slice_msgs)
    total_chars = sum(len(m) for m, _ in slice_msgs)

    tokens   = _WORD_RE.findall(full_text.lower())
    n_tokens = len(tokens)
    unique   = len(set(tokens))

    sentences = [s for s in _SENT_SPLIT_RE.split(full_text) if s.strip()]
    if sentences:
        sent_word_counts = [len(_WORD_RE.findall(s.lower())) for s in sentences]
        mean_sent_len = sum(sent_word_counts) / len(sentences)
    else:
        # No sentence terminator (e.g. single short utterance): treat the window as one sentence
        mean_sent_len = float(n_tokens)

    return {
        "n_interactions":    n_int,
        "total_words":       total_words,
        "mean_words":        (total_words / n_int) if n_int else 0.0,
        "total_chars":       total_chars,
        "mean_chars":        (total_chars / n_int) if n_int else 0.0,
        "unique_words":      unique,
        "ttr":               (unique / n_tokens) if n_tokens else 0.0,
        "mean_sentence_len": mean_sent_len,
    }


# ── windows ──────────────────────────────────────────────────────────────────
def build_windows(participants: dict) -> dict:
    """{(pid, k, window_start): (concatenated_text, metrics_dict)}"""
    windows: dict = {}
    for pid, msgs in participants.items():
        n_msgs = len(msgs)
        for k in STEP_LEVELS:
            if k > n_msgs:
                continue
            for start in range(n_msgs - k + 1):
                slice_msgs = msgs[start:start + k]
                text       = " ".join(m for m, _ in slice_msgs)
                metrics    = compute_metrics(slice_msgs)
                windows[(pid, k, start)] = (text, metrics)
    return windows


# ── main loop ────────────────────────────────────────────────────────────────
def main():
    participants = load_interactions()
    windows      = build_windows(participants)
    progress     = load_progress()

    print(f"Backend: {'Anthropic SDK' if USE_SDK else 'Claude CLI'}")
    print(f"Windows pre-computed: {len(windows)}")
    print(f"Step levels: {STEP_LEVELS}")
    print("Per participant: 1 + 11 + 21 + 26 = 59 windows x 3 prompts = 177 calls")

    # Task list (uncached only)
    tasks = []
    for (pid, k, start), (text, _) in windows.items():
        for prompt_id in (1, 2, 3):
            if f"{pid}_{k}_{start}_{prompt_id}" not in progress:
                tasks.append((pid, k, start, prompt_id, text))

    total  = len(tasks)
    cached = sum(
        1
        for (pid, k, s), _ in windows.items()
        for p in (1, 2, 3)
        if f"{pid}_{k}_{s}_{p}" in progress
    )
    print(f"Cached: {cached}, Remaining: {total}")

    # Sequential — one call at a time (subscription-friendly)
    completed = 0
    for pid, k, start, prompt_id, text in tasks:
        system, template, max_tok = PROMPTS[prompt_id]
        user_msg = template.format(text=text)
        scores   = call_claude(system, user_msg, max_tokens=max_tok)
        if scores is None:
            scores = {t: float("nan") for t in TRAITS}
        progress[f"{pid}_{k}_{start}_{prompt_id}"] = scores

        completed += 1
        if completed % SAVE_EVERY == 0 or completed == total:
            save_progress(progress)
            pct = 100 * completed / total
            print(
                f"  [{completed}/{total}] ({pct:.1f}%) "
                f"pid={pid} k={k} start={start} p={prompt_id}",
                flush=True,
            )

    save_progress(progress)
    print("Inference complete." if total else "All windows already cached.")

    # ── write per-window CSV ────────────────────────────────────────────────
    fieldnames = [
        "participant_ID", "k", "window_start", "window_end",
        "n_interactions", "total_words", "mean_words",
        "total_chars", "mean_chars",
        "unique_words", "ttr", "mean_sentence_len",
    ]
    for p in (1, 2, 3):
        for t in ("Openness", "Conscientiousness", "Extroversion",
                   "Agreeableness", "Neuroticism"):
            fieldnames.append(f"p{p}_{t}")

    rows = []
    for (pid, k, start), (_, metrics) in sorted(windows.items()):
        row = {
            "participant_ID": pid,
            "k":              k,
            "window_start":   start,
            "window_end":     start + k,
            **metrics,
        }
        ok = True
        for prompt_id in (1, 2, 3):
            scores = progress.get(f"{pid}_{k}_{start}_{prompt_id}")
            if scores is None:
                ok = False
                break
            for t in TRAITS:
                out_name = "Extroversion" if t == "Extraversion" else t
                row[f"p{prompt_id}_{out_name}"] = scores.get(t, float("nan"))
        if ok:
            rows.append(row)

    with open(OUTPUT_PATH, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)

    print(f"\nCSV written: {OUTPUT_PATH}")
    print(f"  Rows: {len(rows)} (expected: 104 x 59 = 6,136)")


if __name__ == "__main__":
    main()
