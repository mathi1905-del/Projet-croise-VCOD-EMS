import requests
import os
import json
import time
from concurrent.futures import ThreadPoolExecutor, as_completed

# ============================================================
# CONFIG
# ============================================================

BASE = "http://172.22.215.130:8080"
TOKEN = "TOURE:b3736855980ac77c"

countries = [
    "CM","CA","UG","CI","PT","TZ","CH","DE","GB","KE","CZ","RW","FR","DK","LV",
  "KZ","RU","HR","SI","GR","IT","MA","RO","KG","GA","GE","LT","SE","SN","CG",
  "ES","GH","US","CF","AU","KR","PH","BE","NO","FI","PL","IN","NL","CN","BR",
  "CD","IL","LB","NG","BY","GW","CY","LU","UA","GQ","UZ","JP","AL","IE","AT",
  "CO","AR","AM","AO","ZA","PE","PY","TR","EE","UY","PK","ML","BF","TH","RE",
  "ID","MQ","TD","RS","HU","CU","MW","ME","SG","AZ","BG","SC","BI","TG","TW",
  "GM","SK","DZ","TJ","KW","BJ","GN","MN","PR","MD","NE","MM","ZM","GF","IS",
  "BD","VE","SD","BO","FJ","ET","DJ","IR","GP","HK","VN"
]

years = range(1987, 2026)   # 1987 -> 2025 inclus
quarters = [1, 2, 3, 4]

datasets = ["sequences", "biomedical", "cancer", "social"]

# ============================================================
# FICHIERS DE SORTIE
# ============================================================

OUT_FASTA = "ALL_sequences.fasta"
OUT_BIO   = "ALL_biomedical.csv"
OUT_CAN   = "ALL_cancer.csv"
OUT_SOC   = "ALL_social.csv"

STATE_FILE = "state.json"

# ============================================================
# OUTILS
# ============================================================

def get_output_file(dataset):
    if dataset == "sequences":
        return OUT_FASTA
    if dataset == "biomedical":
        return OUT_BIO
    if dataset == "cancer":
        return OUT_CAN
    if dataset == "social":
        return OUT_SOC
    return None


def load_state():
    if os.path.exists(STATE_FILE):
        try:
            with open(STATE_FILE, "r", encoding="utf-8") as f:
                state = json.load(f)
                if isinstance(state, dict):
                    return state
        except Exception:
            pass

    return {
        "done": [],                # liste des trimestres déjà téléchargés
        "headers_written": {       # pour ne pas répéter les headers CSV
            "biomedical": False,
            "cancer": False,
            "social": False
        }
    }


def save_state(state):
    with open(STATE_FILE, "w", encoding="utf-8") as f:
        json.dump(state, f, ensure_ascii=False, indent=2)


def make_key(dataset, country, trimester):
    return f"{dataset}|{country}|{trimester}"


def fetch_one(dataset, country, year, quarter):
    trimester = f"{year}Q{quarter}"
    key = make_key(dataset, country, trimester)

    url = f"{BASE}/v2/archived/{dataset}/{country}/{trimester}?token={TOKEN}"

    try:
        response = requests.get(url, timeout=60)
    except requests.RequestException as e:
        return {
            "ok": False,
            "key": key,
            "dataset": dataset,
            "country": country,
            "trimester": trimester,
            "error": str(e)
        }

    if response.status_code != 200:
        return {
            "ok": False,
            "key": key,
            "dataset": dataset,
            "country": country,
            "trimester": trimester,
            "status_code": response.status_code
        }

    content = response.text.strip()
    if not content:
        return {
            "ok": False,
            "key": key,
            "dataset": dataset,
            "country": country,
            "trimester": trimester,
            "status_code": 200,
            "empty": True
        }

    return {
        "ok": True,
        "key": key,
        "dataset": dataset,
        "country": country,
        "trimester": trimester,
        "content": content
    }


def append_sequences(content):
    with open(OUT_FASTA, "a", encoding="utf-8") as f:
        if not content.endswith("\n"):
            content += "\n"
        f.write(content)


def append_csv_content(dataset, content, state):
    out_file = get_output_file(dataset)
    lines = content.splitlines()

    if not lines:
        return

    header_written = state["headers_written"].get(dataset, False)

    # première fois : on écrit tout
    if not header_written:
        with open(out_file, "a", encoding="utf-8", newline="") as f:
            f.write("\n".join(lines) + "\n")
        state["headers_written"][dataset] = True
        return

    # après : on saute la première ligne (header)
    if len(lines) > 1:
        with open(out_file, "a", encoding="utf-8", newline="") as f:
            f.write("\n".join(lines[1:]) + "\n")


# ============================================================
# PRÉPARATION DES TÂCHES
# ============================================================

def build_tasks():
    tasks = []
    for dataset in datasets:
        for country in countries:
            for year in years:
                for q in quarters:
                    tasks.append((dataset, country, year, q))
    return tasks


# ============================================================
# BOUCLE PRINCIPALE
# ============================================================

def main():
    state = load_state()
    done_set = set(state.get("done", []))

    tasks = build_tasks()

    max_workers = 16      # tu peux monter à 24 ou 32 si ton réseau suit
    sleep_between_cycles = 3600  # 10 minutes

    print("=== Script lancé ===")
    print("Nombre total de tâches possibles :", len(tasks))
    print("Déjà enregistrées :", len(done_set))

    while True:
        print("\n=== Nouveau cycle ===")
        cycle_new = 0
        cycle_errors = 0
        cycle_skipped = 0

        pending_tasks = []
        for dataset, country, year, q in tasks:
            trimester = f"{year}Q{q}"
            key = make_key(dataset, country, trimester)

            if key in done_set:
                cycle_skipped += 1
                continue

            pending_tasks.append((dataset, country, year, q))

        print("Déjà faits ignorés :", cycle_skipped)
        print("À tester dans ce cycle :", len(pending_tasks))

        if not pending_tasks:
            print("Tout a déjà été téléchargé. Attente avant prochain cycle...")
            time.sleep(sleep_between_cycles)
            continue

        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            futures = [
                executor.submit(fetch_one, dataset, country, year, q)
                for dataset, country, year, q in pending_tasks
            ]

            for future in as_completed(futures):
                result = future.result()

                dataset = result["dataset"]
                country = result["country"]
                trimester = result["trimester"]
                key = result["key"]

                if not result["ok"]:
                    cycle_errors += 1

                    if result.get("status_code") == 200 and result.get("empty"):
                        print(f"[VIDE] {dataset} {country} {trimester}")
                    elif "status_code" in result:
                        print(f"[HTTP {result['status_code']}] {dataset} {country} {trimester}")
                    else:
                        print(f"[ERREUR] {dataset} {country} {trimester} -> {result.get('error', 'erreur inconnue')}")
                    continue

                try:
                    if dataset == "sequences":
                        append_sequences(result["content"])
                    else:
                        append_csv_content(dataset, result["content"], state)

                    done_set.add(key)
                    cycle_new += 1

                    # sauvegarde régulière
                    if cycle_new % 20 == 0:
                        state["done"] = sorted(done_set)
                        save_state(state)

                    print(f"[OK] {dataset} {country} {trimester}")

                except Exception as e:
                    cycle_errors += 1
                    print(f"[ERREUR ECRITURE] {dataset} {country} {trimester} -> {e}")

        state["done"] = sorted(done_set)
        save_state(state)

        print("\n=== Fin du cycle ===")
        print("Nouveaux téléchargements :", cycle_new)
        print("Erreurs :", cycle_errors)
        print("Total sauvegardé :", len(done_set))
        print(f"Pause de {sleep_between_cycles} secondes...\n")

        time.sleep(sleep_between_cycles)


if __name__ == "__main__":
    main()
