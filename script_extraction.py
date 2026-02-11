import requests
import os

BASE = "http://172.22.215.130:8080"

countries = [
 "PT","CH","DE","GB","CZ","FR","DK","LV","RU","HR","SI","GR","IT","RO",
 "LT","SE","ES","BE","NO","FI","PL","NL","BY","LU","UA","AL","IE","AT",
 "EE","RS","HU","ME","BG","SK","MD","IS"
]

years = range(1987, 2026)
quarters = [1, 2, 3, 4]

# j'initialise les 4 fichiers que j'aurais à la fin
OUT_FASTA = "ALL_sequences.fasta"
OUT_BIO = "ALL_biomedical.csv"
OUT_CAN = "ALL_cancer.csv"
OUT_SOC = "ALL_social.csv"

# je supprime les fichiers si ils existent déjas
for f in [OUT_FASTA, OUT_BIO, OUT_CAN, OUT_SOC]:
 if os.path.exists(f):
 os.remove(f)

# Pour éviter de répéter l'en-tête CSV 1000 fois
header_written = {
 "biomedical": False,
 "cancer": False,
 "social": False
}

datasets = ["sequences", "biomedical", "cancer", "social"]

for dataset in datasets:
 for country in countries:
 for year in years:
 for q in quarters:
 trimester = f"{year}Q{q}"
 url = f"{BASE}/archived/{dataset}/{country}/{trimester}"
 print("GET", url)

 r = requests.get(url, timeout=60)

 if r.status_code != 200:
 continue # pas dispo = on passe

 content = r.text.strip()
 if not content:
 continue

 # je mes SEQUENCES en FASTA en concatènent
 if dataset == "sequences":
 with open(OUT_FASTA, "a", encoding="utf-8") as f:
 f.write(content + "\n")
 print("ajouté à", OUT_FASTA)

 # Les 3 autres en CSV (on concatène en gardant 1 seul header)
 else:
 lines = content.splitlines()

 # si c'est un CSV classique : première ligne = header
 if not header_written[dataset]:
 # on écrit tout (header + données)
 with open(
 OUT_BIO if dataset == "biomedical" else OUT_CAN if dataset == "cancer" else OUT_SOC,
 "a",
 encoding="utf-8"
 ) as f:
 f.write("\n".join(lines) + "\n")
 header_written[dataset] = True
 else:
 # on saute la première ligne (header) et on ajoute seulement les données
 if len(lines) > 1:
 data_only = "\n".join(lines[1:]) + "\n"
 out_file = OUT_BIO if dataset == "biomedical" else OUT_CAN if dataset == "cancer" else OUT_SOC
 with open(out_file, "a", encoding="utf-8") as f:
 f.write(data_only)

 print("ajouté à CSV", dataset)

print("___Terminé__")
print(" _", OUT_FASTA)
print(" _", OUT_BIO)
print(" _", OUT_CAN)
print(" _", OUT_SOC)
