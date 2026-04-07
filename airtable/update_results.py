from dotenv import load_dotenv
import os
from pyairtable import Api
from pyairtable.formulas import match
from datetime import datetime
from models import Technician, Sample, Reaction, Result
import pandas as pd
from pathlib import Path
from tqdm import tqdm


load_dotenv()
KEY = os.getenv('KEY')
app = "app7KsgYl2jhOnYg7"

api = Api(KEY)
base = api.base(app)

home_dir = Path("")
data_dir = home_dir / "data"
raw_dir = home_dir / "raw"

# Formulae
def rxn_formula(rxn_name):
    return match({"rxn_name": rxn_name})

def tech_formula(tech_initials):
    return match({"initials": tech_initials})

def sample_formula(sample_id):
    match({"sample_id": sample_id})

def result_formula(reaction, well):
    return match({
        "reaction": reaction,
        "well": well,
    })

# Get Reaction Names
reaction_list = list(raw_dir.rglob("*.xlsx"))
reactions = [file.name.replace(".xlsx", "") for file in reaction_list]

# Update Reaction Table
airtable_reactions = Reaction.all()
existing_rxn_names = [rxn.rxn_name for rxn in Reaction.all()]

for rxn in reactions:
    if rxn in existing_rxn_names:
        print(f"Entry already exists for rxn: {rxn}. Skipping entry.")
        continue

    rxn_split = rxn.split("_")

    assay = rxn_split[rxn.count("_")]
    
    date = rxn_split[0]
    date = "-".join([date[:4], date[4:6], date[6:8]])
    date = datetime.strptime(date, "%Y-%m-%d")

    reader = rxn_split[1]

    tech_initials = rxn_split[2]

    technician = [Technician.first(formula=tech_formula(tech_initials))]

    reaction = Reaction(
        rxn_name = rxn,
        assay = assay,
        date = date,
        technician = technician,
        reader = reader,
        temperature = 42
    )

    reaction.save()

# Load in Results
result_files = list(data_dir.rglob("calcs.parquet"))

df_list = []
for file in result_files:
    df = pd.read_parquet(file)
    df_list.append(df)
print(f"Loaded {len(df_list)} result files.")
df = pd.concat(df_list).rename(columns={"Sample IDs": "sample_id"})

# Pull in Samples and Reactions from Airtable
samples = pd.DataFrame([
    {
        "id": sample.id,
        "sample_id": sample.sample_id
    }
    for sample in Sample.all()
])

rxns = pd.DataFrame([
    {
        "rxn_id": reaction.id,
        "rxn_name": reaction.rxn_name
    }
    for reaction in airtable_reactions
])

# Merge Results with Sample and Reaction IDs
df_results = pd.merge(samples, df, "outer", on="sample_id")
df_results = df_results.rename(columns={'Reaction': 'rxn_name', "Wells": "well", "Dilutions": "dilution"})
df_results = pd.merge(df_results, rxns, "outer", "rxn_name")

# Get Results from Airtable
def get_results(result):
    return {
        "result_id": result.id,
        "rxn_id": result.reaction[0].id,
        "well": result.well
    }
all_results = Result.all(fields=["reaction", "well"])
results = pd.DataFrame(map(get_results, all_results))
df_results = pd.merge(df_results, results, "outer", on=["rxn_id", "well"])
print(f"Total results to update: {len(df_results)}")

# Functions for Updating the Result Table
def get_sample(row):
    sample_id = row.get("id")
    sample_name = row.get("sample_id")
    if pd.isna(sample_id):
        return None
    return [Sample.from_id(sample_id)]

def get_reaction(row):
    rxn_id = row.get("rxn_id")
    rxn_name = row.get("rxn_name")
    if pd.isna(rxn_id):
        return None
    return [Reaction.from_id(rxn_id)]

def get_metrics(row):
    return {    
        "well": row.get("well"),
        "dilution": row.get("dilution"),
        "mpr": row.get("MPR"),
        "ms": row.get("MS"),
        "ttt": row.get("TtT"),
        "raf": row.get("RAF"),
        "auc": row.get("AUC")
    }

def get_result(row):
    result_id = row.get("result_id")
    if pd.isna(result_id):
        return None
    return Result.from_id(result_id)

tqdm.pandas(desc="Updating Results")

def update_result(row):
    metrics = get_metrics(row)
    result = get_result(row)
    if result:
        result.dilution = metrics.get("dilution")
        result.mpr = metrics.get("mpr")
        result.ms = metrics.get("ms")
        result.ttt = metrics.get("ttt")
        result.raf = metrics.get("raf")
        result.auc = metrics.get("auc")
    else:
        sample = get_sample(row)
        if not sample:
            return

        reaction = get_reaction(row)
        if not reaction:
            return

        result = Result(
            sample = sample,
            reaction = reaction,
            dilution = metrics.get("dilution"),
            well = metrics.get("well"),
            mpr = metrics.get("mpr"),
            ms = metrics.get("ms"),
            ttt = metrics.get("ttt"),
            raf = metrics.get("raf"),
            auc = metrics.get("auc")
        )
    return result

# Generate Results to Save
updated_results = df_results.progress_apply(update_result, axis=1)
results_to_save = [result for result in updated_results if result]

# Save Results to Airtable
Result.batch_save(results_to_save)