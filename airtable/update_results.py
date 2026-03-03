from dotenv import load_dotenv
import os
from pyairtable import Api
from pyairtable.formulas import match
from datetime import datetime
from models import *
import pandas as pd
from pathlib import Path


load_dotenv()
KEY = os.getenv('KEY')
app = "app7KsgYl2jhOnYg7"

api = Api(KEY)
base = api.base(app)

home_dir = Path("../")
data_dir = home_dir / "data"
raw_dir = home_dir / "raw"

# Define formula functions for Airtable queries
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

# Get list of reactions from raw data directory
reaction_list = list(raw_dir.rglob("*.xlsx"))
reactions = [file.name.replace(".xlsx", "") for file in reaction_list]

# Get list of reactions not already in Airtable
new_reactions = [
    rxn
    for rxn in reactions
    if not Reaction.all(formula=rxn_formula(rxn)) 
]

# Update Airtable with new reactions
for rxn in new_reactions:

    rxn_split = rxn.split("_")

    assay = rxn_split[rxn.count("_")]
    
    date = rxn_split[0]
    date = "-".join([date[:4], date[4:6], date[6:8]])
    date = datetime.strptime(date, "%Y-%m-%d")

    reader = rxn_split[1]

    tech_initials = rxn_split[2]

    technician = [Technician.first(formula=tech_formula(tech_initials))]

    if not Reaction.first(formula=rxn_formula(rxn)):
        reaction = Reaction(
            rxn_name = rxn,
            assay = assay,
            date = date,
            technician = technician,
            reader = reader,
            temperature = 42
        )

        reaction.save()
    else:
        print(f"Entry already exists for rxn: {rxn}")

# Get list of result files from data directory
result_files = list(data_dir.rglob("calcs.parquet"))

df_list = []
for file in result_files:
    df = pd.read_parquet(file)
    df_list.append(df)
df = (
    pd
    .concat(df_list)
    .rename(columns={"Sample IDs": "sample_id"})
)

# Get list of samples and reactions from Airtable to ensure only 
# new results are being updated.
samples = pd.DataFrame([
    {
        "id": sample.id,
        "sample_id": sample.sample_id
    }
    for sample in Sample.all()
])

new_reactions = pd.DataFrame([
    {
        "id": reaction.id,
        "rxn_name": reaction.rxn_name
    }
    for reaction in
    Reaction.all(formula=match({"results": ""}))
])
new_reactions = new_reactions.rename(columns={'id':'rxn_id'})

# Make sure only samples that are listed on Airtable are being updated.
df_results = pd.merge(samples, df, "inner", on="sample_id")
df_results = df_results[df_results['Reaction'].isin(new_reactions["rxn_name"])]
df_results = df_results.rename(columns={'Reaction': 'rxn_name'})
df_results = pd.merge(df_results, new_reactions, "inner", "rxn_name")

# Create Result objects for each row in the results dataframe.
results = []
for _, row in df_results.iterrows():
    sample_id = row.get("id")
    sample = [Sample.from_id(sample_id)]
    if not sample[0].exists(): 
        raise ValueError(f"No Airtable entry for sample {sample_id}.")

    rxn_id = row.get("rxn_id")
    reaction = [Reaction.from_id(rxn_id)]
    if not reaction[0].exists():
        raise ValueError(f"No reaction exists for {rxn_id}")

    dilution = row.get("Dilutions")
    well = row.get("Wells")
    mpr = row.get("MPR")
    ms = row.get("MS")
    ttt = row.get("TtT")
    raf = row.get("RAF")
    auc = row.get("AUC")

    result = Result(
        sample = sample,
        reaction = reaction,
        dilution = dilution,
        well = well,
        mpr = mpr,
        ms = ms,
        ttt = ttt,
        raf = raf,
        auc = auc
    )
    results.append(result)

# Save results to Airtable
[result.save() for result in results]