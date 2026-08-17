from dotenv import load_dotenv
import os
import argparse
from pyairtable import Api
from pyairtable.formulas import match
from datetime import datetime
from models import Technician, Sample, Reaction, Result
import pandas as pd
from pathlib import Path
from tqdm import tqdm
from rich import print


load_dotenv()
KEY = os.getenv('KEY')
app = "app7KsgYl2jhOnYg7"

api = Api(KEY)
base = api.base(app)
print(f"[bold green]Connected to Airtable Base[/bold green]: [bold blue]{app}[/bold blue]\n")

home_dir = Path("")
data_dir = home_dir / "data" / "processedSamples"
raw_dir = home_dir / "raw" / "processedSamples"

parser = argparse.ArgumentParser(description="Update Airtable with new reactions and results.")
parser.add_argument("--only-new-reactions", action="store_true", help="Update Results table with only reactions that have no associated results.")
parser.add_argument("--skip-reactions", action="store_true", help="Skip updating the Reactions table and only update the Results table.")
parser.add_argument("--dry-run", action="store_true", help="Run the script without saving the results to Airtable.")
args = parser.parse_args()

skip_reactions = args.skip_reactions
only_new_reactions = args.only_new_reactions
dry_run = args.dry_run

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
def parse_reaction(file):
    return file.name.replace(".xlsx", "")

reaction_list      = list(raw_dir.rglob("*.xlsx"))
reactions          = list(map(parse_reaction, reaction_list))
airtable_reactions = Reaction.all()
existing_rxn_names = set([rxn.rxn_name for rxn in airtable_reactions])
new_reactions      = [rxn for rxn in reactions if rxn not in existing_rxn_names]

print("[bold green]Retrieving samples from Airtable...[/bold green]")
airtable_samples   = Sample.all()
print(f"[bold green]Retrieved {len(airtable_samples)} samples from Airtable.[/bold green]\n")

# Update Reaction Table
def update_reaction(rxn):
    existing_rxn = rxn in existing_rxn_names

    if not existing_rxn:
        rxn_split     = rxn.split("_")
        assay         = rxn_split[rxn.count("_")]
        date_raw      = rxn_split[0]
        date_join     = "-".join([date_raw[:4], date_raw[4:6], date_raw[6:8]])
        date          = datetime.strptime(date_join, "%Y-%m-%d")
        reader        = rxn_split[1]
        tech_initials = rxn_split[2]
        technician    = [Technician.first(formula=tech_formula(tech_initials))]
        
        reaction = Reaction(
            rxn_name    = rxn,
            assay       = assay,
            date        = date,
            technician  = technician,
            reader      = reader,
            temperature = 42
        )
        return reaction

if not skip_reactions: 
    reactions_to_save = [rxn for rxn in map(update_reaction, reactions) if rxn]
    if reactions_to_save:
        print(f"\n[bold green]Saving {len(reactions_to_save)} reactions to Airtable...[/bold green]")
        Reaction.batch_save(reactions_to_save)
    else:
        print("[bold yellow]No new reactions to save.[/bold yellow]\n")
else:
    print("[bold yellow]Skipping updating Reactions table. Only updating Results table.[/bold yellow]\n")

if new_reactions:
    print("[bold green]Retrieving reactions from Airtable...[/bold green]\n")
    airtable_reactions = Reaction.all()
    print(f"[bold green]Retrieved {len(airtable_reactions)} reactions from Airtable.[/bold green]\n")

rxns_no_results = set([rxn.rxn_name for rxn in airtable_reactions if not rxn.results])
print(f"[bold green]Found {len(rxns_no_results)} reactions without results.[/bold green]")
print(rxns_no_results)

def load_results(reactions, only_new_reactions=False):
    print("[bold green]Loading results from parquet files...[/bold green]")
    result_files = list(data_dir.rglob("calcs.parquet"))
    df_list = list(map(pd.read_parquet, result_files))
    df = pd.concat(df_list).rename(columns={"Sample IDs": "sample_id"})
    print(f"Loaded {len(df)} results from parquet files.")
    
    if only_new_reactions:
        def filter_new_reactions(reactions):
            def is_new(rxn): 
                return rxn in rxns_no_results
            return set(filter(is_new, reactions))
        
        print("[bold yellow]Filtering to only reactions with no associated results...[/bold yellow]\n")
        reactions = filter_new_reactions(reactions)
        df = df.loc[df["Reaction"].isin(reactions)]

    return df

# Load in Results
df = load_results(reactions, only_new_reactions=only_new_reactions)

# Pull in Samples and Reactions from Airtable
sample_df = pd.DataFrame([
    {
        "id": sample.id,
        "sample_id": sample.sample_id
    }
    for sample in airtable_samples
])

rxn_df = pd.DataFrame([
    {
        "rxn_id": reaction.id,
        "rxn_name": reaction.rxn_name
    }
    for reaction in airtable_reactions
])
rxn_df = rxn_df.loc[rxn_df["rxn_name"].isin(reactions)]

# Merge Results with Sample and Reaction IDs
df_results = df.rename(columns={'Reaction': 'rxn_name', "Wells": "well", "Dilutions": "dilution"})
df_results = df_results.merge(rxn_df, "left", on="rxn_name")
df_results = df_results.merge(sample_df, "left", on="sample_id")

print(f"[bold green]Total results to update:[/bold green] {len(df_results)}\n")

# Get Results from Airtable
if not only_new_reactions:
    def get_results(result):
        return {
            "result_id": result.id,
            "rxn_id": result.reaction[0].id,
            "well": result.well
        }
    print("[bold green]Retrieving results from Airtable...[/bold green]\n")
    airtable_results = Result.all()
    results = pd.DataFrame(map(get_results, airtable_results))
    df_results = pd.merge(df_results, results, "left", on=["rxn_id", "well"])
    print(f"Total results to update: {len(df_results)}")


# Functions for Updating the Result Table
def get_sample(row):
    sample_id = row.get("id")
    if pd.isna(sample_id):
        return None
    return [sample for sample in airtable_samples if sample_id == sample.id]

def get_reaction(row):
    rxn_id = row.get("rxn_id")
    if pd.isna(rxn_id):
        return None
    return [rxn for rxn in airtable_reactions if rxn_id == rxn.id]

def get_reactions(row): # for getting reactions from a nested column
    rxn_ids = row.get("rxn_id")
    # if not rxn_ids: return None
    rxns = [rxn for rxn_id in rxn_ids for rxn in airtable_reactions if rxn_id == rxn.id]
    if len(rxns) == 0: raise ValueError("No reactions found for row.")
    return rxns

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
    results = [result for result in airtable_results if result_id == result.id]
    if len(results) != 1:
        raise ValueError("Result query returned more than one result.")
    return results[0]

tqdm.pandas(desc="Updating Results")

def update_result(row):
    metrics = get_metrics(row)
    result = get_result(row)

    sample = get_sample(row)
    if not sample: return

    reaction = get_reaction(row)
    if not reaction: return

    if result:
        result.dilution = metrics.get("dilution")
        result.mpr = metrics.get("mpr")
        result.ms = metrics.get("ms")
        result.ttt = metrics.get("ttt")
        result.raf = metrics.get("raf")
        result.auc = metrics.get("auc")
    else:

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
updated_results = list(df_results.progress_apply(update_result, axis=1))
results_to_save = [result for result in updated_results if result]

tqdm.pandas(desc="Updating Samples")

def update_sample_reaction(row):
    sample = get_sample(row)[0]
    if not sample: return
    reactions = get_reactions(row)
    if not reactions: return
    sample.reactions = reactions
    return sample

# Update Samples with Reactions
sample_rxn_df = df_results[["id", "rxn_id"]].groupby("id").agg(list).reset_index()
updated_samples = sample_rxn_df.progress_apply(update_sample_reaction, axis=1)
samples_to_save = [sample for sample in updated_samples if sample]

# Save Samples and Results to Airtable
if not dry_run:
    print(f"\nSaving {len(samples_to_save)} samples to Airtable.")
    Sample.batch_save(samples_to_save)
    print(f"\nSaving {len(results_to_save)} results to Airtable.")
    Result.batch_save(results_to_save)