from pyairtable import Api
from pyairtable.formulas import match
import pandas as pd
from dotenv import load_dotenv
from models import *
from pathlib import Path
from rich import print

load_dotenv()
KEY = os.getenv('KEY')
app = "app7KsgYl2jhOnYg7"

api = Api(KEY)
base = api.base(app)
home_dir = Path("")
data_dump_path = home_dir / "data" / "data_dump.parquet"

def resolve_links(df, cols):
    df = df.explode(cols)
    for col in cols:
        df[col] = [x.id if hasattr(x, 'id') else x for x in df[col]]
    return df

print("Fetching records from Airtable...")
animals   = Animal.all()
samples   = Sample.all()
reactions = Reaction.all()
results   = Result.all()
print(f"  [dim]{len(animals)} animals, {len(samples)} samples, {len(reactions)} reactions, {len(results)} results[/dim]")

df_animals = pd.DataFrame([
    {
        "animal_id": animal.id,
        "animal": animal.animal,
        "dob": animal.dob,
        "dod": animal.dod,
        "room": animal.room,
        "group": animal.group,
        "sex": animal.sex,
        "genotype": animal.genotype,
        "inoculum": animal.inoculum,
        "species": animal.species
    } for animal in animals
])

df_samples = pd.DataFrame([
    {
        "sample_id": sample.id,
        "sample": sample.sample,
        "animal_id": sample.animal_id,
        "sample_type_id": sample.sample_type_id,
        "concentration": sample.concentration,
        "mpi": sample.mpi,
        "bilateral": sample.bilateral,
        "process_date": sample.process_date,
    } for sample in samples
])
df_samples = resolve_links(df_samples, ["animal_id", "sample_type_id"])

df_reactions = pd.DataFrame([
    {
        "reaction_id": reaction.id,
        "rxn_name": reaction.rxn_name,
        "assay": reaction.assay,
        "date": reaction.date,
        "reader": reaction.reader,
        "temperature": reaction.temperature
    } for reaction in reactions
])

df_results = pd.DataFrame([
    {
        "result_id": result.result_id,
        "sample_id": result.sample_id,
        "reaction_id": result.reaction_id,
        "dilution": result.dilution,
        "well": result.well,
        "mpr": result.mpr,
        "ms": result.ms,
        "ttt": result.ttt,
        "raf": result.raf,
        "auc": result.auc
    } for result in results
])
df_results = resolve_links(df_results, ["sample_id", "reaction_id"])

print("Merging dataframes...")
df_merged = (
    df_results
    .merge(df_reactions, on="reaction_id")
    .merge(df_samples, on="sample_id")
    .merge(df_animals, on="animal_id")
)
print(f"  [dim]{len(df_merged)} rows[/dim]")

print(f"Saving to [cyan]{data_dump_path}[/cyan]...")
df_merged.to_parquet(data_dump_path)

print("Uploading to Airtable...")
data_dump_record = File.first(formula=match({"name": "data_dump"}))
data_dump_record.file.clear()
data_dump_record.save()
data_dump_record.file.upload(data_dump_path)
print("[green]Done.[/green]")
