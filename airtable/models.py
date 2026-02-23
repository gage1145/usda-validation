from pyairtable.orm import Model, fields as F
from dotenv import load_dotenv
import os


load_dotenv()
KEY = os.getenv('KEY')
app = "app7KsgYl2jhOnYg7"


class Animal(Model):
    animal_id = F.SingleLineTextField("animal_id", readonly=True)
    sex = F.SelectField("sex", readonly=True)
    species = F.SingleLineTextField("species", readonly=True)

    class Meta:
        api_key = KEY
        base_id = app
        table_name = "animals"

class SampleType(Model):
    sample_type_id = F.AutoNumberField("sample_type_id")
    sample_type = F.SelectField("sample_type")
    mortem = F.SelectField("mortem")
    notes = F.MultilineTextField("notes")

    class Meta:
        api_key = KEY
        base_id = app
        table_name = "sample-types"

class Technician(Model):
    technician_id = F.AutoNumberField("technician_id")
    first_name = F.SingleLineTextField("first_name")
    last_name = F.SingleLineTextField("last_name")
    initials = F.SingleLineTextField("initials")
    email = F.EmailField("email")

    class Meta:
        api_key = KEY
        base_id = app
        table_name = "technicians"

class Sample(Model):
    sample_id = F.SingleLineTextField("sample_id")
    animal = F.LinkField("animal", Animal)
    sample_type_id = F.LinkField("sample_type_id", SampleType)
    sample_type = F.LookupField("sample_type")
    mortem = F.LookupField("mortem")
    concentration = F.PercentField("concentration")
    mpi = F.NumberField("mpi")
    bilateral = F.CheckboxField("bilateral")
    process_date = F.DateField("process_date")
    technician_id = F.LinkField("technician_id", Technician)
    tech_name = F.LookupField("tech_name")

    class Meta:
        api_key = KEY
        base_id = app
        table_name = "samples"

class Reaction(Model):
    rxn_name = F.SingleLineTextField("rxn_name")
    assay = F.SelectField("assay")
    date = F.DateField("date")
    technician = F.LinkField("technician", Technician)
    reader = F.SelectField("reader")
    temperature = F.NumberField("temperature")

    class Meta:
        api_key = KEY
        base_id = app
        table_name = "reactions"

class SampleReaction(Model):
    junction_id = F.AutoNumberField("junction_id")
    sample = F.LinkField("sample", Sample)
    reaction = F.LinkField("reaction", Reaction)

    class Meta:
        api_key = KEY
        base_id = app
        table_name = "sample-reaction-junctions"

class Raw(Model):
    raw_id = F.AutoNumberField("raw_id")
    sample = F.LinkField("sample", Sample)
    reaction = F.LinkField("reaction", Reaction)
    dilution = F.NumberField("dilutions")
    well = F.SingleLineTextField("well")
    time = F.NumberField("time")
    value = F.NumberField("value")

    class Meta:
        api_key = KEY
        base_id = app
        table_name = "raw"

class Result(Model):
    result_id = F.AutoNumberField("result_id")
    sample = F.LinkField("sample", Sample)
    reaction = F.LinkField("reaction", Reaction)
    dilution = F.NumberField("dilution")
    well = F.SingleLineTextField("well")
    mpr = F.NumberField("mpr")
    ms = F.NumberField("ms")
    ttt = F.NumberField("ttt")
    raf = F.NumberField("raf")
    auc = F.NumberField("auc")

    class Meta:
        api_key = KEY
        base_id = app
        table_name = "results"