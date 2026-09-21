from pyairtable.orm import Model, fields as F
from dotenv import load_dotenv
import os


load_dotenv()
KEY = os.getenv('KEY')
app = "app7KsgYl2jhOnYg7"


class Animal(Model):
    animal_id = F.AutoNumberField("animal_id", readonly=True)
    animal = F.SingleLineTextField("animal", readonly=True)
    dob = F.DateField("dob", readonly=True)
    dod = F.DateField("dod", readonly=True)
    room = F.SelectField("room", readonly=True)
    group = F.SelectField("group", readonly=True)
    sex = F.SelectField("sex", readonly=True)
    genotype = F.SelectField("genotype", readonly=True)
    inoculum = F.SelectField("inoculum", readonly=True)
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

class Reaction(Model):
    rxn_id = F.AutoNumberField("rxn_id")
    rxn_name = F.SingleLineTextField("rxn_name")
    assay = F.SelectField("assay")
    date = F.DateField("date")
    technician_id = F.LinkField("technician_id", Technician, lazy=True)
    reader = F.SelectField("reader")
    temperature = F.NumberField("temperature")
    results = F.LinkField("results", "Result", lazy=True)

    class Meta:
        api_key = KEY
        base_id = app
        table_name = "reactions"

class Sample(Model):
    sample_id = F.AutoNumberField("sample_id")
    sample = F.SingleLineTextField("sample")
    animal_id = F.LinkField("animal_id", Animal, lazy=True)
    sample_type_id = F.LinkField("sample_type_id", SampleType, lazy=True)
    # sample_type = F.LookupField("sample_type")
    # mortem = F.LookupField("mortem")
    concentration = F.PercentField("concentration")
    mpi = F.NumberField("mpi")
    bilateral = F.CheckboxField("bilateral")
    process_date = F.DateField("process_date")
    technician_id = F.LinkField("technician_id", Technician, lazy=True)
    # tech_name = F.LookupField("tech_name")
    reactions = F.LinkField("reactions", Reaction, lazy=True)

    class Meta:
        api_key = KEY
        base_id = app
        table_name = "samples"

class SampleReaction(Model):
    junction_id = F.AutoNumberField("junction_id")
    sample = F.LinkField("sample", Sample, lazy=True)
    reaction = F.LinkField("reaction", Reaction, lazy=True)

    class Meta:
        api_key = KEY
        base_id = app
        table_name = "sample-reaction-junctions"

# class Raw(Model):
#     raw_id = F.AutoNumberField("raw_id")
#     sample = F.LinkField("sample", Sample, lazy=True)
#     reaction = F.LinkField("reaction", Reaction, lazy=True)
#     dilution = F.NumberField("dilutions")
#     well = F.SingleLineTextField("well")
#     time = F.NumberField("time")
#     value = F.NumberField("value")

#     class Meta:
#         api_key = KEY
#         base_id = app
#         table_name = "raw"

class Result(Model):

    # def dont_be_lazy(self):
    #     self.be_lazy = False
    
    # be_lazy = True

    result_id = F.AutoNumberField("result_id")
    sample_id = F.LinkField("sample_id", Sample, lazy=True)
    reaction_id = F.LinkField("reaction_id", Reaction, lazy=True)
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

class File(Model):
    name = F.SingleLineTextField("name")
    file = F.AttachmentsField("file", validate_type=False)

    class Meta:
        api_key = KEY
        base_id = app
        table_name = "files"