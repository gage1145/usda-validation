from pyairtable.orm import Model, fields as F
from dotenv import load_dotenv
import os


load_dotenv()
KEY = os.getenv('KEY')
APP = os.getenv('APP')


# class Species(Model):
#     genus = F.SingleLineTextField("Genus")
#     species = F.SingleLineTextField("Specific Epithet")

#     class Meta:
#         base_id = APP
#         table_name = "Species"
#         api_key = KEY

class Subject(Model):
    # species = F.LinkField("Species", "Species")
    sex = F.SingleLineTextField("Sex")
    animal_id = F.RequiredSingleLineTextFieldTextField("Animal ID")
    # project = F.LinkField("Project", "Project")
    # dob = F.DateField("Date of Birth")
    # age = F.IntegerField("Age")
    # dod = F.DateField("Date of Death")

    class Meta:
        base_id = APP
        table_name = "Subjects"
        api_key = KEY

# class MNPRO(Model):
#     mnpro_id = F.IntegerField("MNPRO ID")
#     subject = F.LinkField("Subject", "Subject")

#     class Meta:
#         base_id = APP
#         table_name = "MNPRO"
#         api_key = KEY

class SampleType(Model):
    sample_type = F.SingleLineTextField("Sample Type")

    class Meta:
        base_id = APP
        table_name = "Sample Types"
        api_key = KEY

class Sample(Model):
    subject = F.LinkField("Subject ID", "Subject")
    sample_type = F.LinkField("Sample Type", "SampleType")

    class Meta:
        base_id = APP
        table_name = "Samples"
        api_key = KEY

class ProcessedSample(Model):
    sample = F.LinkField("Sample", "Sample")
    process_id = F.IntegerField("Process ID")
    conc = F.PercentField("Concentration")
    buffer = F.SingleLineTextField("Buffer")
    tech = F.LinkField("Technician", "Technician")

    class Meta:
        base_id = APP
        table_name = "Processed Samples"
        api_key = KEY

class Reaction(Model):
    reaction = F.SingleLineTextField("Reaction")
    date = F.DateField("Date")
    machine = F.LinkField("Machine", "Machine")
    temp = F.FloatField("Temperature")
    tech = F.LinkField("Technician", "Technician")

    class Meta:
        base_id = APP
        table_name = "Reactions"
        api_key = KEY

class ProcessReaction(Model):
    processed_sample = F.LinkField("Processed Sample", "ProcessedSample")
    reaction = F.LinkField("Reaction", "Reaction")

    class Meta:
        base_id = APP
        table_name = "Process Reaction"
        api_key = KEY

class Technician(Model):
    first_name = F.SingleLineTextField("First Name")
    last_name = F. SingleLineTextField("Last Name")
    email = F.EmailField("Email")
    class Meta:
        base_id = APP
        table_name = "Technicians"
        api_key = KEY

class Machine(Model):
    name = F.SingleLineTextField("Name")
    nickname = F.SingleLineTextField("Nickname")
    class Meta:
        base_id = APP
        table_name = "Machines"
        api_key = KEY

class QuicResult(Model):
    reaction = F.LinkField("Reaction", "Reaction")
    sample = F.LinkField("Processed Sample", "ProcessedSample")
    well = F.SingleLineTextField("Well")
    dilution = F.IntegerField("Dilution")
    mpr = F.FloatField("Maxpoint Ratio")
    ms = F.FloatField("Max Slope")
    ttt = F.FloatField("Time to Threshold")
    raf = F.FloatField("Rate of Amyloid Formation")
    class Meta:
        base_id = APP
        table_name = "QuIC Results"
        api_key = KEY
