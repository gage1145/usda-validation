from dotenv import load_dotenv
import os
from pyairtable import Api
from pyairtable.models import *
import pandas as pd


load_dotenv()
KEY = os.getenv('KEY')
app = "app7KsgYl2jhOnYg7"

api = Api(KEY)
base = api.base(app)


# Get raw data



# Update raw table
raw = pd.read_parquet("../data")



# Update results table