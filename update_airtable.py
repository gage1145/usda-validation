from dotenv import load_dotenv
import os
from pyairtable import Api
from models import *


load_dotenv()
KEY = os.getenv('KEY')
APP = os.getenv('APP')

api = Api(KEY)
base = api.base(APP)


# subject_fields = [
#     {
#         "name": "Sex",
#         "type": "singleLineText",
#     },
#     {
#         "name": "Animal ID",
#         "type": "singleLineText",
#     },
# ]

# subject_table = base.create_table(
#     name="Subjects",
#     fields=subject_fields,
#     description="The animals used in the study."
# )

# print(f"Table '{subject_table.name}' created successfully with ID: {subject_table.id}")
