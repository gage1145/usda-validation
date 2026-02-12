from dotenv import load_dotenv
import os
from pyairtable import Api


load_dotenv()
KEY = os.getenv('KEY')
APP = os.getenv('APP')
TBL = os.getenv('TBL')

api = Api(KEY)
table = api.table(APP, TBL)
print(table.all())