import gspread
import pandas as pd
import os

# Authenticate via service account (JSON key stored as GitHub secret)
from google.oauth2.service_account import Credentials

SCOPES = ["https://www.googleapis.com/auth/spreadsheets.readonly"]

SERVICE_ACCOUNT_JSON = "service_account.json"
SPREADSHEET_ID = os.getenv("GOOGLE_SHEET_ID")
SHEET_NAME = "Responses"

creds = Credentials.from_service_account_file(SERVICE_ACCOUNT_JSON, scopes=SCOPES)
gc = gspread.authorize(creds)

sh = gc.open_by_key(SPREADSHEET_ID)
worksheet = sh.worksheet(SHEET_NAME)
data = worksheet.get_all_records()

df = pd.DataFrame(data)
df.to_csv("data/form_responses.csv", index=False)
print("Data saved to data/form_responses.csv")
