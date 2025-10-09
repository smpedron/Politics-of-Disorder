## Municipal and County Politicians Email Data
## Google Civic Information API

## Python Code

import requests
import pandas as pd

# Google API key
API_KEY = 'AIzaSyAirG-ipNftEJJFDR8v_tMxgS5f6SV9bjQ'

# Define the endpoint for the Google Civic Information API
url = 'https://www.googleapis.com/civicinfo/v2/representatives'

# List of sample addresses in California and New York
addresses = [
  "Los Angeles, CA",
  "San Francisco, CA",
  "Sacramento, CA",
  "New York, NY",
  "Buffalo, NY",
  "Jersey City, NJ",
  "Newark, NJ",
  "Austin, TX",
  "Dallas, TX",
  "Houston, TX",
]

# List to store representative's data
representatives_data = []

# Grab representatives from each address
def get_representatives(address):
  params = {
    'key': API_KEY,
    'address': address
  }

# API request
response = requests.get(url, params=params)

# Check if request was successful
if response.status_code == 200:
  return response.json()
else:
  print(f"Failed to grab data for {address}")
return None

# Extract only county and municipal representatives with emails
def extract_representatives(data):
  if 'offices' in data and 'officials' in data:
  offices = data['offices']
officials = data['officials']

# Grabbing city and state from address list
city, state = address.split(", ")

for office in offices:
  # Check for both county and municipal offices
  if 'administrativeArea2' in office.get('levels', []):  # County level
  level = 'County'
elif 'locality' in office.get('levels', []):  # Municipal level
  level = 'Municipality'
else:
  level = None  # Not applicable

# Only proceed if the office is either county or municipality
if level:
  official_indices = office['officialIndices']
for index in official_indices:
  official = officials[index]
name = official.get('name', 'N/A')
email = official.get('emails', [None])[0]  # Get the first email or None
party = official.get('party', 'N/A')
office_name = office.get('name', 'N/A')

# Only add the representative if they have an email
if email:
  representatives_data.append({
    "Name": name,
    "Office": office_name,
    "Party": party,
    "Email": email,
    "City": city,
    "State": state,
    "Level": level  # Add the level (County or Municipality)
  })


# Iterate through the list of addresses to grab only municipal representatives with emails
for address in addresses:
  print(f"Local representatives for {address}...")
data = get_representatives(address)

if data:
  extract_representatives(data)


# Convert to dataframe
df = pd.DataFrame(representatives_data)

# check dataframe
print(df)

# Save the DataFrame to an Excel file
df.to_excel("municipal+county_representatives.xlsx", index=False)

# Print success message
print("Data successfully saved to municipal+county_representatives.xlsx")