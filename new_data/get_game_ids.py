import requests, json

# NC State Womens is team ID 152
url = (
    "https://site.api.espn.com/apis/site/v2/sports/"
    "basketball/womens-college-basketball/teams/152/schedule"
)
resp = requests.get(url)
resp.raise_for_status()
data = resp.json()

# 1) Get the list of events from the JSON response:
events = data.get("events")

# 2) Extract their IDs in chronological order:
game_ids = [
    event["id"]
    for event in events
]

print(game_ids)
