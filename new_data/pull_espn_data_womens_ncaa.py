"""
Fetches ESPN women's college basketball boxscore pages for given game IDs,
extracts stats for specified players, and compiles them into a single table.
"""

import requests
from bs4 import BeautifulSoup
import pandas as pd

# —— CONFIGURATION —— #
HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/113.0.0.0 Safari/537.36"
    )
}
# List the ESPN game IDs you want to fetch:
# !! - To keep things consistent between the women and men
# !! - I only pulled the regular season games, no postseason
GAME_IDS = [
    '401702885', # e.g. ETSU vs. NC State, November 5, 2024
    '401702886', 
    '401702887', 
    '401702888', 
    '401702889', 
    '401724488', 
    '401729147', 
    '401702890', 
    '401702891', 
    '401702892', 
    '401720401', 
    '401702893', 
    '401724565', 
    '401724576', 
    '401724588', 
    '401724594', 
    '401724737', 
    '401724601', 
    '401724607', 
    '401724620', 
    '401724623', 
    '401724636', 
    '401724649', 
    '401724655', 
    '401724662', 
    '401724673', 
    '401724678', 
    '401724690', 
    '401724695', 
    '401743780', 
    '401743783', 
    '401743785'
    ]

# List the exact player display names you want stats for:
# !! - For now, only look at the primary starting lineup
PLAYERS = [
    "Tilda Trygger",
    "Zoe Brooks",
    "Aziaha James",
    "Saniya Rivers",
    "Madison Hayes"
    # etc.
]

# API URL for per-game summary
API_URL = (
    "https://site.api.espn.com/apis/site/v2/sports/"
    "basketball/womens-college-basketball/summary?event={game_id}"
)

def fetch_boxscore_json(game_id):
    """Return the per-team player blocks from ESPN's JSON."""
    resp = requests.get(API_URL.format(game_id=game_id), headers=HEADERS)
    resp.raise_for_status()
    return resp.json()["boxscore"]["players"]

def parse_boxscore_data(teams_data, game_id):
    """
    teams_data: list of two dicts, each with a 'statistics' list.
    Each stat-group dict has:
       - 'labels': list of column names (e.g. ["MIN","FG","3PT",…])
       - 'athletes': list of player entries, each with:
            • 'athlete' → {'displayName': …}
            • 'stats': list of strings matching 'labels'
    """
    rows = []
    for team_block in teams_data:
        for group in team_block.get("statistics", []):
            labels = group.get("labels", [])
            for entry in group.get("athletes", []):
                name = entry["athlete"]["displayName"]
                if name in PLAYERS:
                    values = entry.get("stats", [])
                    rec = dict(zip(labels, values))
                    rec["player"]  = name
                    rec["game_id"] = game_id
                    rows.append(rec)
    return rows

def main():
    all_rows = []
    for gid in GAME_IDS:
        teams = fetch_boxscore_json(gid)
        all_rows.extend(parse_boxscore_data(teams, gid))

    df = pd.DataFrame(all_rows)

    if df.empty:
        print("No matching stats found. Check that PLAYERS exactly match displayName values.")
        return

    # game_id, player first
    cols = ["game_id", "player"] + [c for c in df.columns if c not in ("game_id", "player")]
    df = df[cols]
    
    # For any games where players didn't play, fill in 0s for missing stats
    df.fillna(0, inplace=True)

    print(df.to_string(index=False))
    df.to_csv("womens_player_stats_across_games_RAW.csv", index=False)

if __name__ == "__main__":
    main()