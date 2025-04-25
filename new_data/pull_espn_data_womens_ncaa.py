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

def fetch_game_data(game_id):
    """Return the per-team player blocks from ESPN's JSON."""
    resp = requests.get(API_URL.format(game_id=game_id), headers=HEADERS)
    resp.raise_for_status()
    data = resp.json()

    # 1) boxscore player blocks
    players = data.get("boxscore", {}).get("players", [])

    # 2) was NC State home?
    is_home = False
    comps = data.get("header", {}).get("competitions", [])
    if comps:
        for team_block in comps[0].get("competitors", []):
            team = team_block.get("team", {})
            abb = team.get("abbreviation", "")
            name = team.get("displayName", "")
            if abb.upper() == "NCSU" or name == "NC State Wolfpack":
                is_home = (team_block.get("homeAway") == "home")
                break

    return players, is_home

def parse_boxscore_data(teams_data, game_id, is_home):
    """
    Turn ESPN's 'players' list into flat rows, filtering by PLAYERS,
    and tagging each row with game_id + is_home.
    """
    rows = []
    for team_block in teams_data:
        for group in team_block.get("statistics", []):
            labels = group.get("labels", [])
            for entry in group.get("athletes", []):
                name = entry["athlete"]["displayName"]
                if name in PLAYERS:
                    rec = dict(zip(labels, entry.get("stats", [])))
                    rec["player"]  = name
                    rec["game_id"] = game_id
                    rec["is_home"] = is_home
                    rows.append(rec)
    return rows

def main():
    all_rows = []
    for gid in GAME_IDS:
        teams, home_flag = fetch_game_data(gid)
        all_rows.extend(parse_boxscore_data(teams, gid, home_flag))

    df = pd.DataFrame(all_rows)
    if df.empty:
        print("No matching stats found. Check that PLAYERS exactly match displayName values.")
        return

    # reorder so game_id, player, is_home come first
    front = ["game_id", "player", "is_home"]
    cols = front + [c for c in df.columns if c not in front]
    df = df[cols]

    # ensure bool type & fill missing
    df["is_home"] = df["is_home"].astype(bool)
    df.fillna(0, inplace=True)

    print(df.to_string(index=False))
    df.to_csv("womens_player_stats_across_games_RAW.csv", index=False)

if __name__ == "__main__":
    main()