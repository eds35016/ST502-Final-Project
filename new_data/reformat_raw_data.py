import pandas as pd

def reformat_stats(raw_csv_path: str) -> pd.DataFrame:
    """
    Reads raw per-game player stats and reformats into a table matching the template structure.
    Splits FG and FT into made/missed, converts MIN to HH:MM:SS as MP, and renames columns.
    """
    # Load raw data
    raw = pd.read_csv(raw_csv_path)

    # Ensure 3PT/FG/FT are strings and replace NaN
    raw['3PT'] = raw['3PT'].fillna('0-0').astype(str)
    raw['FG'] = raw['FG'].fillna('0-0').astype(str)
    raw['FT'] = raw['FT'].fillna('0-0').astype(str)
    
    # Robust parse 3PT Field Goals
    threept_parts = raw['3PT'].str.split('-', n=1, expand=True)
    raw['3PM'] = pd.to_numeric(threept_parts[0], errors='coerce').fillna(0).astype(int)
    raw['3PA'] = pd.to_numeric(threept_parts[1], errors='coerce').fillna(0).astype(int)
    raw['misses_3pt'] = raw['3PA'] - raw['3PM']

    # Robust parse Field Goals
    fg_parts = raw['FG'].str.split('-', n=1, expand=True)
    raw['FG_made'] = pd.to_numeric(fg_parts[0], errors='coerce').fillna(0).astype(int)
    raw['FG_att'] = pd.to_numeric(fg_parts[1], errors='coerce').fillna(0).astype(int)
    raw['misses'] = raw['FG_att'] - raw['FG_made']

    # Robust parse Free Throws
    ft_parts = raw['FT'].str.split('-', n=1, expand=True)
    raw['FT_made'] = pd.to_numeric(ft_parts[0], errors='coerce').fillna(0).astype(int)
    raw['FT_att'] = pd.to_numeric(ft_parts[1], errors='coerce').fillna(0).astype(int)
    raw['misses_ft'] = raw['FT_att'] - raw['FT_made']

    # Convert minutes to HH:MM:SS for MP
    raw['MP'] = raw['MIN'].apply(lambda m: f"00:{int(m):02d}:00" if pd.notnull(m) else pd.NA)
    
    raw['home'] = raw['is_home'].replace({True: 1, False: 0})

    # Assemble final DataFrame
    df = pd.DataFrame({
        'id_game':    raw['game_id'],
        'player':     raw['player'],
        'points':     raw['PTS'],
        'rebounds':   raw['REB'],
        'assists':    raw['AST'],
        'steals':     raw['STL'],
        'blocks':     raw['BLK'],
        'misses':     raw['misses'],
        'misses_ft':  raw['misses_ft'],
        'turnovers':  raw['TO'],
        'fouls':      raw['PF'],
        'home':       raw['home'],
        'FGM':        raw['FG_made'],
        'FGA':        raw['FG_att'],
        'FTM':        raw['FT_made'],
        'FTA':        raw['FT_att'],
        '3PM':        raw['3PM'],
        '3PA':        raw['3PA'],
        'MP':         raw['MP']
    })

    # Reorder columns to template order
    template_cols = [
        'id_game', 'player', 'points', 'rebounds', 'assists', 'steals', 'blocks',
        'misses', 'misses_ft', 'turnovers', 'fouls', 'home', 'FGM', 'FGA', 'FTM', 'FTA', '3PM', '3PA', 'MP'
    ]
    df = df[template_cols]

    return df

if __name__ == "__main__":
    raw_path_women = "womens_player_stats_across_games_RAW.csv"
    raw_path_men = "mens_player_stats_across_games_RAW.csv"
    out_path_women = "womens_player_stats_across_games_formatted.csv"
    out_path_men = "mens_player_stats_across_games_formatted.csv"
    reformatted_women = reformat_stats(raw_path_women)
    reformatted_men = reformat_stats(raw_path_men)
    reformatted_women.to_csv(out_path_women, index=False)
    reformatted_men.to_csv(out_path_men, index=False)
    print(f"Reformatted table saved to {out_path_women}")
    print(f"Reformatted table saved to {out_path_men}")
    