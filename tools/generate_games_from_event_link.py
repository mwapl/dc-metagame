import argparse
import csv
import pandas as pd
from time import gmtime, strftime
import sys

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def load_csv_to_dicts(filename):
    database = pd.read_csv(filename, sep=',', header=0)
    return database

def build_player_deck_map(correspondence_data):
    # Assuming headers are: Player, Deck
    return {row[correspondence_data.keys()[0]]: row[correspondence_data.keys()[1]] for iter,row in correspondence_data.iterrows()}
    
parser = argparse.ArgumentParser(description="Process player-deck mapping and game results.")
parser.add_argument('correspondence_file', help='CSV file with player-to-deck mappings')
parser.add_argument('results_file', help='CSV file with match results')
args = parser.parse_args()

try:
    correspondence_data = load_csv_to_dicts(args.correspondence_file)
    results_data = load_csv_to_dicts(args.results_file)
except Exception as e:
    eprint(f"Error loading files: {e}")
    exit(1)


player_not_found = []
    
player_to_deck = build_player_deck_map(correspondence_data)

for i, row in results_data.iterrows():
    table = row['Table']
    player1 = row['Player 1']
    player2 = row['Player 2']
    result = row['MatchResult']

    proceed = True
    
    # Validate players
    if player1 not in player_to_deck:
        if player1 in player_not_found:
            # Second time with see this one, by now we must have covered every unvalid player once, exit
            exit(1)
        eprint(f"Error: No deck found for player '{player1}'\r\n")
        player_not_found.append(player1)
        proceed = False
    if player2 not in player_to_deck:
        if player2 in player_not_found:
            # Second time with see this one, by now we must have covered every unvalid player once, exit
            exit(1)
        eprint(f"Error: No deck found for player '{player2}'\r\n")
        player_not_found.append(player2)
        proceed = False

    if not proceed:
        continue
        
    deck1 = player_to_deck[player1]
    deck2 = player_to_deck[player2]

    time = strftime("%d/%m/%Y %H:%M:%S", gmtime())
    
    print(f'{time};{result};{deck1};{deck2};;{player1};{player2};TOURNAMENT_NAME_PLACEHOLDER\r\n')
