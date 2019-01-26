import os # lettura directory
import sys # lettura da file
import re
import winsound


def my_split(text):
    text = text.replace('{"user_hash"','${"user_hash"')
    res = text.split("$")
    return res[1:]

def remove_all(array,char):
    array = list(set(array))
    array.rem(char)
    return array


def process_text(text):
    splitted = my_split(text)
    tot = len(splitted)
    for line in splitted:
        removed = False
        # rimuovo le righe senza durata
        result = re.search(r'"duration":null',line)
        index = splitted.index(line)
        if result != None:
            splitted[index]="$"
            removed = True
        # oppure senza carte giocate
        result = re.search(r'"card_history":\[\]',line)
        if result != None and not removed:
            splitted[index]="$"
            removed = True
        # oppure le arene
        result = re.search(r'"mode":"arena"',line)
        if result != None and not removed:
            splitted[index]="$"
            removed = True
##        if removed:
##            print(index)
##            for i in range( max(index-2,0),min(index+3,len(splitted)) ):    
##                print(i,": ", splitted[i])
##            input()
        
    splitted = remove_all(splitted,"$")
    print(len(splitted), "partite salvate - ", tot-len(splitted), "partite eliminate")
    return "".join(splitted)


def read_dir(path=""):
    list_of_files = os.listdir(path)

    # reset del file totale se esiste
    try:
        full_file = open("full_data.json","w")
        full_file.write("[")
        full_file.close()
    except Exception as e:
        print(e)
    
    # inserimento dei singoli file
    full_file = open("full_data.json","a")

    i=0
    #list_of_files=["2018-11.json","2018-10.json","2018-09.json"]
    for file in list_of_files:
        i+=1
        print("analizzo il file: ", file)
        if "json" in file:
            file = open(path+file,"r")
            text = file.read()
            file.close()
            # preprocessing del testo: rimuovo testa e parentesi quadre
            pos_square_bracket = text.find("[")
            text = text[pos_square_bracket+1:-2]
            # rimuovo le righe inutili
            text = process_text(text)
            # lo carico sul file finale
            if i == len(list_of_files):
                print("dai che è l'ultimo")
                full_file.write(text+"]")
            else:
                full_file.write(text+",")
    full_file.close()

def dir_to_csv(path):
    # reset del file totale se esiste
    try:
        full_file = open("games.csv","w")
        full_file.write("id,user_hash,region,mode,hero,hero_deck,opponent,opponent_deck,coin,result,rank,legend,added\n")
        full_file.close()
    except Exception as e:
        print(e)

    # reset del file totale se esiste
    try:
        full_file = open("plays.csv","w")
        full_file.write("game_id,player,turn,card_id,name,mana\n")
        full_file.close()
    except Exception as e:
        print(e)
        
    list_of_files = os.listdir(path)
    for file_name in list_of_files:
        if "json" in file_name:
            file = open(path+file_name,"r")
            json_file = file.read()
            file.close()
            print("processo il file", file_name)
            json_to_csv(json_file)
    return True
            
def json_to_csv(json):
    # preprocessing del testo: rimuovo testa e parentesi quadre
    pos_square_bracket = json.find("[")
    json = json[pos_square_bracket+1:-2]
    # prende il file json del mese e lo ritorna splittato in partite
    games = my_split(json)
    
    try:
        games_csv = open("games.csv","a")
    except Exception as e:
        print(e)

    try:
        plays_csv = open("plays.csv","a")
    except Exception as e:
        print(e)

    for game in games:
        text = game_to_csv(game)
        if text:
            games_csv.write(text[0]+"\n")
            plays_csv.write(text[1]+"\n")
    games_csv.close()
    plays_csv.close()
    return True

game_values = ["id",
               "user_hash",
               "region",
               "mode",
               "hero",
               "hero_deck",
               "opponent",
               "opponent_deck",
               "coin",
               "result",
               "rank",
               "legend",
               "added"
               ]
card_values = ["player", "turn", "id", "name", "mana"]
def game_to_csv(game):
    # prende il file json della partita e lo trasforma in csv
    if is_good_game(game):
        # trovo i dati della partita
        game_data = find_value(game,game_values[0])
        for game_value in game_values[1:]:
            game_data += ','+find_value(game, game_value)
        
        # trovo i dati delle giocate della partita
        cards_data = find_all_plays(game,  find_value(game, "id"))
        return game_data,cards_data
    return False

def find_all_plays(game, game_id):
    result = re.search(r'"card_history":',game)
    index  = result.span()[1]
    game = game[index:]
    # finchè ho stringa
    result = re.search(r'"player":.+?"player"',game)
    riga=""
    while result:
        turno = game[result.span()[0]:result.span()[1]-11]
        game = game[result.span()[1]-9:]
        giocata = game_id
        for card_value in card_values:
            giocata += "," + find_value(turno,card_value)
        riga    += giocata + "\n"
        result = re.search(r'"player":.+?"player"',game)
    return riga

def find_value(string, param):
    result = re.search(r'"'+param+'"',string)
    if not result: return ""
    index  = result.span()[1]+1
    string = string[index:]
    index  = string.find(",") if string.find(",") else string.find("}") 
    value = string[:index]
    return value.replace('"','')
    
def is_good_game(game):
    # cerco le righe senza durata
    result = re.search(r'"duration":null',game)
    if result != None:
        return False
    # oppure senza carte giocate
    result = re.search(r'"card_history":\[\]',game)
    if result != None:
        return False
    # oppure le arene
    result = re.search(r'"mode":"arena"',game)
    if result != None:
        return False
    return True
    
if __name__=="__main__":
    dir_to_csv("dati_hs/")
    duration = 1000  # millisecond
    freq = 500  # Hz
    winsound.Beep(freq, duration)
    #read_dir("dati hs/")
