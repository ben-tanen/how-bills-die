# -*- coding: utf-8 -*-

### PULLING CONGRESSIONAL DATA
### from ProPublica Congress API
### https://projects.propublica.org/api-docs/congress-api/

#################
# LOAD PACKAGES #
#################

import requests, os
import pandas as pd
from datetime import datetime

###########################
# DEFINE HELPER FUNCTIONS #
###########################

def status_message(log_file, message, loud):
    with open(log_file, 'a') as file:
        file.write("%s\n" % message)
    if loud:
        print(message)

def valid_key(key, obj):
    return key in obj.keys()

def get_bill_json(bill_type, session_id, bill_id):
    url = 'https://api.propublica.org/congress/v1/%s/bills/%s%d.json' % (session_id, bill_type, bill_id)
    response = requests.get(url, headers = {'X-API-KEY': 'u84H8fU6EwpifT34SDdrsa2wrp7tGPAU30jeLZsE'})

    if response.status_code == 200:
        return response.json()

############################
# DEFINE PARSING FUNCTIONS #
############################

# function to parse bill json and return general bill information
def parse_general_bill_data(bill_json):
    return {
        '_type': bill_json['bill_type'],
        '_id': bill_json['bill_id'],
        '_session': bill_json['congress'],
        '_short_title': bill_json['short_title'],
        '_official_title': bill_json['title'],
        '_passed_house': bill_json['house_passage'] != None,
        '_passed_senate': bill_json['senate_passage'] != None,
        '_introduced': datetime.strptime(bill_json['introduced_date'], '%Y-%M-%d'),
        '_top_subject': bill_json['primary_subject']
    }
    
# function to parse bill json and 
# return list of actions taken for a particular bill
def parse_actions_bill_data(bill_json):
    actions = [ ]
    
    # pull raw action data from json
    for action in bill_json['actions']:
        actions.append({
            '_bill': bill_json['bill_id'],
            '_id': action['id'],
            '_date': datetime.strptime(action['datetime'][:10], '%Y-%m-%d').date(),
            '_time': datetime.strptime(action['datetime'][11:19], '%H:%M:%S').time() if len(action['datetime']) > 10 else None,
            '_type': action['action_type'],
            '_action_text': action['description']
        })

    # reverse order (first action at beginning)
    actions.reverse()
          
    return actions

##################
# INIT VARIABLES #
##################

os.chdir('/Users/ben-tanen/Desktop/Projects/how-bills-die/')

loud = True

bill_type = "s"
session_id = 116

log_file = "logs/log_%s.txt" % datetime.now().strftime("%Y-%m-%d_%H-%M-%S")

####################################
# LOOP THROUGH ALL BILLS AND PARSE #
####################################

general_info = [ ]
actions = [ ]

# get list of bills
# manually entering ids as of 2019-10-09 @ 09:34:49 PM
if bill_type == "hr" and session_id == 116:
    all_bill_ids = [1] + list(range(3, 10)) + list(range(20, 4631)) 
elif bill_type == "s" and session_id == 116:
    all_bill_ids = list(range(1, 2595))

all_bill_ids.sort()

status_message(log_file, "%d total bills; last bill is %s%d" % (len(all_bill_ids), bill_type, all_bill_ids[-1]), loud)

# iterate over all bills
for bill_id in all_bill_ids:

    status_message(log_file, "--> parsing %s%d" % (bill_type, bill_id), loud)
    
    bill_json = get_bill_json(bill_type, session_id, bill_id)
    
    if bill_json['status'] != "OK":
        status_message(log_file, "--> error, got status: %s" % bill_json['status'], loud)
        continue
    elif len(bill_json['results']) != 1:
        status_message(log_file, "--> error, expecting 1 result, got " % len(bill_json['results']), loud)
        continue

    general_info.append(parse_general_bill_data(bill_json['results'][0]))
    actions += parse_actions_bill_data(bill_json['results'][0])

#######################################
# CONVERT DATA ARRAYS INTO PANDAS DFS #
#######################################

status_message(log_file, "--> converting to pandas dfs", loud)

general_df = pd.DataFrame(general_info)
actions_df = pd.DataFrame(actions)

############
# SAVE DFS #
############

status_message(log_file, "--> saving datasets", loud)

general_df.to_csv('data/2019-10-09_%s%d_general_%d-%d.csv' % (bill_type, session_id, all_bill_ids[0], all_bill_ids[-1]), index = False)
actions_df.to_csv('data/2019-10-09_%s%d_actions_%d-%d.csv' % (bill_type, session_id, all_bill_ids[0], all_bill_ids[-1]), index = False)
    