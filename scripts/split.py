
import sys
from collections import defaultdict
import glob
import json

db = defaultdict(list)

with open(sys.argv[1],"r") as file:
    for line in file:
        obj = json.loads(line)
        db[obj['doc_id']].append(obj)

for k in db:
    print(f"{k} => {len(db[k])}")

