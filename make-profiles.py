
import os, sys
import glob
import json
import jsonlines
import utils


class Phrase:
    def __init__(self, v): 
        self.id = v
        self.text = None   
        self.type = None   
        self.tokens = []

    def update(self):

        s = len(self.tokens)

        # if the first token is a open-double-quote and the last
        # tokens are: end-double-quote OR end-double-quote followed by
        # semi-colon
        
        if self.type == 'ex' and self.tokens[0]['form'] == '“':
            try:
                if self.tokens[s-2].get('form','') == "”" and self.tokens[s-1]['form'] == ';':
                    self.tokens = self.tokens[1:s-2]
                elif self.tokens[s-1]['form'] == "”":
                    self.tokens = self.tokens[1:s-1]
            except Exception as e:
                print(e)
                sys.exit(1)

        c, forms = 0, []
        for t in self.tokens:
            if t.get('form',None) != None:
                t['begin'] = c
                t['end'] = t['begin'] + len(t['form'])
                forms.append(t['form'])
                forms.append(t.get('sep'," "))
                c = c + len(t['form']) + len(t.get('sep'," "))
                
        self.text =  "".join(forms)


def collect_sentences(tokens, sid):
    def action(t):
        if 'action' in t:
            return t['action']
        else:
            return None
         
    ph, reading, i, sentences = None, False, 1, []
    for t in tokens:
        if action(t) == 'open' and 'def' in t['kind']:
            ph = Phrase(f"{sid}-{i}")
            reading = True
            i += 1
            ph.type = 'def'
            continue

        if action(t) == 'close' and 'def' in t['kind']:
            if len(ph.tokens) > 0:
                ph.update()
                sentences.append(ph)
            reading, ph = False, None
            continue

        if action(t) == 'open' and 'ex' in t['kind']:
            ph = Phrase(f"{sid}-{i}")
            i += 1
            ph.type = 'ex'
            reading = True
            continue

        if action(t) == 'close' and 'ex' in t['kind']:
            if len(ph.tokens) > 0:
                ph.update()
                sentences.append(ph)
            reading, ph = False, None
            continue

        if reading:
            ph.tokens.append(t)
            
    return sentences


def serialize(sentences, seq):
    out = f"data/own-{seq:02}.csv"
    ann = f"data/annotation/own-{seq:02}.jl"
    header = "i-origin@i-comment@i-input"

    with open(out, "w") as f:
        print(header, file= f)         
        for s in sentences:
            print(f"{s.id}@{s.type}@{s.text}", file= f)
    with jsonlines.open(ann, mode='w') as f:
        for s in sentences:
            obj = dict(id = s.id, text = s.text, type = s.type, tokens = s.tokens)
            f.write(obj)

            
def main():
    wn30 = utils.read_wn("/Users/ar/work/wn/Wordnet-3.0/dict/")
    res = []
    
    for fn in glob.glob("/Users/ar/work/wn/glosstag/data/*.jl"):
        for line in open(fn).readlines():
            try:
                obj = json.loads(line)
                sid = f"{obj['ofs']}-{obj['type']}"
                assert sid in wn30 and obj['text'] == wn30[sid]['gloss']
            except Exception as e:
                print(f"{sid} {fn}\n gt:[{obj['text']}]\n wn:[{wn30[sid]['gloss']}]\n\n")
            tmp = collect_sentences(obj['tokens'], sid)
            res.extend(tmp)

    n, i = 2000, 0
    for g in [res[k:k+n] for k in range(0, len(res), n)]:
        serialize(g, i)
        i += 1

main()
