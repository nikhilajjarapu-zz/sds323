import numpy as np 
from io import StringIO
from bs4 import BeautifulSoup
import requests

# program to collect and create dataset with kicker data

# step 0: create a kicker class to easily store information
class Kicker:
    def __init__(self, name, url, xpa, fga, ffpts):
        self.name = name
        self.url = url
        self.xpa = xpa
        self.fga = fga
        self.ffpts = ffpts
        self.hasCollegeData = False
    def getName(self):
        return self.name
    def addCollegeData(self, fgm, xpm, fga, xpa, avg_diff_xp, avg_diff_fg):
        self.college_fgm = fgm
        self.college_xpm = xpm
        self.college_fga = fga
        self.college_xpa = xpa
        self.avg_diff_xp = avg_diff_xp
        self.avg_diff_fg = avg_diff_fg
        self.hasCollegeData = True
    def calculateFPK(self):
        return np.array([self.ffpts / (self.fga + 3 * self.xpa)])
    def getXrow(self):
        if not self.hasCollegeData:
            return -1
        else:
            return np.array([self.college_fgm, self.college_xpm, self.college_fga, self.college_xpa, self.avg_diff_xp, self.avg_diff_fg])
    

# step 1: get all kickers in the NFL that played more than 16 games (1 season) between the years 2000 - 2019
# url: https://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=combined&year_min=2000&year_max=2019&season_start=1&season_end=-1&pos%5B%5D=k&draft_year_min=1936&draft_year_max=2020&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos%5B%5D=qb&draft_pos%5B%5D=rb&draft_pos%5B%5D=wr&draft_pos%5B%5D=te&draft_pos%5B%5D=e&draft_pos%5B%5D=t&draft_pos%5B%5D=g&draft_pos%5B%5D=c&draft_pos%5B%5D=ol&draft_pos%5B%5D=dt&draft_pos%5B%5D=de&draft_pos%5B%5D=dl&draft_pos%5B%5D=ilb&draft_pos%5B%5D=olb&draft_pos%5B%5D=lb&draft_pos%5B%5D=cb&draft_pos%5B%5D=s&draft_pos%5B%5D=db&draft_pos%5B%5D=k&draft_pos%5B%5D=p&c1stat=g&c1comp=gt&c1val=16&c2stat=fg_perc&c2comp=gt&c2val=1&c5val=1.0&order_by=fantasy_points

# do this step manually, csv file is located in data/kicker_data.csv

# step 2: for each kicker, calculate FP/K (fantasy points per real points -> formula is roughly (total fantasy points) / (FGA  + 3 * XPA)
# - doesn't unfairly reward long careers or super accurate kickers on small sample sizes (game limit)
# - fantasy points over regular points because fantasy scoring rewards higher points for longer kicks (test of player ability)
# - easily calculable from given table
# - one drawback: not entirely accurate, as not all FGA reward the same points (5 points for some FGA)

kicker_data = np.genfromtxt('data/kicker_data.csv', delimiter=',', dtype=None, encoding=None)[2:]
kicker_stat_map = dict()
for kicker in kicker_data:
    names = str(kicker[1]).split('\\')
    name = names[0]
    url = "https://www.pro-football-reference.com/players/" + names[1][0].capitalize() + "/" + names[1] + ".htm"
    xpa = int(kicker[11])
    fga = int(kicker[14])
    ffpts = float(kicker[19])
    kicker_stat_map[name] = Kicker(name, url, xpa, fga, ffpts) 
print("Finished step 2!")
# step 3: for every kicker, use "https://www.sports-reference.com/cfb/search/search.fcgi?search=" url to search their name.
# if there is a redirect, that means kicker exists and we can add him as a data point

data_set_names = set()
for (name, kicker_obj) in kicker_stat_map.items():
    new_url = "https://www.sports-reference.com/cfb/search/search.fcgi?search=" + "+".join(name.split(" "))
    # manual overrides
    if name == "Matt Bryant":
        new_url = "https://www.sports-reference.com/cfb/players/matt-bryant-2.html"
    if name == "Josh Brown":
        new_url = "https://www.sports-reference.com/cfb/players/josh-brown-1.html"
    if name == "Jason Elam":
        new_url = "https://www.sports-reference.com/cfb/players/jason-elam-1.html"
    if name == "Dan Bailey":
        new_url = "https://www.sports-reference.com/cfb/players/dan-bailey-1.html"
    if name == "John Carney":
        new_url = "https://www.sports-reference.com/cfb/players/john-carney-3.html"
    if name == "Dan Carpenter":
        continue
    if name == "John Hall":
        new_url = "https://www.sports-reference.com/cfb/players/john-hall-5.html"
    if name == "Gary Anderson":
        new_url = "https://www.sports-reference.com/cfb/players/gary-anderson-3.html"
    if name == "Todd Peterson":
        new_url = "https://www.sports-reference.com/cfb/players/todd-peterson-4.html"
    if name == "Jason Sanders":
        new_url = "https://www.sports-reference.com/cfb/players/jason-sanders-1.html"
    if name == "Bill Gramatica":
        new_url = "https://www.sports-reference.com/cfb/players/bill-gramatica-2.html"
    if name == "Matt Gay":
        new_url = "https://www.sports-reference.com/cfb/players/matt-gay-2.html"
    if name == "Greg Joseph":
        new_url = "https://www.sports-reference.com/cfb/players/greg-joseph-2.html"
    if name == "Steve Lindsey":
        new_url = "https://www.sports-reference.com/cfb/players/steve-lindsey-1.html"
    
        
    resp = requests.get(new_url)
    college_soup = BeautifulSoup(resp.text, "html.parser")
    result = college_soup.find_all("strong", string="0 hits")
    # only continue if kicker was found
    if len(result) == 0:
        table_div = college_soup.find_all("table", attrs={"id":["kicking", "punting"]})[0]
# step 4: from kicker page, scrape data and calculate: fg accuracy %, xp %, fga, xpa, std. dev of kicking accuracy?
        tbody_div = table_div.find_all("tbody")[0]
        xpm = np.array([int(x.text) for x in tbody_div.find_all("td", attrs={"data-stat": "xpm"}) if x.text is not ''])
        xpa = np.array([int(x.text) for x in tbody_div.find_all("td", attrs={"data-stat": "xpa"}) if x.text is not ''])
        fgm = np.array([int(x.text) for x in tbody_div.find_all("td", attrs={"data-stat": "fgm"}) if x.text is not ''])
        fga = np.array([int(x.text) for x in tbody_div.find_all("td", attrs={"data-stat": "fga"}) if x.text is not ''])
        avg_diff_xp = sum(np.diff(np.divide(xpm, xpa))) / (len(xpm) - 1) if (len(xpm) - 1) > 0 else 0
        avg_diff_fg = sum(np.diff(np.divide(fgm, fga))) / (len(fgm) - 1) if (len(fgm) - 1) > 0 else 0
        kicker_obj.addCollegeData(sum(xpm), sum(xpa), sum(fgm), sum(fga), avg_diff_xp, avg_diff_fg)

print("Finished steps 3-4!")

# step 5: export data to process in R
# TODO: make number of features (6) customizable
X_data = np.array([-1] * 6).reshape(1,6)
y_data = np.array([-1]).reshape(1,1)
statAccuracy = dict()
for kicker in kicker_stat_map.values():
    if isinstance(kicker.getXrow(), int):
        continue
    X_data = np.vstack((X_data, kicker.getXrow().reshape(1,6)))
    y_data = np.append(y_data, kicker.calculateFPK().reshape(1,1), axis=1)
    statAccuracy[kicker.getName()] = kicker.calculateFPK()
print("Finished step 5!")

y_data = y_data.reshape(87,1)
for key in sorted(statAccuracy, key=statAccuracy.get):
    print(str(key) + ": " + str(statAccuracy[key]))
np.savetxt("data/X_data.csv", X_data,delimiter=",", fmt='%.8f')
np.savetxt("data/y_data.csv",y_data,delimiter=",", fmt='%.8f')
np.savetxt("/Users/nikhil/Documents/SDS323_Spring2020/sds323/final_project/R/data/X_data.csv", X_data,delimiter=",", fmt='%.8f')
np.savetxt("/Users/nikhil/Documents/SDS323_Spring2020/sds323/final_project/R/data/y_data.csv", y_data,delimiter=",", fmt='%.8f')