import MySQLdb
import urllib2
import time

max_packet_size = 1048576 # Set in my.conf
byte_encoding = 5 # UTF-8 Uses up to four bytes
max_string = int(max_packet_size/byte_encoding)

cities = ["newyork",
          "losangeles",
          "chicago",
          "houston",
          "philadelphia",
          "phoenix",
          "sanantonio",
          "sandiego",
          "dallas",
#          "sanjose",
          "jacksonville",
          "indianapolis",
          "sanfrancisco",
          "austin",
          "columbus",
#          "fortworth",
          "charlotte",
          "detroit",
          "elpaso",
          "memphis",
          "baltimore",
          "boston",
          "seattle",
          "dc",
          "nashville",
          "denver",
          "louisville",
          "milwaukee",
          "portland",
          "lasvegas",
          "oklahomacity",
          "albuquerque",
          "tucson",
          "fresno",
          "sacramento",
#          "longbeach",
          "kansascity",
#         "mesa",
#          "virginiabeach",
          "atlanta",
          "cosprings",
          "omaha",
          "raleigh",
          "miami",
          "cleveland",
          "tulsa",
#          "oakland",
          "minneapolis",
          "wichita",
#          "arlington",
          "knoxville",
          "asheville"
          ]

std_feeds = [["sublet", "http://%s.craigslist.org/sub/index.rss"],
         ["room", "http://%s.craigslist.org/roo/index.rss"],
         ["apartment" , "http://%s.craigslist.org/apa/index.rss"]
        ]

ny_feeds =  [["sublet", "http://%s.craigslist.org/sub/index.rss"],
            ["room", "http://%s.craigslist.org/roo/index.rss"],
            ["apartment" , "http://%s.craigslist.org/abo/index.rss"]
            ]

db = MySQLdb.connect(db="neighborhood",
                     read_default_file="~/.my.cnf")
c = db.cursor()

for city in cities :
    if city == "newyork" :
        feeds = ny_feeds
    else :
        feeds = std_feeds 

    for section, url in feeds :
        url = url % city
        try:
            listing = urllib2.urlopen(url).read()
        except urllib2.HTTPError as e:
            print e
            print url
            continue
        listing = [listing[i:i + max_string] for i in range(0,
                                                        len(listing),
                                                        max_string)]
        c.execute(""" INSERT INTO rss (section, url, raw, city) VALUES (%s, %s, %s, %s) """,
                  (section, url, listing.pop(0), city))
        rss_id = db.insert_id()
        for i, packet in enumerate(listing) :
            offset = (1 + i) * max_string
            c.execute(""" UPDATE rss SET raw = INSERT(raw, %s, %s, %s) WHERE rss_id = %s""",
                      (offset, max_string, packet, rss_id))
        time.sleep(10)    
c.close()
db.close()
