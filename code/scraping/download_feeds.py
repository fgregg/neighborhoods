import MySQLdb
import urllib2
import time
import logging
import optparse
import config
import sys

optp = optparse.OptionParser()
optp.add_option('-v', '--verbose', dest='verbose', action='count',
                help='Increase verbosity (specify multiple times for more)'
                )
(opts, args) = optp.parse_args()
log_level = logging.WARNING 
if opts.verbose == 1:
    log_level = logging.INFO
elif opts.verbose >= 2:
    log_level = logging.DEBUG
logging.basicConfig(level=log_level)

string_chunk = config.string_chunk

db = MySQLdb.connect(db="neighborhood",
                     read_default_file="~/.my.cnf")
c = db.cursor()

for city in config.cities :
    if city == "newyork" :
        feeds = config.ny_feeds
    else :
        feeds = config.std_feeds 

    for section, url in feeds :
        url = url % city
        logging.info('%s', city)
        logging.info('%s', section)
        logging.info('%s', url)

        try:
            listing = urllib2.urlopen(url, timeout=60).read()
        except urllib2.HTTPError as e:
            print e
            print url
            continue

        listing = [listing[i:i + string_chunk]
                   for i in range(0, len(listing), string_chunk)]

        c.execute("""INSERT INTO rss (section, url, raw, city) """
                  """VALUES (%s, %s, %s, %s) """,
                  (section, url, listing.pop(0), city))

        rss_id = db.insert_id()

        for i, packet in enumerate(listing) :
            offset = (1 + i) * string_chunk
            c.execute("""UPDATE rss SET raw = INSERT(raw, %s, %s, %s) """
                      """WHERE rss_id = %s""",
                      (offset, string_chunk, packet, rss_id))

        db.commit()
        time.sleep(1)    

c.close()
db.close()
