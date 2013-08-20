import MySQLdb
import feedparser
import time
from datetime import datetime
import urllib2
from BeautifulSoup import BeautifulSoup
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
logging.StreamHandler(sys.stdout)

def getListing(url) :
    soup = BeautifulSoup(urllib2.urlopen(url, timeout=60).read())
    return str(soup.find('article', {"id" : "pagecontainer"}))
    


db = MySQLdb.connect(db="neighborhood",
                     read_default_file="~/.my.cnf")
db.set_character_set('utf8')

c = db.cursor()
c.execute('SET NAMES utf8;')
c.execute('SET CHARACTER SET utf8;')
c.execute('SET character_set_connection=utf8;')

for city in config.cities :
    logging.info(city)
    c.execute("""SELECT rss_id, raw FROM rss """
              """WHERE """
              """rss_id >= (SELECT IFNULL(MAX(rss_id),0) """
              """          FROM listing """
              """          WHERE city = %s) """
              """AND city = %s """
              """AND add_time > '2013-02-01' """
              """ORDER BY rss_id""",
              (city, city))

    rows = c.fetchall()

    for row in rows :
        rss_id, feed = row
        feed = feedparser.parse(feed)
        logging.info("rss id: %s", rss_id)
        for entry in feed.entries :
            c.execute("SELECT EXISTS(SELECT * FROM listing "
                      "              WHERE listing_id = %s)",
                      (entry.id,))
            if c.fetchone()[0] == 1 :
                continue
            logging.info("entry id: %s", entry.id)
            try:
                content = getListing(entry.id)
            except urllib2.HTTPError as e :
                if e.code == 404 :
                    logging.info("404")
                    time.sleep(1)
                    continue
                else:
                    raise
                
            time.sleep(1)
            try :
                published = datetime.fromtimestamp(time.mktime(entry.updated_parsed))
                c.execute("""REPLACE INTO listing """
                          """(listing_id, published, title, """
                          """ content, rss_id, city) """
                          """VALUES (%s, %s, %s, %s, %s, %s)""",
                          (entry.id, published, entry.title,
                           content, rss_id, city))
                db.commit()
            except TypeError as e:
                logging.info("TypeError: %s", e)


c.close()
db.close()
