import MySQLdb
from geopy import geocoders
from string import Template
from BeautifulSoup import BeautifulSoup, Comment
import re
from collections import defaultdict
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
logging.StreamHandler(sys.stdout)

def placeFromCLTags(listing) :
    # Craigslists listings have very good meta data tags that
    # look like <-- CLTAG xstreet0==32 W. 21st st -->

    soup = BeautifulSoup(listing)
    comments = soup.findAll(text=lambda text:isinstance(text, Comment))

    x_street_0 = re.compile('xstreet0=(.+) $')
    x_street_1 = re.compile('xstreet1=(.+) $')
    geographic_area = re.compile('GeographicArea=(.+) $')

    cross_street_0 = ''
    cross_street_1 = ''
    space = ''

    # Has to be a nicer way of extracting particular tag contents from
    # the list of comment tags
    for comment in comments :
        xs0 = x_street_0.findall(comment)
        if xs0 : cross_street_0 = xs0[0].strip()
        xs1 = x_street_1.findall(comment)
        if xs1 : cross_street_1 = xs1[0].strip()
        ga  = geographic_area.findall(comment)        
        if ga : space = ga[0].strip()
    
    return (cross_street_0, cross_street_1, space)

def addressOrIntersection(cross_street0, cross_street1, city, state) :
    place = None
    
    street_address = Template("$street_0, $city $state")
    intersection = Template("$street_0 & $street_1, $city $state")

    if cross_street0 :
        # Sometimes street addresses are written out, particularly
        # 'One' and sometimes 'Two.' Right now we are only catching
        # addresses that start with a sequence of numbers a space and
        # something that is not a number or whitespace (ideally an
        # letter.
        if re.compile('\d+ [\D\W]').match(cross_street0) :
            place = street_address.substitute(street_0 = cross_street0,
                                              city = city,
                                              state = state)
            place = place.lower()
        else :
            if cross_street1 and cross_street1 != cross_street0 :
                # Likely fertile place for improvement
                xstreet_splitter = re.compile('(.+)( and |/)(.+)')
                xstreet_split = xstreet_splitter.findall(cross_street1)
                if xstreet_split :
                    xstreet0 = xstreet_split[0][0].strip()
                    xstreet1 = xstreet_split[0][2].strip()
                else :
                    xstreet0 = cross_street0
                    xstreet1 = cross_street1
                xstreet0 = xstreet0.title()
                xstreet1 = xstreet1.title()
                place = intersection.substitute(street_0 = xstreet0,
                                                street_1 = xstreet1,
                                                city = city,
                                                state = state
                                                )
                place = place.lower()
                
    return place

def parseListing(listing, city, state) :
    cross_street_0, cross_street_1, geo_area = placeFromCLTags(listing)
    place = addressOrIntersection(cross_street_0, 
                                  cross_street_1, 
                                  city, state)

    return place, geo_area

def parseListings(listings, city, state) :
    place_d = defaultdict(lambda : defaultdict(list))
 
    for listing_id, listing in listings :
        place, geo_area = parseListing(listing, city, state)
        if place :
            place_d[place][geo_area].append(listing_id)

    return place_d

def localLookup(cursor, place) :     
        # Geocoding from the Google API is expensive, and a great many of
        # listings are duplicates, so we store geocoding results in local
        # place lookup table
        cursor.execute("""SELECT location_id FROM place_lookup WHERE dirty_location = %s """, (place,))
        location_id = c.fetchone()
        if location_id :
            location_id = location_id[0]

        return location_id

def updateLookup(cursor, location_id, place) :
    c.execute("""INSERT INTO place_lookup """
              """(location_id, dirty_location) VALUES (%s, %s) """, 
              (location_id, place))


    db.commit()

   
def geocode(place) :
    # We want one and only one result from our geocoding call.
    # If we have any other number, we have  geo.geocode() throw
    # an error.
    try :
        georeference = geo.geocode(place, exactly_one=True)
    except Exception as e :
        georeference = None
        if "Didn't find exactly one placemark!" in str(e) :
            logging.debug(e)
        elif "successful but returned no results." in str(e) :
            logging.debug(e)
        else:
            raise

    time.sleep(20)

    logging.debug("  Geocoded")
    logging.debug(georeference)

    return georeference

def deduplicateGeoreference(cursor, georeference) :
    cursor.execute("""SELECT location_id FROM location where canonical_location = %s""", (georeference[0]))
    location_id = c.fetchone()
    if location_id :
        location_id = location_id[0]
    
    return location_id





def updateLocations(cursor, georeference, place) :
    cursor.execute("""REPLACE INTO location """
                   """(canonical_location, lat_long, city) """
                   """VALUES (%s, PointFromText('POINT(%s %s)'), %s)""", 
                   (georeference[0], 
                    georeference[1][0], 
                    georeference[1][1], city))
    location_id = db.insert_id()
    db.commit()
    

    return location_id




db = MySQLdb.connect(db="neighborhood",
                     read_default_file="~/.my.cnf")
db.set_character_set('utf8')

c = db.cursor()
c.execute('SET NAMES utf8;')
c.execute('SET CHARACTER SET utf8;')
c.execute('SET character_set_connection=utf8;')


db = MySQLdb.connect(db="neighborhood",
                     read_default_file="~/.my.cnf")
c = db.cursor()

# Get the listings from rss feeds that are more recent than the
# rss that contained the last location we inserted

num_cities = len(config.cities)

for k, city in enumerate(config.cities) :
    c.execute("""SELECT COUNT(*) FROM label""")
    num_labels_start = c.fetchone()[0]

    logging.info("%(city)s: %(i)d / %(total)d : %(time)s",
                 {'city' : city, 
                  'i' : k, 
                  'total' : num_cities,
                  'time' : time.strftime("%Y-%m-%d %H:%M:%S")})
    city_name, state = config.cities[city]

    c.execute("""SELECT IFNULL(MIN(published),0) """
              """       FROM listing """
              """       WHERE """
              """       listing_id IN (SELECT listing_id """
              """                      FROM label """
              """                      WHERE """
              """                      location_id = (SELECT """
              """                                     IFNULL(MAX(location_id),0) """
              """                                     FROM location """
              """                                     WHERE city = %s))""",
              city)

    min_published = c.fetchone()[0]


    if min_published != 0 :
        logging.debug(min_published)
    else :
        logging.debug("No 'latest' listing can't be found, starting from beginning'")

    c.execute("""SELECT listing_id, content """
              """FROM listing """
              """WHERE """
              """published >= %s """
              """AND city = %s """
              """ORDER BY published""",
              (min_published, city, ))

    listings = c.fetchall()

    places = parseListings(listings, city_name, state)

    geo = geocoders.MapQuest('Fmjtd%7Cluub2q0bnl%2C80%3Do5-9u7xhu')

    for i, place in enumerate(places) :
        logging.debug(" Place: %(i)i: %(place)s", {'i':i, 'place':place})

        location_id = localLookup(c, place)
        if location_id :
            logging.debug("  place found place in db")
        else :
            georeference = geocode(place)
            if georeference is None :
                continue

            location_id = deduplicateGeoreference(c, georeference)

            if location_id :
                logging.debug("  location found in db")
                updateLookup(c, location_id, place)

            else :
                location_id = updateLocations(c, georeference, place)
                updateLookup(c, location_id, place)


        # Now update the label table
        for label in places[place] :
            logging.debug("  label: %(label)s", {"label" :label})
            for listing_id in places[place][label] :
                c.execute(""" REPLACE INTO label """
                          """(location_id, listing_id, label) """
                          """VALUES (%s, %s, %s) """, 
                          (location_id, listing_id, label))
                db.commit()

    c.execute("""SELECT COUNT(*) FROM label""")
    num_labels_end = c.fetchone()[0]
    
    logging.info("%(new labels)d new labels", 
                 {'new labels' : num_labels_end - num_labels_start})

            
c.close()
db.close()    

