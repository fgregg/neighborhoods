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

import SECRET

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

def placeFromMap(listing) :
    soup = BeautifulSoup(listing)

    geo_location_div = soup.find('div', {'id' : 'map'})

    if geo_location_div :
        latitude = geo_location_div['data-latitude']
        longitude = geo_location_div['data-longitude']
        geolocation = (float(latitude), float(longitude))
    else :
        geolocation = None

    address_div = soup.find('div', {'class' : 'mapaddress'})

    if address_div :
        address = address_div.text
    else :
        address = None

    posting_title_div = soup.find('h2', {'class' : 'postingtitle'})
    
    if posting_title_div :
        match = re.search(r'.*\((.+?)\)$', posting_title_div.text.strip())
        if match :
            end_label = match.group(1)
        else :
            end_label = None
    else :
        end_label = None

    return address, geolocation, end_label
    

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

def addressOrIntersection(cross_street0, cross_street1) :
    place = None
    
    street_address = Template("$street_0")
    intersection = Template("$street_0 & $street_1")

    if cross_street0 :
        # Sometimes street addresses are written out, particularly
        # 'One' and sometimes 'Two.' Right now we are only catching
        # addresses that start with a sequence of numbers a space and
        # something that is not a number or whitespace (ideally an
        # letter.
        if re.compile('\d+ [\D\W]').match(cross_street0) :
            place = street_address.substitute(street_0 = cross_street0)
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
                                                street_1 = xstreet1)
                
    return place

def parseListing(listing) :
    cross_street_0, cross_street_1, geo_area = placeFromCLTags(listing)
    place = addressOrIntersection(cross_street_0, 
                                  cross_street_1) 

    if place is None :
        place, geolocation, end_label = placeFromMap(listing)
        if geo_area is None :
            geo_area = end_label
    else :
        geolocation = None

    return place, geolocation, geo_area


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

def deduplicateGeoreference(cursor, place) :
    cursor.execute("""SELECT location_id FROM location where canonical_location = %s""", (place))
    location_id = c.fetchone()
    if location_id :
        location_id = location_id[0]
    
    return location_id


def updateLocations(cursor, place, geolocation) :
    cursor.execute("""REPLACE INTO location """
                   """(canonical_location, lat_long) """
                   """VALUES (%s, PointFromText('POINT(%s %s)'))""", 
                   (place, 
                    geolocation[0], 
                    geolocation[1]))
    location_id = db.insert_id()
    db.commit()
    

    return location_id

def last_published(c, city) :
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

    return c.fetchone()[0]

def new_listings(c, min_published, city) :
    c.execute("""SELECT listing_id, content """
              """FROM listing """
              """WHERE """
              """published >= %s """
              """AND city = %s """
              """ORDER BY published""",
              (min_published, city, ))

    return c.fetchall()

def lookupLocation(c, place, geolocation) :
    location_id = None

    if place :
        location_id = localLookup(c, place)

        if location_id is None :
            location_id = deduplicateGeoreference(c, place)

        if location_id is None and geolocation is None :
            georeference = geocode(place)
            if georeference :
                normalized_place, geolocation = georeference
                
                location_id = lookupLocation(c, 
                                             normalized_place, 
                                             geolocation)

        if location_id is None and geolocation :
            location_id = updateLocations(c,
                                          place,
                                          geolocation)

    elif geolocation :
        location_id = updateLocations(c,
                                      '',
                                      geolocation)

    
    return location_id 

            
    


def label_count(c) :
    c.execute("""SELECT COUNT(*) FROM label""")
    return c.fetchone()[0]


if __name__ == '__main__' :
    geo = geocoders.GeocodeFarm(SECRET.API_KEY)

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
        logging.info("%(city)s: %(i)d / %(total)d : %(time)s",
                     {'city' : city, 
                      'i' : k + 1, 
                      'total' : num_cities,
                      'time' : time.strftime("%Y-%m-%d %H:%M:%S")})

        city_name, state = config.cities[city]

        num_labels_start = label_count(c)

        last_listing = last_published(c, city)

        if last_listing != 0 :
            logging.debug(last_listing)
        else :
            logging.debug("No 'latest' listing can't be found, starting from beginning'")

        listings = new_listings(c, last_listing, city)

        i = 0
        for listing_id, listing in listings :
            try :
                place, geolocation, label = parseListing(listing)
            except :
                print listing
                print listing_id
                raise

            if not place and not geolocation :
                continue

            i += 1
            
            if place :
                logging.debug(" Place: %(i)i: %(place)s", {'i':i, 'place':place})
                place = "%s, %s %s" % (place.lower().strip(), city, state)

            else :
                logging.debug(" Place: %(i)i: No address", {'i':i})

            location_id = lookupLocation(c, place, geolocation)
            
            if location_id and label :
                logging.debug("  label: %(label)s", {"label" :label})

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

