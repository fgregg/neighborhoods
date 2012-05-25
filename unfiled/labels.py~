import MySQLdb
from geopy import geocoders
from geopy.geocoders.google import GQueryError
from string import Template
from BeautifulSoup import BeautifulSoup, Comment
import re
import time

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
c.execute(""" SELECT listing_id, content FROM listing WHERE rss_id > (SELECT rss_id FROM listing WHERE listing_id = (SELECT listing_id FROM label WHERE location_id = (SELECT MAX(location_id) FROM location) limit 1)) """)

listings = c.fetchall()


def placeFromCLTags(listing) :
    # Craigslists listings have very meta data tags that
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

def addressOrIntersection(location) :
    place = None
    
    street_address = Template("$street_0, $city $state")
    intersection = Template("$street_0 & $street_1, $city $state")

    if location['xstreet0'] :
        # Sometimes street addresses are written out, particularly
        # 'One' and sometimes 'Two.' Right now we are only catching
        # addresses that start with a sequence of numbers a space and
        # something that is not a number or whitespace (ideally an
        # letter.
        if re.compile('\d+ [\D\W]').match(location['xstreet0']) :
            place = street_address.substitute(street_0 = location['xstreet0'],
                                              city = "Chicago",
                                              state = "IL")
        else :
            if location['xstreet1'] and location['xstreet1'] != location['xstreet0'] :
                # Likely fertile place for improvement
                xstreet_splitter = re.compile('(.+)( and |/)(.+)')
                xstreet_split = xstreet_splitter.findall(location['xstreet1'])
                if xstreet_split :
                    xstreet0 = xstreet_split[0][0].strip()
                    xstreet1 = xstreet_split[0][2].strip()
                else :
                    xstreet0 = location['xstreet0']
                    xstreet1 = location['xstreet1']
                xstreet0 = xstreet0.title()
                xstreet1 = xstreet1.title()
                place = intersection.substitute(street_0 = xstreet0,
                                                street_1 = xstreet1,
                                                city = "Chicago",
                                                state = "IL"
                                                )
    return place


locations = []
for listing_id, listing in listings :
    location = {'listing.id' : listing_id,
                'xstreet0' : '', 
                'xstreet1' : '',
                'geographic_area'   : ''}
    [location['xstreet0'],
     location['xstreet1'],
     location['geographic_area']] = placeFromCLTags(listing)
    if location['xstreet0'] or location['xstreet1'] :
        locations.append(location)

places = {}
for location in locations :
    place = addressOrIntersection(location)
    if place :
        place = place.lower()
        if not place in places :
            places[place] = { location['geographic_area'] :
                              [location['listing.id']] }
        else :
            if not location['geographic_area'] in places[place] :
                places[place][location['geographic_area']] = [location['listing.id']]
            else :
                places[place][location['geographic_area']].append(location['listing.id'])

geo = geocoders.Google()

for place in places :
    # Geocoding from the Google API is expensive, and a great many of
    # listings are duplicates, so we store geocoding results in local
    # place lookup table
    c.execute(""" SELECT location_id FROM place_lookup WHERE dirty_location = %s """, (place,))
    location_id = c.fetchone()
    if location_id :
        location_id = location_id[0]
    else :
        # We want one and only one result from our geocoding call.
        # If we have any other number, we throw geo.geocode() throws
        # an error.
        try :
            time.sleep(2)
            georeference = geo.geocode(place, exactly_one=True)
            c.execute(""" SELECT location_id FROM location where canonical_location = %s""", (georeference[0]))
            location_id = c.fetchone()
            if location_id :
                location_id = location_id[0]
            else :
                # If and only if the place is referring to a new
                # location, add the location to our location table
                c.execute(""" REPLACE INTO location (canonical_location, lat_long) VALUES (%s, PointFromText('POINT(%s %s)')) """, (georeference[0], georeference[1][0], georeference[1][1]))
                location_id = db.insert_id()
            c.execute(""" REPLACE INTO place_lookup (location_id, dirty_location) VALUES (%s, %s) """, (location_id, place))
        except (ValueError, GQueryError) :
            location_id = None
    # Now update the label table
    if location_id :
        for label in places[place] :
            for listing_id in places[place][label] :
                c.execute(""" REPLACE INTO label (location_id, listing_id, label) VALUES (%s, %s, %s) """, (location_id, listing_id, label))
            

                                                                                


c.close()
db.close()    

