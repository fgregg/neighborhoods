max_packet_size = 1048576 # Set in my.conf
byte_encoding = 5 # UTF-8 Uses up to four bytes
string_chunk = int(max_packet_size/byte_encoding)

cities = {"newyork" : ('New York', 'NY'),
          "losangeles" : ('Los Angeles', 'CA'),
          "chicago" : ('Chicago', 'IL'),
          "houston" : ('Houston', 'TX'),
          "philadelphia" : ('Philadelphia', 'PA'),
          "phoenix" : ('Phoenix', 'AZ'),
          "sanantonio" : ('San Antonio', 'TX'),
          "sandiego" : ('San Diego', 'CA'),
          "dallas" : ('Dallas', 'TX'),
          "jacksonville" : ('Jacksonville', 'FL'),
          "indianapolis" : ('Indianapolis', 'IN'),
          "sanfrancisco" : ('San Francisco', 'CA'),
          "austin" : ('Austin', 'TX'),
          "columbus" : ('Columbus', 'OH'),
          "charlotte" : ('Charlotte', 'NC'),
          "detroit" : ('Detroit', 'MI'),
          "elpaso" : ('El Paso', 'TX'),
          "memphis" : ('Memphis', 'TN'),
          "baltimore" : ('Baltimore', 'MD'),
          "boston" : ('Boston', 'MA'),
          "seattle" : ('Seattle', 'WA'),
          "dc" : ('Washington', 'DC'),
          "nashville" : ('Nashville', 'TN'),
          "denver" : ('Denver', 'CO'),
          "louisville" : ('Louisville', 'KY'),
          "milwaukee" : ('Milwaukee', 'WI'),
          "portland" : ('Portland', 'OR'),
          "lasvegas" : ('Las Vegas', 'NV'),
          "oklahomacity" : ('Oklahoma City', 'OK'),
          "albuquerque" : ('Albuquerque', 'NM'),
          "tucson" : ('Tucson', 'AZ'),
          "fresno": ('Fresno', 'CA'),
          "sacramento" : ('Sacremento', 'CA'),
          "kansascity" : ('Kansas City', 'MO'),
          "atlanta" : ('Atlanta', 'GA'),
          "cosprings" : ('Colorado Springs', 'CO'),
          "omaha" : ('Omaha', 'NE'),
          "raleigh" : ('Raleigh', 'NC'),
          "miami" : ('Miami', 'FL'),
          "cleveland" : ('Cleveland', 'OH'),
          "tulsa" : ('Tulsa', 'OK'),
          "minneapolis" : ('Minneapolis', 'MN'),
          "wichita" : ('Wichita', 'KS'),
          "knoxville" : ('Knoxville', 'TN'),
          "asheville" : ('Asheville', 'NC')
          }

std_feeds = [["sublet", "http://%s.craigslist.org/sub/index.rss"],
             ["room", "http://%s.craigslist.org/roo/index.rss"],
             ["apartment" , "http://%s.craigslist.org/apa/index.rss"]
             ]

ny_feeds =  [["sublet", "http://%s.craigslist.org/sub/index.rss"],
             ["room", "http://%s.craigslist.org/roo/index.rss"],
             ["apartment" , "http://%s.craigslist.org/abo/index.rss"]
             ]
