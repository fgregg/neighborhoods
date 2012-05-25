import MySQLdb
import re

db = MySQLdb.connect(db="neighborhood",
                     read_default_file="~/.my.cnf")
c = db.cursor()

c.execute(""" SELECT listing_id, title FROM listing """)

listings = c.fetchall()

for listing_id, title in listings :
    price = re.search("(^| )\$(\d+)( |$)", title)
    bedrooms = re.search("(^| )(\d)bd( |$)", title)
    if price and bedrooms :
        print "(%s, %s, %s)" % (listing_id,
                                 int(price.group(2)),
                                 int(bedrooms.group(2))
                                 )
        
        c.execute(""" INSERT INTO price VALUES (%s, %s, %s) """,
                  (listing_id,
                   int(price.group(2)),
                   int(bedrooms.group(2))
                   )
                  )

        

db.close()

    
        
    
