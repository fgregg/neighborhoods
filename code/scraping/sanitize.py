import MySQLdb
from BeautifulSoup import BeautifulSoup
import re

STOP_WORDS = set([r'and', r'to', r'in', r'a', r'the', r'of',
                  r'for', r'with', r'is'])

def extractPrice(title) :
    price = None
    bedrooms = None

    price_re = re.search("(^| )\$(\d+)( |$)", title)
    bedrooms_re = re.search("(^| )(\d)bd( |$)", title)

    if price_re :
        price = int(price_re.group(2))
    if bedrooms_re :
        bedrooms = int(bedrooms_re.group(2))

    return price, bedrooms

def monthYear(published_date) :
    return published_date.replace(day = 1, hour=0, minute=0, second=0)

def countWords(c) :
    c.execute("SELECT title, content FROM listing")

    listings = c.fetchall()
    for title, content in listings :
        title = title + r' '
        all_content = BeautifulSoup(title + content).getText()
        all_content = all_content.replace('\n', ' ')
        all_content = re.sub(r'START CLTAGS.*END CLTAGS', '', all_content)
        tokens = set(token.lower() for token in all_content.split())
        tokens -= STOP_WORDS
        for word in tokens  :
            c.execute("INSERT INTO words (word, counter) "
                      "VALUES (%s, 1) "
                      "ON DUPLICATE KEY UPDATE counter = counter + 1", (word.lower(),))



def baggify(content, c) :

    content_text = BeautifulSoup(content).getText()
    content_text = re.sub(r'START CLTAGS.*END CLTAGS', '', content_text)
    tokens = [token.lower() for token in content_text.split()]

    content_bag = []
    for token in tokens :
        if token in STOP_WORDS :
            continue
        c.execute("SELECT counter FROM words where word = %s", (token,))
        count = c.fetchone()
        if count and count[0] > 10 :
            content_bag.append(token)

    return r' '.join(content_bag)
            
            
        
def sanitizeListings(c) :
    c.execute("SELECT listing_id, title, content, rss_id, city, published "
              "FROM listing")
    
    listings = c.fetchall() 

    for listing_id, title, content, rss_id, city, published in listings :
        bagged_title = baggify(title, c)
        bagged_content = baggify(content, c)
        month_year = monthYear(published)
        price, bedrooms = extractPrice(title)
        c.execute("INSERT IGNORE INTO sanitized_listing VALUES "
                  "(%s, %s, %s, %s, %s, %s, ROUND(%s, -2), %s)",
                  (listing_id, month_year, bagged_title, bagged_content, 
                   rss_id, city, price, bedrooms))
                   

        

if __name__ == '__main__' :
    db = MySQLdb.connect(db="neighborhood",
                         read_default_file="~/.my.cnf")
    db.set_character_set('utf8')

    c = db.cursor()
    c.execute('SET NAMES utf8;')
    c.execute('SET CHARACTER SET utf8;')
    c.execute('SET character_set_connection=utf8;')

    #countWords(c)
    sanitizeListings(c)
    #c.execute("DELETE FROM listings")
    db.commit()
    db.close()
