import sqlite3
import requests
import os, sys, time

github_token = os.environ['GITHUB_TOKEN']


def get(owner_login):
    time.sleep(0.72)  # throttle requests to ~5000 per hour
    res = requests.get(
        f'https://api.github.com/users/{owner_login}',
        headers={'Authorization': f'token {github_token}'}
    )
    if res.status_code == 403:
        return handle_rate_limit_error(res)
    else:
        #res.raise_for_status()
        return res


def handle_rate_limit_error(res):
    t = res.headers.get('X-RateLimit-Reset')
    if t is not None:
        t = max(0, int(int(t) - time.time()))
    else:
        t = int(res.headers.get('Retry-After', 60))
    print(f'Exceeded rate limit. Retrying after {t} seconds...')
    time.sleep(t)
    return get(res.url)


db = sqlite3.connect('results.db')
db.executescript('''
    CREATE TABLE IF NOT EXISTS owner 
    ( owner_id INTEGER PRIMARY KEY
    , owner_login TEXT NOT NULL
    , created_at TEXT NOT NULL
    , bio TEXT
    , blog TEXT
    , company TEXT
    , email TEXT
    , location TEXT
    , name TEXT
    , twitter_username TEXT
    );
''')

db.executescript('''
  CREATE TABLE IF NOT EXISTS owner_missing
  ( owner_id INTEGER PRIMARY KEY
  , owner_login TEXT NOT NULL
  , status_code INTEGER
  );
''')

total = db.execute('''
  select count(distinct owner_id) from repo 
  where owner_id not in (select owner_id from owner) 
  and owner_id not in (select owner_id from owner_missing)
''').fetchone()[0]
print(f'scraping public user data for {total} github users...')

print()
i = 0
errors = 0
rows = db.execute('''
  select distinct owner_id, owner_login from repo 
  where owner_id not in (select owner_id from owner) 
  and owner_id not in (select owner_id from owner_missing)
''')
for row in rows:
    i = i + 1
    owner_id = row[0]    
    owner_login = row[1]
    sys.stdout.write('\033[F\r\033[J') # overwrite current line
    print(f'{i}/{total} ({round(i*100.0/total,2)}%), {errors} errors, currently fetching: {owner_login}')
    res = get(owner_login)
    if res.status_code != 200:
      errors = errors + 1
      db.execute(
        'INSERT OR IGNORE INTO owner_missing (owner_id, owner_login, status_code) VALUES (?,?,?)',
        (owner_id, owner_login, res.status_code)
      )
      db.commit()
      continue
    user = res.json()
    db.execute('''
      INSERT OR IGNORE INTO owner
      ( owner_id, owner_login, created_at
      , bio, blog, company, email, location, name, twitter_username
      ) VALUES (?,?,?,?,?,?,?,?,?,?)
    ''',
    ( owner_id, owner_login, user['created_at'],
      user['bio'], user['blog'], user['company'], user['email'], 
      user['location'], user['name'], user['twitter_username']
    ))
    db.commit()

sys.stdout.write('\033[F\r\033[J') # overwrite current line
print(f'done. encountered {errors} errors.')
