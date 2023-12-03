# https://github.com/starr-dusT/dotfiles
# Info: https://github.com/miniflux/python-client
import miniflux

try:
    client = miniflux.Client("<miniflux url>", "<username>", "<password>")
except Exception as e: print(e)
print("client connected")

# Example: Make substitution of feed url for Youtube feeds
feeds = client.get_feeds()
print(f"got {len(feeds)} feeds")
for feed in feeds:
   id = feed['id']
   cat = feed['category']['title']
   if cat == "Youtube":
       old_feed_url = feed['feed_url']
       client.update_feed(feed_id=id, feed_url=old_feed_url.replace("CustomYoutubeBridge","YoutubeBridge"))
