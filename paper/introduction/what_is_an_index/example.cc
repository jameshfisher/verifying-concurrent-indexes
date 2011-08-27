Index<Url, Page> cache;  // `cache` stores webpages previously accessed.

Page get(Url u) {  // return the webpage at the URL `u`.
  Page p = cache.search(u);  // First look in the cache ...

  if (p != null && !p.expired())
    // if the cache has an entry for this URL and it has not expired,
    return p;  // just return that.

  p = http_get(u);  // Otherwise, fetch the page from the Web.

  if (p == null) cache.remove(u);  // If the page doesn't exist, remove our records of it,
  else           cache.insert(u, p);  // otherwise cache it for future requests.

  return p;
}
