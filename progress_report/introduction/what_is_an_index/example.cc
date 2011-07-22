typedef Index<Url, Page> Webcache;
Webcache cache;

Page get(Url u) {
  Page p = cache.search(u);

  if (p != null && !p.expired()) return p;

  p = http_get(u);

  if (p == null) cache.remove(u);
  else           cache.insert(u, p);

  return p;
}
