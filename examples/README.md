# Examples

GenSpider comes with some useful generic spiders that can be found in the
[examples](examples) directory. Their aim is to provide convenient functionality
for a few common scraping cases:

- [ ] [Crawler](examples/crawler.exs) - the most commonly used spider for crawling
  regular websites, as it provides a convenient mechanism for following links by
  defining a set of rules.

- [ ] [SitemapSpider](examples/sitemap_spider.exs) - crawl a site by discovering the
  URLs using Sitemaps. It supports nested sitemaps and discovering sitemap urls
  from robots.txt.

- [ ] [WebScraperIO](examples/webscraper_io.exs) - A spider using "sitemap" from
  WebScraper.IO, which provides a Chrome extension to visually define scraping
  rules. This module provides a spider to use those rules to collect data.
