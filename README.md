# GenSpider

[![Build Status](https://img.shields.io/travis/sntran/gen_spider/master.svg)](https://travis-ci.org/sntran/gen_spider)
[![Test Coverage](https://img.shields.io/coveralls/github/sntran/gen_spider.svg)](https://coveralls.io/github/sntran/gen_spider)
[![Hex Version](https://img.shields.io/hexpm/v/gen_spider.svg)](https://hex.pm/packages/gen_spider)
[![License](https://img.shields.io/github/license/sntran/gen_spider.svg)](https://choosealicense.com/licenses/apache-2.0/)

<!-- MDOC !-->

GenSpider is a behaviour for defining Spiders.

Spiders are modules which define how a certain site (or a group of sites) will
be scraped, including how to perform the crawl (i.e. follow links) and how to
extract structured data from their pages (i.e. scraping items). In other words,
Spiders are the place where you define the custom behaviour for crawling and
parsing pages for a particular site (or, in some cases, a group of sites).

## Hello World

The basic Quotes Spider from Scrapy is implemented with `gen_spider` in both
[Erlang](examples/quotes_spider.erl) and [Elixir](examples/quotes_spider.ex).

<!-- MDOC !-->

## Generic Spiders

GenSpider also comes with some useful generic spiders that can be found in the
[examples](examples) directory. Their aim is to provide convenient functionality
for a few common scraping cases, like following all links on a site based on
certain rules, crawling from Sitemaps, or parsing an XML/CSV feed.

<!-- MDOC !-->

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `gen_spider` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:gen_spider, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/gen_spider](https://hexdocs.pm/gen_spider).

## Contributing

We welcome everyone to contribute to GenSpider and help us tackle existing issues!

Use the [issue tracker][issues] for bug reports or feature requests. Open a [pull request][pulls] when you are ready to contribute.

When submitting a pull request you should not update the `CHANGELOG.md`.

## License

GenSpider source code is released under Apache 2 License.
Check LICENSE file for more information.

[issues]: https://github.com/sntran/gen_spider/issues
[pulls]: https://github.com/sntran/gen_spider/pulls
