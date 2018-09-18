# File: quotes_spider.ex
defmodule QuotesSpider do
  @behaviour GenSpider

  def start_link() do
    GenSpider.start_link(__MODULE__, [], start_urls: ["http://quotes.toscrape.com/page/1/"])
  end

  @impl true
  def init(_) do
    {:ok, []}
  end

  @impl true
  def parse(%{:status => 200, :headers => _headers, :body => body}, state) do
    IO.inspect(body)
    {:ok, [body] ++ state}
  end
end
