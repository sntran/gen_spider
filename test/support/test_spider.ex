defmodule TestSpider do
  @moduledoc """
  A simple spider for testing purpose.

  In order to test various callbacks of GenSpider without defining
  many different modules.

  To start a test spider, call `start_link/2` with a keyword list
  of callbacks to test, in the form of {name, function}.
  """

  @behaviour GenSpider

  @type callback :: {atom(), fun}
  @typep state :: any()
  @typep spider :: list()

  ## API
  @spec start([callback], [GenSpider.option()]) :: :ignore | {:error, any()} | {:ok, pid()}
  def start(callbacks \\ [], options \\ []) do
    GenSpider.start(__MODULE__, callbacks, options)
  end

  @spec start_link([callback], [GenSpider.option()]) :: :ignore | {:error, any()} | {:ok, pid()}
  def start_link(callbacks \\ [], options \\ []) do
    GenSpider.start_link(__MODULE__, callbacks, options)
  end

  ## GenSpider callbacks

  @impl true
  @spec init(keyword()) ::
          {:ok, state}
          | :ignore
          | {:stop, reason :: term}
  def init(options \\ []) do
    # Init arguments if any.
    args = Keyword.get(options, :args)

    case maybe_apply(options, :init, [args], {:ok, args}) do
      {:ok, state} ->
        spider = Keyword.put(options, :state, state)
        {:ok, spider}

      {:ok, state, delay} ->
        spider = Keyword.put(options, :state, state)
        {:ok, spider, delay}

      other ->
        other
    end
  end

  @impl true
  @spec start_requests(spider()) :: {:ok, list(), state()}
  def start_requests(spider) do
    state = spider[:state]

    case maybe_apply(spider, :start_requests, [state], {:ok, [], state}) do
      {:ok, fetches, state} ->
        {:ok, fetches, Keyword.put(spider, :state, state)}

      other ->
        other
    end
  end

  @impl true
  @spec parse(any(), spider()) :: {:ok, term(), state()}
  def parse(response, spider) do
    state = spider[:state]

    case maybe_apply(spider, :parse, [response, spider], {:ok, response, state}) do
      {:ok, response, state} ->
        {:ok, response, Keyword.put(spider, :state, state)}

      other ->
        other
    end
  end

  defp maybe_apply(spider, fun, args, default_reply) do
    case Keyword.get(spider, fun) do
      nil ->
        default_reply

      callback when is_function(callback) ->
        apply(callback, args)
    end
  end
end
