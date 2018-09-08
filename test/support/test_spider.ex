defmodule TestSpider do
  @moduledoc """
  A simple spider for testing purpose.

  In order to test various callbacks of GenSpider without defining
  many different modules.

  To start a test spider, call `start_link/2` with a keyword list
  of callbacks to test, in the form of {name, function}.
  """
  alias :gen_spider, as: GenSpider

  @behaviour GenSpider

  @type callback :: {atom(), fun}
  @typep state :: any()

  ## API
  @spec start([callback], [GenSpider.option]) :: :ignore | {:error, any()} | {:ok, pid()}
  def start(callbacks, options \\ []) do
    GenSpider.start(__MODULE__, callbacks, options)
  end

  ## GenSpider callbacks

  @impl true
  @spec init(keyword()) :: {:ok, state}
                          | :ignore
                          | {:stop, reason :: term}
  def init(options \\ []) do
    # Init arguments if any.
    args = Keyword.get(options, :args)
    case maybe_apply(options, :init, [args], {:ok, args}) do
      {:ok, state} ->
        spider = Keyword.put(options, :state, state)
        {:ok, spider}
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
