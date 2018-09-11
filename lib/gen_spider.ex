defmodule GenSpider do
  @moduledoc "README.md"
             |> File.read!()
             |> String.split("<!-- MDOC !-->")
             |> Enum.fetch!(1)

  require Logger

  @typedoc "Options used by the `start*` functions"
  @type options :: [option]

  @type option :: :gen_spider.option()

  @typep state :: any

  defdelegate start(module, args, options), to: :gen_spider

  @doc """
  Starts a `GenSpider` process linked to the current process.

  This is often used to start the `GenSpider` as part of a supervision
  tree.

  Once the spider is started, it calls the `init/1` function in the
  given `module` passing the given `args` to initialize it. To ensure
  a synchronized start-up procedure, this function does not return
  until `init/1` has returned.

  Note that a `GenSpider` started with `start_link/3` is linked to the
  parent process and will exit in case of crashes. The GenSpider will
  also exit due to the `:normal` reasons in case it is configured to
  trap exits in the `init/1` callback.
  """
  defdelegate start_link(module, args, options), to: :gen_spider

  defdelegate stop(spider), to: :gen_spider

  # Define the callbacks for `GenSpider`
  @callback init(any) ::
              {:ok, state}
              | {:ok, state, timeout | :hibernate}
              | :ignore
              | {:stop, reason :: term}

  @callback start_requests(state) :: {:ok, list, state}

  @doc """
  Elixir-specific child specification for a spider to be supervised.
  """
  @spec child_spec([atom() | term() | options()]) :: Supervisor.child_spec()
  def child_spec([module, args, options]) when is_atom(module) and is_list(options) do
    %{
      id: module,
      start: {__MODULE__, :start_link, [module, args, options]},
      restart: :transient,
      shutdown: 5000,
      type: :worker
    }
  end
end
