defmodule GenSpider do
  @moduledoc "README.md"
              |> File.read!()
              |> String.split("<!-- MDOC !-->")
              |> Enum.fetch!(1)

  require Logger

  @typedoc "Options used by the `start*` functions"
  @type options :: [options]

  @type option :: {:name, GenServer.name}

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

  # Define the callbacks for `GenSpider`
  @callback init(any) ::
    {:ok, state} | {:ok, state, timeout | :hibernate} |
    :ignore | {:stop, reason :: term}
end
