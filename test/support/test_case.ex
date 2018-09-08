defmodule GenSpider.TestCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      import GenSpider.TestCase
    end
  end

  setup tags do
    # So the callbacks can send message to this test process.
    Process.register(self(), :tester)

    unless tags[:async] do

    end

    :ok
  end

  # Quick macro to generate a spider module that implements :gen_spider
  defmacro gen_spider(module, callbacks) do
    unless Keyword.keyword?(callbacks) do
      raise ArgumentError, "second argument to `spider` must be a compile time keyword list"
    end

    callbacks = Macro.escape(callbacks)

    quote bind_quoted: [module: module, callbacks: callbacks] do
      defmodule module do
        @behaviour :gen_spider

        callbacks = Keyword.put_new(callbacks, :init, quote do
          fn(args) -> {:ok, args} end
        end)

        for {name, {:fn, _, [{:->, _, [args, body]}]}} <- callbacks do
          @impl true
          def unquote(name)(unquote_splicing(args)) do
            unquote(body)
          end
        end
      end
    end
  end
end
