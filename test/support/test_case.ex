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
end
