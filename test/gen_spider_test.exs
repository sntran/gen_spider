defmodule GenSpiderTest do
  use GenSpider.TestCase, async: true
  doctest GenSpider

  describe "@callback init/1" do
    test "is called when started" do
      TestSpider.start([
        init: fn(_args) ->
          Kernel.send(:tester, {:called_back, :init, 1})
        end
      ])
      assert_received {:called_back, :init, 1}, "@callback init/1 was not called"
    end

    test "should have args passed through" do
      callbacks = [
        init: fn(args) ->
          Kernel.send(:tester, {:called_back, :init, args})
        end
      ]
      TestSpider.start(callbacks)
      assert_received {:called_back, :init, callbacks}, "@callback init/1 did not receive args"
    end

    test "must return initial state" do
      assert {:error, _Reason} = TestSpider.start([
        init: fn(_args) -> :ok end
      ])
      assert {:ok, pid} = TestSpider.start([
        init: fn(_args) -> {:ok, 1} end
      ])
    end

    test "can ignore initalization" do
      assert :ignore = TestSpider.start([
        init: fn(_args) -> :ignore end
      ])
    end

    test "can stop with error" do
      reason = "test"
      assert {:error, ^reason} = TestSpider.start([
        init: fn(_args) -> {:stop, reason} end
      ])
    end
  end
end
