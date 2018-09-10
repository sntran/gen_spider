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
      assert {:error, _reason} = TestSpider.start([
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

  describe "@callback start_request/1" do
    setup do
      Process.flag(:trap_exit, true)

      :ok
    end

    test "is called when URLs are not provided in option for `start/3`" do
      TestSpider.start([
        start_requests: fn(_state) ->
          Kernel.send(:tester, {:called_back, :start_requests, 1})
        end
      ])

      assert_received {:called_back, :start_requests, 1}, "@callback start_requests/1 was not called"
    end

    test "is not called when URLs are provided in option for `start/3`" do
      TestSpider.start([
        start_requests: fn(_state) ->
          Kernel.send(:tester, {:called_back, :start_requests, 1})
        end
      ], [start_urls: ["http://www.example.com"]])

      refute_receive {:called_back, :start_requests, 1}, 100, "@callback start_requests/1 was not supposed to be called"
    end

    test "will be delayed if specified in the return of @callback init/1" do
      TestSpider.start([
        init: fn(args) ->
          {:ok, args, 100}
        end,
        start_requests: fn(_state) ->
          Kernel.send(:tester, {:called_back, :start_requests, 1})
        end
      ])

      refute_receive {:called_back, :start_requests, 1}, 100, "@callback start_requests/1 was not supposed to be called before the delay"
      assert_receive {:called_back, :start_requests, 1}, 101, "@callback start_requests/1 was not called after delay"
    end

    test "must return list of Requests" do
      {:ok, pid1} = TestSpider.start_link([
        start_requests: fn(_state) -> :ok end
      ])
      assert_receive {:EXIT, ^pid1, :invalid_return}

      {:ok, pid2} = TestSpider.start([
        start_requests: fn(state) ->
          {:ok, [], state}
        end
      ])
      refute_receive {:EXIT, ^pid2, :invalid_return}
    end
  end
end
