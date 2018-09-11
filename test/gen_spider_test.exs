defmodule GenSpiderTest do
  use GenSpider.TestCase, async: true
  doctest GenSpider

  setup_all do
    {:ok, _} = Registry.start_link(keys: :unique, name: Registry.GenSpiderTest)

    start_supervised({DynamicSupervisor, strategy: :one_for_one, name: GenSpider.TestSupervisor})

    :ok
  end

  describe "start/3" do
    test "spawns a process" do
      {:ok, pid} = GenSpider.start(TestSpider, [], [])
      assert is_pid(pid)
    end

    test "can be optionally named locally with an atom" do
      {:ok, pid} = GenSpider.start(TestSpider, [], name: TestSpider)

      assert Process.whereis(TestSpider) === pid
    end

    test "can be optionally named globally with an atom" do
      {:ok, pid} = GenSpider.start(TestSpider, [], name: {:global, TestSpider})

      assert :global.whereis_name(TestSpider) === pid
    end

    test "can be optionally named via another module" do
      name = {:via, Registry, {Registry.GenSpiderTest, "TestSpider"}}
      {:ok, pid} = GenSpider.start(TestSpider, [], name: name)

      assert Registry.whereis_name({Registry.GenSpiderTest, "TestSpider"}) === pid
    end
  end

  describe "stop/1" do
    test "stops the spider by pid" do
      {:ok, pid} = GenSpider.start(TestSpider, [], [])
      assert Process.alive?(pid)
      GenSpider.stop(pid)
      refute Process.alive?(pid)
    end

    test "stops the spider by local atom name" do
      {:ok, pid} = GenSpider.start(TestSpider, [], name: StoppableTestSpider)

      assert Process.alive?(pid)
      GenSpider.stop(StoppableTestSpider)
      refute Process.alive?(pid)
    end

    test "stops a global spider" do
      name = {:global, GlobalTestSpider}
      {:ok, pid} = GenSpider.start(TestSpider, [], name: name)

      assert Process.alive?(pid)
      GenSpider.stop(name)
      refute Process.alive?(pid)
    end

    test "stops a spider registered via another module" do
      name = {:via, Registry, {Registry.GenSpiderTest, "BinaryNamedTestSpider"}}
      {:ok, pid} = GenSpider.start(TestSpider, [], name: name)

      assert Process.alive?(pid)
      GenSpider.stop(name)
      refute Process.alive?(pid)
    end
  end

  describe "start_link/3" do
    test "can be used in a supervision tree" do
      spec = {GenSpider, [TestSpider, [], []]}

      {:ok, spider} = DynamicSupervisor.start_child(GenSpider.TestSupervisor, spec)
      assert is_pid(spider)
    end
  end

  describe "@callback init/1" do
    test "is called when started" do
      TestSpider.start(
        init: fn _args ->
          Kernel.send(:tester, {:called_back, :init, 1})
        end
      )

      assert_received {:called_back, :init, 1}, "@callback init/1 was not called"
    end

    test "should have args passed through" do
      callbacks = [
        args: 1,
        init: fn args ->
          Kernel.send(:tester, {:called_back, :init, args})
        end
      ]

      TestSpider.start(callbacks)
      assert_received {:called_back, :init, 1}, "@callback init/1 did not receive args"
    end

    test "must return initial state" do
      assert {:error, _reason} = TestSpider.start(init: fn _args -> :ok end)
      assert {:ok, _pid} = TestSpider.start(init: fn _args -> {:ok, 1} end)
    end

    test "can ignore initalization" do
      assert :ignore = TestSpider.start(init: fn _args -> :ignore end)
    end

    test "can stop with error" do
      reason = "test"
      assert {:error, ^reason} = TestSpider.start(init: fn _args -> {:stop, reason} end)
    end
  end

  describe "@callback start_request/1" do
    setup do
      Process.flag(:trap_exit, true)

      :ok
    end

    test "is called when URLs are not provided in option for `start/3`" do
      TestSpider.start(
        start_requests: fn _state ->
          Kernel.send(:tester, {:called_back, :start_requests, 1})
        end
      )

      assert_received {:called_back, :start_requests, 1},
                      "@callback start_requests/1 was not called"
    end

    test "is not called when URLs are provided in option for `start/3`" do
      TestSpider.start(
        [
          start_requests: fn _state ->
            Kernel.send(:tester, {:called_back, :start_requests, 1})
          end
        ],
        start_urls: ["http://www.example.com"]
      )

      refute_receive {:called_back, :start_requests, 1},
                     100,
                     "@callback start_requests/1 was not supposed to be called"
    end

    test "will be delayed if specified in the return of @callback init/1" do
      TestSpider.start(
        init: fn args ->
          {:ok, args, 100}
        end,
        start_requests: fn _state ->
          Kernel.send(:tester, {:called_back, :start_requests, 1})
        end
      )

      refute_receive {:called_back, :start_requests, 1},
                     100,
                     "@callback start_requests/1 was not supposed to be called before the delay"

      assert_receive {:called_back, :start_requests, 1},
                     101,
                     "@callback start_requests/1 was not called after delay"
    end

    test "will be delayed if specified in option for `start/3`" do
      TestSpider.start(
        [
          start_requests: fn _state ->
            Kernel.send(:tester, {:called_back, :start_requests, 1})
          end
        ],
        delay: 100
      )

      refute_receive {:called_back, :start_requests, 1},
                     100,
                     "@callback start_requests/1 was not supposed to be called before the delay"

      assert_receive {:called_back, :start_requests, 1},
                     101,
                     "@callback start_requests/1 was not called after delay"
    end

    test "will be delayed by value in the return of @callback init/1 rather than delay option" do
      TestSpider.start(
        [
          init: fn args ->
            {:ok, args, 100}
          end,
          start_requests: fn _state ->
            Kernel.send(:tester, {:called_back, :start_requests, 1})
          end
        ],
        delay: 500
      )

      refute_receive {:called_back, :start_requests, 1},
                     100,
                     "@callback start_requests/1 was not supposed to be called before the delay"

      assert_receive {:called_back, :start_requests, 1},
                     101,
                     "@callback start_requests/1 was not called after delay"
    end

    test "must return list of Requests" do
      {:ok, pid1} = TestSpider.start_link(start_requests: fn _state -> :ok end)
      assert_receive {:EXIT, ^pid1, :invalid_return}

      {:ok, pid2} =
        TestSpider.start(
          start_requests: fn state ->
            {:ok, [], state}
          end
        )

      refute_receive {:EXIT, ^pid2, :invalid_return}
    end

    test "is optional even when there is no `start_urls`" do
      {:ok, pid} =
        TestSpider.start(
          init: fn args ->
            {:ok, args, 100}
          end
        )

      refute_receive {:EXIT, ^pid, :invalid_return}
    end
  end
end
