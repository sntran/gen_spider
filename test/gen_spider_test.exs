defmodule GenSpiderTest do
  use GenSpider.TestCase, async: true
  doctest GenSpider

  setup_all do
    start_supervised({Registry, keys: :unique, name: Registry.GenSpiderTest})
    start_supervised({DynamicSupervisor, strategy: :one_for_one, name: GenSpider.TestSupervisor})

    # A simple HTTP Server for testing.
    :inets.start()

    {:ok, httpd} =
      :inets.start(:httpd,
        port: 0,
        server_name: 'localhost',
        server_root: 'test/fixtures',
        document_root: 'test/fixtures',
        bind_address: 'localhost',
        directory_index: ['index.html']
      )

    [port: port] = :httpd.info(httpd, [:port])

    {
      :ok,
      [
        start_urls: [
          "http://localhost:#{port}",
          "http://localhost:#{port}/about"
        ]
      ]
    }
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

    test "is called if provided" do
      TestSpider.start_link(
        start_requests: fn _state ->
          Kernel.send(:tester, {:called_back, :start_requests, 1})
        end
      )

      assert_received {:called_back, :start_requests, 1},
                      "@callback start_requests/1 was not called"
    end

    test "will be delayed if specified in the return of @callback init/1" do
      TestSpider.start_link(
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
      TestSpider.start_link(
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
      TestSpider.start_link(
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
      assert_receive {:EXIT, ^pid1, {:shutdown, :invalid_return}}

      {:ok, pid2} =
        TestSpider.start_link(
          start_requests: fn state ->
            {:ok, [], state}
          end
        )

      refute_receive {:EXIT, ^pid2, :invalid_return}
    end
  end

  describe "@callback parse/2" do
    setup do
      Process.flag(:trap_exit, true)

      :ok
    end

    test "is not called if no `start_urls` and no `start_requests` are provided" do
      TestSpider.start_link(
        parse: fn _response, _state ->
          Kernel.send(:tester, {:called_back, :parse, 2})
        end
      )

      refute_receive {:called_back, :parse, 2}, 100, "@callback parse/2 should not be called"
    end

    test "is called when `start_urls` is provided as option for `start_link/3`", context do
      TestSpider.start_link(
        [
          parse: fn _response, _state ->
            Kernel.send(:tester, {:called_back, :parse, 2})
          end
        ],
        start_urls: context[:start_urls]
      )

      assert_receive {:called_back, :parse, 2}, 100, "@callback parse/2 should be called"
    end

    test "is called with a response body", context do
      TestSpider.start_link(
        [
          parse: fn response, _state ->
            Kernel.send(:tester, {:called_back, :parse, response})
          end
        ],
        start_urls: context[:start_urls]
      )

      assert_receive {:called_back, :parse, response}, 100, "@callback parse/2 should be called"
      assert response.body
    end

    test "is called with the body of the requested URL", context do
      body = File.read!("test/fixtures/index.html")

      TestSpider.start_link(
        [
          parse: fn response, _state ->
            Kernel.send(:tester, {:called_back, :parse, response})
          end
        ],
        start_urls: context[:start_urls]
      )

      assert_receive {:called_back, :parse, response}, 100, "@callback parse/2 should be called"
      assert response.body === body
    end

    test "is called with requested URL in the response", context do
      urls = context[:start_urls]

      TestSpider.start_link(
        [
          parse: fn response, _state ->
            Kernel.send(:tester, {:called_back, :parse, response.url})
          end
        ],
        start_urls: urls
      )

      assert_receive {:called_back, :parse, url}, 100, "@callback parse/2 should be called"
      assert Enum.any?(urls, fn u -> u == url end)
    end

    test "is not called with invalid URL and stop the spider" do
      {:ok, pid} =
        TestSpider.start_link(
          [
            parse: fn _response, _state ->
              Kernel.send(:tester, {:called_back, :parse, 1})
            end
          ],
          start_urls: ["foo://bar"]
        )

      refute_receive {:called_back, :parse, 1}, 100, "@callback parse/2 should not be called"
      assert_receive {:EXIT, ^pid, {:shutdown, :nxdomain}}
    end

    test "is called with each URLs in `start_urls`", context do
      urls = context[:start_urls]

      TestSpider.start_link(
        [
          parse: fn %{url: url}, _state ->
            Kernel.send(:tester, {:called_back, :parse, url})
          end
        ],
        start_urls: urls
      )

      Enum.each(urls, fn url ->
        assert_receive {:called_back, :parse, ^url}, 100
      end)
    end
  end
end
