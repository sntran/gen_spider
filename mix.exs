defmodule GenSpider.MixProject do
  use Mix.Project

  @version "0.1.0"

  def project do
    [
      app: :gen_spider,
      version: @version,
      name: "GenSpider",
      source_url: "https://github.com/sntran/gen_spider",
      homepage_url: "http://sntran.github.io/gen_spider",
      description: """
      A behaviour for defining Spiders that crawl and parse pages
      for a particular site (or, in some cases, a group of sites).
      """,
      elixir: "~> 1.6",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      docs: docs(),
      aliases: aliases(),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {GenSpider.Application, []}
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"},
      {:hackney, "~> 1.13"},
      # Analysis and formatting tools
      {:dialyxir, "~> 1.0.0-rc.3", only: [:dev], runtime: false},
      {:excoveralls, "~> 0.10", only: [:test]},
      # Helpers
      {:git_hooks, "~> 0.2.0", only: [:dev]},
      {:mix_test_watch, "~> 0.8", only: [:dev], runtime: false}
    ]
  end

  defp package do
    [
      maintainers: [
        "Son Tran-Nguyen"
      ],
      licenses: ["Apache 2.0"],
      links: %{github: "https://github.com/sntran/gen_spider"},
      files: ~w(assets lib priv) ++ ~w(CHANGELOG.md LICENSE mix.exs README.md)
    ]
  end

  defp docs do
    [
      main: "GenSpider",
      source_ref: "v#{@version}"
    ]
  end

  defp aliases do
    [
      format: ["format --check-equivalent"],
      test: ["test --cover"]
    ]
  end
end
