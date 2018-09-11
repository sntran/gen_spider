use Mix.Config

config :git_hooks,
  verbose: true,
  hooks: [
    pre_commit: [
      mix_tasks: [
        "format --check-formatted",
        "test"
      ]
    ],
    pre_push: [
      verbose: false,
      mix_tasks: [
        "dialyzer"
      ]
    ]
  ]

config :mix_test_watch,
  clear: true
