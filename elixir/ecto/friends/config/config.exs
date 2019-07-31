use Mix.Config

config :friends, Friends.Repo,
  adapter: Ecto.Adapters.Postgres,
  database: "friends_repo",
  username: "postgres",
  password: "postgres",
  hostname: "localhost"

config :friends, ecto_repos: [Friends.Repo]

