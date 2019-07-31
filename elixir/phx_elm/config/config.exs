# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :phx_elm,
  ecto_repos: [PhxElm.Repo]

# Configures the endpoint
config :phx_elm, PhxElmWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "BH3Ecgc28ehcouOo0LTJZi/HnSXXM4rCFiAXnfsyjAdadXC1wKaxKOp3FHj0OYFe",
  render_errors: [view: PhxElmWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: PhxElm.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:user_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
