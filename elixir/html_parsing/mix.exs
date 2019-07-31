defmodule ElixirHtmlParsing.Mixfile do
  use Mix.Project

  def project do
    [app: :elixir_html_parsing,
     version: "0.1.0",
     elixir: "~> 1.3",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  def application do
    [applications: [:logger, :floki, :httpoison]]
  end

  
  defp deps do
    [{:floki, "~> 0.11"},
     {:httpoison, "~> 0.10.0"}]
  end
end
