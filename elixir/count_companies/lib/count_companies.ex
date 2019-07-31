defmodule CountCompanies do
  @moduledoc """
  Funny module to count a list of companies on the doomspork/elixir-companies
  website.
  """

  @companies_md "https://raw.githubusercontent.com/doomspork/elixir-companies/master/README.md"

  @doc """
  Count companies listed on the doomsport/elixir-companies

  ## Examples

      iex> CountCompanies.run 
      :45

  """
  def run do
    @companies_md
    |> get_raw_md
    |> extract_companies
  end

  defp get_raw_md(url) do
    %HTTPoison.Response{status_code: 200, body: md} = HTTPoison.get!(url)
    md
  end

  defp extract_companies(markdown) do
    regex = ~r/\* \[(\w+)\]/
    markdown
    |> String.split("\n")
    |> Enum.reduce([], fn comp, acc ->
      case Regex.run(regex, comp, capture: :all) do
        nil -> acc
        [_whole_match, comp_name] -> [comp_name | acc]
      end
    end)
  end

  def markets() do
    @companies_md
    |> get_raw_md
    |> extract_markets
  end

  defp extract_markets(markdown) do
    regex = ~r/#### (\w+)/
    markdown
    |> String.split("\n")
    |> Enum.reduce([], fn comp, acc ->
      case Regex.run(regex, comp, capture: :all) do
        nil -> acc
        [_whole_match, market] -> [market | acc]
      end
    end)
  end
end
