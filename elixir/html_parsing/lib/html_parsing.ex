defmodule ElixirHtmlParsing do
  def smoke_test() do
    {:ok, %HTTPoison.Response{body: body}} =
      HTTPoison.get("http://projektkregoslup.pl")
    body |> Floki.find("a") |> Floki.attribute("href")
  end

  def crawl(url), do: crawl(url, [url], %{})

  def crawl(_, [], processed_urls_to_cnt), do: processed_urls_to_cnt
  def crawl(base_url, [url | pending_urls], processed_urls_to_cnt)
  when  binary_part(url, 0, byte_size(base_url)) == base_url do
    if processed_urls_to_cnt[url] do
      crawl(base_url,
        pending_urls,
        Map.update(processed_urls_to_cnt, url, 0, &(&1 + 1)))
    else
        crawl(base_url,
          pending_urls ++ find_links(url),
          Map.put(processed_urls_to_cnt, url, 1))
    end
  end
  def crawl(base_url, [_different_base_url | rest], acc),
  do: crawl(base_url, rest, acc)

  defp find_links(url) do
    %HTTPoison.Response{body: body} = HTTPoison.get!(url)
    body |> Floki.find("a") |> Floki.attribute("href")
  end

end
