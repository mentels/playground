defmodule DockerHubClient do
  use HTTPoison.Base

  @standard_headers [{"Accept", "application/json"},
                     {"Content-Type", "application/json"}]
  @user "xxx"
  @password "xxx"

  ## Callbacks

  def process_request_headers(headers) do
    case Enum.find(headers,
          &(elem(&1, 0) == "Authorization" || elem(&1, 0) == "Token")) do
      nil ->
        [basic_auth_header(@user, @password) | headers]
      {"Token", token} ->
        [bearer_header(token) | Enum.reject(headers, &(&1 == "Token"))]
    end ++ @standard_headers
  end

  def process_response_body(body) do
    {:ok, body} = Poison.decode(body)
    body
  end

  def process_headers(headers) do
    is_www_auth_h? = &(elem(&1, 0) == "Www-Authenticate")
    case Enum.find(headers, is_www_auth_h?) do
      nil ->
        headers
      {_, header_val} ->
        www_auth_params = parse_www_authenticate_header(header_val)
        [{"Www-Authenticate", www_auth_params}
         | Enum.reject(headers, is_www_auth_h?)]
    end
  end

  ## Helpers

  defp basic_auth_header(user, password) do
    credentials_endcoded = Base.encode64("#{user}:#{password}")
    {"Authorization", "Basic #{credentials_endcoded}"}
  end

  defp bearer_header(token), do: {"Authorization", "Bearer " <> token}

  defp parse_www_authenticate_header(header) do
    regex = ~r{(realm|service|scope)="(.*?)"}
    regex
    |> Regex.scan(header, capture: :all_but_first)
    |> Enum.map(fn [field, value] -> {field, value} end)
    |> Enum.into(%{})
  end

end

defmodule HTTPoisonBaseExample do
  @standard_headers [{"Accept", "application/json"},
                     {"Content-Type", "application/json"}]
  @user "mentels"
  @password "xxx"

  @mim_docker_repo "mongooseim/mongooseim"
  @endpoint "https://registry-1.docker.io/v2"
  @list_tags_path "tags/list"

  defmodule ImageType do
    defstruct [:tag]
  end

  def images(), do: get_images(tags_endpoint_url(), [])

  def get_images(url, headers) do
    case DockerHubClient.get(url, headers) do
      {:ok, %HTTPoison.Response{status_code: 401, headers: headers}} ->
        token = get_token!(headers)
        get_images(url, [{"Token", token}])
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        build_images(body["tags"])
    end
  end

  ### Helpers

  defp get_token!(headers) do
    auth_header = Enum.find(headers, &(elem(&1, 0) == "Www-Authenticate"))
    auth_header == nil && raise "Cannot authenticate to DockerHub"
    {:ok, resp} =
      auth_header
      |> elem(1)
      |> api_challenge_url()
      |> DockerHubClient.get()
    resp.body["token"]
  end

  defp tags_endpoint_url() do
    [@endpoint, @mim_docker_repo, @list_tags_path]
    |> Path.join()
    |> URI.parse()
    |> to_string()
  end

  defp build_images(tags), do: Enum.map(tags, &(%ImageType{tag: &1}))

  defp api_challenge_url(params) do
    query = URI.encode_query(service: params["service"], scope: params["scope"])
    params["realm"]
    |> URI.parse()
    |> Map.put(:query, query)
    |> to_string()
  end

end
