defmodule PhxElmWeb.PageController do
  use PhxElmWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
