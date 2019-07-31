.PHONY: postgres postgres_cli

db_container = ecto

postgres:
	docker start $(db_container) || dhttps://8thlight.com/blog/damon-kelley/2016/12/07/designing-for-pipeline-transformations.htmlocker run --name $(db_container) -e POSTGRES_PASSWORD=postgres -e POSTGRES_USER=postgres -p 5432:5432 -d postgres

postgres_cli:
	docker run -it --rm --link ($db_container):postgres postgres psql -h postgres -U postgres
