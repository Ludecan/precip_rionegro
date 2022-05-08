SHELL := /bin/bash

# This must be called:
# - `make docker-run` (defaults to current date)
# - `make docker-run "run_args=dt_fin=2022-03-01"` (single date)
# - `make docker-run "run_args=dt_fin=2022-03-01\;dt_ini=2022-03-31"` (date range)
docker-run $(run_args):
	docker run -u `stat -c "%u:%g" $(PWD)/datos` -v $(PWD)/datos:/datos -v $(PWD)/Resultados:/Resultados -v $(HOME)/.netrc:/root/.netrc:ro --env-file .env precip_rionegro $(run_args)

docker-build:
	docker build -t precip_rionegro .
