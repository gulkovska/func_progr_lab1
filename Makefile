.PHONY: build run init-db seed clean

build:
	cabal build

run:
	cabal run faculty-newspaper

init-db:
	mysql -uroot --default-character-set=utf8mb4 < sql/schema.sql

seed:
	mysql -uroot --default-character-set=utf8mb4 < sql/seed.sql

clean:
	cabal clean
